(ns pr-review.trigger
  "PostToolUse hook entrypoint.

   Exit 0 means silence. Exit 2 wakes agent A with whatever this process wrote
   to stderr — and only stderr: the harness discards a hook's stdout even when
   stderr is empty. Never exit anything else; other codes produce a
   `Failed with non-blocking status code:` notice and the pass is lost."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [pr-review.context :as context]
            [pr-review.gh :as gh]
            [pr-review.ledger :as ledger]
            [pr-review.lock :as lock]
            [pr-review.prompt :as prompt]
            [pr-review.reviewer :as reviewer]))

(def ^:private context-keep 5)

(defn core-prompt
  "The generic review core, read off the classpath.

   `hooks/` is a :paths root in bb.edn, so `review_core.md` resolves as a
   resource regardless of the working directory and without CLAUDE_PLUGIN_ROOT —
   which is set only for hooks declared in a plugin's hooks/hooks.json, and is
   therefore nil when the /review command invokes this same entrypoint."
  []
  (if-let [r (io/resource "review_core.md")]
    (slurp r)
    (throw (ex-info "review_core.md not on the classpath" {}))))

(defn decide
  "Pure decision from the hook input. No side effects, no spawning."
  [{:keys [cwd]} opts]
  (let [repo-root ((or (:repo-root-fn opts) #(gh/repo-root cwd opts)))]
    (if-not repo-root
      {:action :silent :reason "not a git repo"}
      (let [branch ((or (:branch-fn opts) #(gh/current-branch repo-root opts)))]
        (if-not branch
          {:action :silent :reason "detached HEAD, no branch to match a PR"}
          (let [pr ((or (:open-pr-fn opts) #(gh/open-pr repo-root branch opts)))]
            (if-not pr
              {:action :silent :reason (str "no open PR for branch " branch)}
              (let [pr-num (:number pr)
                    sha ((or (:head-sha-fn opts) #(gh/head-sha repo-root opts)))]
                (if (ledger/cap-reached? repo-root pr-num)
                  {:action :cap-reached
                   :repo-root repo-root :pr pr-num
                   :reason (str "review cap of " ledger/max-passes
                                " passes reached for PR #" pr-num)}
                  {:action :review
                   :repo-root repo-root
                   :pr pr-num
                   :pass (ledger/next-pass-number repo-root pr-num)
                   :sha sha
                   :base-ref (:baseRefName pr)
                   :draft? (boolean (:isDraft pr))
                   :prior-fingerprints
                   (->> (ledger/read-passes repo-root pr-num)
                        (mapcat :fingerprints)
                        distinct
                        (filterv #(>= (ledger/raise-count repo-root pr-num %) 2)))})))))))))

(defn findings-message
  "The text agent A will see. The harness prefixes it with a fixed, unhelpful
   wrapper and ignores rewakeMessage for third-party plugins, so this string
   has to introduce itself."
  [{:keys [repo-root pr pass]} parsed]
  (str "pr-review-loop — " (fs/file-name repo-root)
       " PR #" pr ", pass " pass ": " (:verdict parsed) "\n\n"
       (:body parsed)
       "\n\nNext: use the pr-review-loop skill. Verify each"
       " [correctness/blocking] finding against the source before fixing it."))

(defn- review!
  [{:keys [repo-root pr pass sha base-ref draft? prior-fingerprints] :as d} opts]
  (case (:status (lock/acquire! repo-root {:pr pr :sha sha} opts))
    :duplicate {:exit 0 :message nil}
    (try
      (let [ctx    (context/build! repo-root {:pr pr :sha sha :base-ref base-ref} opts)
            core   (core-prompt)
            text   (prompt/build {:core core :repo-root repo-root :ctx ctx
                                  :pr pr :pass pass :draft? draft?
                                  :prior-fingerprints prior-fingerprints})
            res    (reviewer/run! text repo-root opts)
            parsed (reviewer/parse-output (:out res))
            ;; A non-zero reviewer exit or an unparsed MALFORMED verdict means
            ;; the real diagnosis is sitting unread in :err — surface it in
            ;; the wake message, or the session sees only the bare word
            ;; MALFORMED and never learns why the reviewer never ran cleanly.
            parsed (if (or (= "MALFORMED" (:verdict parsed)) (not (zero? (:exit res))))
                     (update parsed :body str
                             "\n\nreviewer process exited " (:exit res) ": " (:err res))
                     parsed)]
        (ledger/append-pass!
         repo-root
         {:pr pr :sha sha :pass pass
          :verdict (:verdict parsed)
          :blocking (get (:counts parsed) "correctness/blocking" 0)
          :followup (get (:counts parsed) "correctness/followup" 0)
          :coverage (get (:counts parsed) "coverage" 0)
          :fingerprints (:fingerprints parsed)})
        (context/prune! repo-root context-keep)
        {:exit 2 :message (findings-message d parsed)})
      ;; Any exception here (context/build!, core-prompt, ledger/append-pass!
      ;; and context/prune! are all uncaught otherwise) must not propagate:
      ;; -main has no try of its own around review!, and an exit code other
      ;; than 0 or 2 is a silently lost pass, not a loud failure. Converting
      ;; it to exit 2 wakes the session with the diagnosis instead.
      (catch Exception e
        {:exit 2 :message (str "pr-review-loop — " (fs/file-name repo-root)
                               " PR #" pr ", pass " pass
                               " crashed: " (ex-message e))})
      ;; Same `opts` acquire! was called with, not just repo-root: release!
      ;; now only deletes the record if its :pid still matches, and a test
      ;; that stubs :pid in opts to acquire! must have that same stub honoured
      ;; on release! or the two would disagree about who holds the lock.
      (finally (lock/release! repo-root opts)))))

(defn -main
  [& _]
  (let [input (try (json/parse-string (slurp *in*) true)
                   (catch Exception _ nil))
        d     (decide (or input {}) {})]
    (let [{:keys [exit message]}
          (case (:action d)
            :silent      {:exit 0 :message nil}
            :cap-reached {:exit 2 :message (str "pr-review-loop — " (:reason d)
                                                ". No further reviews will run"
                                                " on this PR. Decide manually.")}
            :review      (review! d {})
            {:exit 0 :message nil})]
      (when message
        (binding [*out* *err*] (println message) (flush)))
      (System/exit exit))))
