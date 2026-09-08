(ns pr-review.trigger
  "PostToolUse hook entrypoint.

   Exit 0 means silence. Exit 2 wakes agent A with whatever this process wrote
   to stderr — and only stderr: the harness discards a hook's stdout even when
   stderr is empty. Never exit anything else; other codes produce a
   `Failed with non-blocking status code:` notice and the pass is lost.

   That contract is why every filesystem-touching call below sits inside
   `review!`'s try, `lock/acquire!` included. A worktree used to make
   `acquire!` throw from the `case` head — outside the try — and babashka
   exited 1 on every push."
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

(defn- resolve-git-dir
  "The clone's shared git directory, or `<repo-root>/.git` when git cannot
   say. Resolved once per decision and threaded into every state module: all
   of them used to assume `<repo-root>/.git`, which is a *file* in a linked
   worktree."
  [repo-root opts]
  (or ((or (:git-dir-fn opts) #(gh/git-common-dir repo-root opts)))
      (str repo-root "/.git")))

(defn decide
  "Pure decision from the hook input. No side effects, no spawning.

   Reads the ledger exactly once and hands the result to ledger's pure
   predicates."
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
              (let [pr-num  (:number pr)
                    sha     ((or (:head-sha-fn opts) #(gh/head-sha repo-root opts)))
                    git-dir (resolve-git-dir repo-root opts)
                    passes  (ledger/read-passes git-dir pr-num)]
                (cond
                  ;; Idempotence, before the cap: repeat pushes of one commit
                  ;; are ordinary (`git push` twice, `--tags`, `--dry-run`,
                  ;; `--delete` all match `Bash(git push:*)`), and re-reviewing
                  ;; an already-reviewed SHA tells agent A nothing new while
                  ;; spending a cap slot for it.
                  (ledger/reviewed-sha? passes sha)
                  {:action :silent
                   :reason (str "PR #" pr-num " already has a recorded pass at "
                                sha)}

                  (ledger/cap-reached? passes)
                  {:action :cap-reached
                   :repo-root repo-root :git-dir git-dir :pr pr-num
                   :reason (str "review cap of " ledger/max-passes
                                " passes reached for PR #" pr-num)}

                  :else
                  {:action :review
                   :repo-root repo-root
                   :git-dir git-dir
                   :pr pr-num
                   :pass (ledger/next-pass-number passes)
                   :sha sha
                   :base-ref (:baseRefName pr)
                   :draft? (boolean (:isDraft pr))
                   :prior-fingerprints (ledger/suppressed-fingerprints passes)})))))))))

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

(defn- unresolved-base-message
  "Names the unresolved ref and the exact fix, so agent A does not have to
   guess why a branch with a real diff came back with nothing to say."
  [{:keys [repo-root pr pass base-ref]}]
  (str "pr-review-loop — " (fs/file-name repo-root)
       " PR #" pr ", pass " pass
       ": could not diff against base ref \"" base-ref "\" — this clone likely"
       " never fetched it, so the diff command itself failed rather than"
       " finding no changes. Fix: `git fetch origin " base-ref "`, then push"
       " again."))

(defn- crash-message
  [{:keys [repo-root pr pass]} e]
  (str "pr-review-loop — " (fs/file-name repo-root)
       " PR #" pr ", pass " pass " crashed: " (ex-message e)))

(defn- release-quietly!
  "release! reaches the filesystem — flock/with-file-lock creates its guard
   file and parent — so it can throw. A throw out of a `finally` replaces the
   value the body already computed, so an unwrapped release! is a second
   route to losing a completed review, and (before `review!` had an outer
   try) to a non-2 exit. A failed release is self-healing anyway: the record
   names a pid, and acquire! treats a dead holder's lock as free."
  [git-dir opts]
  (try (lock/release! git-dir opts) (catch Exception _ nil)))

(defn- run-review!
  "The reviewer pass proper, with the lock already held."
  [{:keys [repo-root git-dir pr pass sha base-ref draft? prior-fingerprints] :as d}
   opts]
  (let [ctx (context/build! repo-root git-dir
                            {:pr pr :sha sha :base-ref base-ref} opts)]
    (if (:diff-failed? ctx)
      ;; The diff command itself failed — almost always an unresolved base
      ;; ref. A 0-byte diff here looks exactly like a real empty one, so
      ;; spawning the reviewer would have it correctly report "nothing to
      ;; review" and the loop would record a false MERGEABLE. Refuse to
      ;; review, and refuse to spend a ledger slot on a pass that reviewed
      ;; nothing — the PR would still owe a real review even after the cap.
      {:exit 2 :message (unresolved-base-message d)}
      (let [text   (prompt/build {:core (core-prompt)
                                  :repo-root repo-root :git-dir git-dir
                                  :ctx ctx :pr pr :pass pass :draft? draft?
                                  :prior-fingerprints prior-fingerprints})
            res    (reviewer/run! text repo-root opts)
            parsed (reviewer/parse-output (:out res))]
        ;; A non-zero reviewer exit or an unparsed MALFORMED verdict means the
        ;; real diagnosis is sitting unread in :err — surface it in the wake
        ;; message, or the session sees only the bare word MALFORMED and never
        ;; learns why the reviewer never ran cleanly.
        (let [parsed (if (or (= "MALFORMED" (:verdict parsed)) (not (zero? (:exit res))))
                       (update parsed :body str
                               "\n\nreviewer process exited " (:exit res) ": " (:err res))
                       parsed)]
          (do (ledger/append-pass!
               git-dir
               {:pr pr :sha sha :pass pass
                :verdict (:verdict parsed)
                :blocking (get (:counts parsed) "correctness/blocking" 0)
                :followup (get (:counts parsed) "correctness/followup" 0)
                :coverage (get (:counts parsed) "coverage" 0)
                :fingerprints (:fingerprints parsed)})
              (context/prune! git-dir context-keep)
              {:exit 2 :message (findings-message d parsed)}))))))

(defn- review!
  "Always returns an :exit of 0 or 2. Nothing inside — acquire!, the context
   build, the reviewer, the ledger write, the release — may propagate, because
   -main has no try of its own and any other exit code is a silently lost
   pass rather than a loud failure."
  [{:keys [git-dir pr sha] :as d} opts]
  (try
    (if (= :duplicate (:status (lock/acquire! git-dir {:pr pr :sha sha} opts)))
      {:exit 0 :message nil}
      (try
        (run-review! d opts)
        ;; Same `opts` acquire! was called with, not just git-dir: release!
        ;; only deletes the record if its :pid still matches, and a test that
        ;; stubs :pid in opts to acquire! must have that same stub honoured on
        ;; release! or the two would disagree about who holds the lock.
        (finally (release-quietly! git-dir opts))))
    (catch Exception e
      {:exit 2 :message (crash-message d e)})))

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
