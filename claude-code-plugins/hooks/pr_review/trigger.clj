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
            [pr-review.pushrecord :as pushrecord]
            [pr-review.reviewer :as reviewer]
            [pr-review.workdir :as workdir]))

(def ^:private context-keep 5)

(def ^:private record-max-age-ms
  "How long a push record stays interesting. A PostToolUse hook fires when the
   Bash call returns, so the record it wants is seconds old; an hour is slack,
   not a window."
  (* 60 60 1000))

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
  (or ((or (:git-dir-fn opts) gh/git-common-dir) repo-root opts)
      (str repo-root "/.git")))

(defn- recorded-dir
  "The directory the pushing shell reported for this tool call, or nil.

   Accepted only when it is still a directory inside a git repository. A
   record can name a worktree that has since been removed, and handing
   `decide` a directory nothing could have pushed from would make the
   authoritative source a worse answer than the parser it outranks — so the
   same admission test `pr-review.workdir` applies to its own guesses applies
   here too, and a rejected record falls through to parsing."
  [tool-use-id]
  (let [pwd (:pwd (pushrecord/read-record tool-use-id))]
    (when (and pwd (workdir/usable-dir? pwd)) pwd)))

(defn- effective-dir
  "Which directory the push ran in, and which input said so. Highest first:

     :push-record — `pr-review.pushrecord`: a `PreToolUse` recorder made the
                    shell itself write its `$PWD`, keyed by this call's
                    `tool_use_id`, which both payloads carry. Nothing is
                    inferred, so it outranks everything below
     :command      — `pr-review.workdir`'s reading of `tool_input.command`
     :session-cwd  — the payload's `cwd`, which is the SESSION's directory

   `:command` is claimed only when parsing actually moved the directory: a
   command that says nothing about it resolves to the payload cwd, and then
   cwd is as much the source as the parse was."
  [{:keys [cwd tool_input tool_use_id]}]
  (if-let [d (recorded-dir tool_use_id)]
    {:dir d :source :push-record}
    (let [{:keys [dir]} (workdir/resolve-dir (:command tool_input) cwd)]
      {:dir dir :source (if (= dir cwd) :session-cwd :command)})))

(defn- decide-in
  "The decision proper, for an already-resolved directory."
  [dir opts]
  (let [repo-root ((or (:repo-root-fn opts) gh/repo-root) dir opts)]
    (if-not repo-root
      {:action :silent :reason (str "not a git repo: " dir)}
      (let [branch ((or (:branch-fn opts) gh/current-branch) repo-root opts)]
        (if-not branch
          {:action :silent :reason "detached HEAD, no branch to match a PR"}
          (let [pr ((or (:open-pr-fn opts) gh/open-pr) repo-root branch opts)]
            (if-not pr
              {:action :silent :reason (str "no open PR for branch " branch)}
              (let [pr-num  (:number pr)
                    sha     ((or (:head-sha-fn opts) gh/head-sha) repo-root opts)
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

(defn decide
  "Pure decision from the hook input. No side effects, no spawning.

   Reads the ledger exactly once and hands the result to ledger's pure
   predicates.

   The directory is never the payload's `cwd` alone: `cwd` is the *session's*
   directory, and an agent that `cd`s into a worktree and pushes from there is
   on another branch of another checkout. Measured: `cwd` on
   `docs/mydeck-design` with no open PR, the worktree the push ran in on a
   branch with open PR #391. See `effective-dir` for the three sources and
   their order.

   `:dir-source` rides on EVERY decision, the silent ones included. Which
   input won is the first thing to ask when a review lands on the wrong
   repository — or on none — and a key present only on `:review` would be
   missing exactly where it is needed."
  [input opts]
  (let [{:keys [dir source]} (effective-dir input)]
    (assoc (decide-in dir opts) :dir-source source)))

(defn findings-message
  "The text agent A will see. The harness prefixes it with a fixed, unhelpful
   wrapper and ignores rewakeMessage for third-party plugins, so this string
   has to introduce itself."
  ([d parsed] (findings-message d parsed nil))
  ([{:keys [repo-root pr pass]} parsed warnings]
   (str "pr-review-loop — " (fs/file-name repo-root)
        " PR #" pr ", pass " pass ": " (:verdict parsed) "\n\n"
        (:body parsed)
        (when (seq warnings)
          (str "\n\n" (str/join "\n" warnings)))
        "\n\nNext: use the pr-review-loop skill. Verify each"
        " [correctness/blocking] finding against the source before fixing it.")))

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

(defn- failed-review-message
  "A review that produced no findings still has to wake agent A — the real
   diagnosis is sitting unread in the reviewer's stderr — but it must say
   plainly that no slot was spent, because none was."
  [{:keys [repo-root pr pass]} parsed res]
  (str "pr-review-loop — " (fs/file-name repo-root)
       " PR #" pr ", pass " pass ": review did not complete ("
       (:verdict parsed) ")"
       "\n\nreviewer process exited " (:exit res) ": " (:err res)
       (when-not (str/blank? (str (:body parsed)))
         (str "\n\n" (:body parsed)))
       "\n\nNo ledger row was written, so this attempt did not consume one of"
       " the " ledger/max-passes " review slots for PR #" pr
       ". Fix the cause and push again."))

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
            ;; reconcile is mergeable? wired in: a count block that
            ;; contradicts the verdict line loses, here, once, so both the
            ;; headline and the ledger row carry the same reconciled verdict.
            parsed (reviewer/reconcile (reviewer/parse-output (:out res)))]
        (cond
          ;; A newer push superseded this trigger and killed its reviewer
          ;; mid-answer. Recording that truncated output would spend a slot
          ;; and wake agent A with findings for a SHA that is already stale.
          (lock/superseded? git-dir opts)
          {:exit 0 :message nil}

          ;; A crashed or unparsed reviewer produced no findings, so it does
          ;; not consume a cap slot — the same ruling the unresolved-base-ref
          ;; path already makes. Six pushes against an expired token used to
          ;; write six MALFORMED rows and, with four real passes, exhaust the
          ;; PR's budget permanently.
          (or (not (zero? (:exit res))) (= "MALFORMED" (:verdict parsed)))
          {:exit 2 :message (failed-review-message d parsed res)}

          :else
          (do (ledger/append-pass!
               git-dir
               {:pr pr :sha sha :pass pass
                :verdict (:verdict parsed)
                :blocking (get (:counts parsed) "correctness/blocking" 0)
                :followup (get (:counts parsed) "correctness/followup" 0)
                :coverage (get (:counts parsed) "coverage" 0)
                :fingerprints (:fingerprints parsed)})
              (context/prune! git-dir context-keep)
              {:exit 2 :message (findings-message
                                 d parsed (reviewer/parse-warnings parsed))}))))))

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

(defn- respond
  "The exit code and wake message a decision earns.

   Split out of -main so the exit-code contract is testable without
   System/exit: an action this `case` never learned falls through to the
   default and is silently inert — which is precisely the defect class this
   namespace keeps paying for."
  [d opts]
  (case (:action d)
    :silent      {:exit 0 :message nil}
    :cap-reached {:exit 2 :message (str "pr-review-loop — " (:reason d)
                                        ". No further reviews will run"
                                        " on this PR. Decide manually.")}
    :review      (review! d opts)
    {:exit 0 :message nil}))

(defn -main
  [& _]
  (let [input (try (json/parse-string (slurp *in*) true)
                   (catch Exception _ nil))
        d     (decide (or input {}) {})
        {:keys [exit message]} (respond d {})]
    (when message
      (binding [*out* *err*] (println message) (flush)))
    ;; Housekeeping, last of all: the wake message is already on stderr and
    ;; the exit code is already decided, so nothing prune-records! does — or
    ;; fails to do — can reach the review. It sits outside every branch
    ;; `decide` can take, so it runs on all of them, and it is the only thing
    ;; that ever deletes these files.
    (pushrecord/prune-records! record-max-age-ms)
    (System/exit exit)))
