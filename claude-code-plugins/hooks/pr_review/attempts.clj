(ns pr-review.attempts
  "Push attempts, with the provenance the reflog cannot carry.

   Two things git's reflog cannot say, and this file says both:

   WHICH CLONE. The PostToolUse hook has no working directory of its own —
   only the SESSION cwd, which two days of defects proved is not where the
   agent pushed from — so something has to name the clone whose reflog to
   read.

   WHO PUSHED. Git records a push whoever made it, so the reflog alone cannot
   tell an agent's push from the user's own from a terminal. Claude Code
   exports CLAUDE_CODE_SESSION_ID into a Bash tool call's environment and
   every child inherits it — measured identical to the PostToolUse payload's
   `session_id` — so a push carries the session that made it, or `-` for a
   human one. That is R2 decided by what actually made the push rather than by
   how the command was spelled, which is what lets `git -C /x push`, aliases
   and scripts through a gate no command-text match can see.

   These lines are ATTEMPTS, not outcomes: a `pre-push` hook runs before the
   push and cannot know it succeeded. The reflog stays the proof that a push
   landed; these say only who tried, and at which sha.

   Two record shapes share the file, told apart by field count:

     <ts>\\t<git-dir>\\t<session>                     a clone was pushed
     <ts>\\t<git-dir>\\t<session>\\t<ref>\\t<sha>        one ref of that push

   Under XDG_CACHE_HOME rather than TMPDIR, deliberately: TMPDIR measured at
   three distinct values on this machine, and the git hook runs outside any
   session, so a temp path is a place the writer and the reader would never
   meet."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [pr-review.flock :as flock]))

(def ^:private max-age-ms
  "How long a record stays interesting. Long enough to span the gap between
   pushing a branch and opening its PR — the `gh pr create` path reads these —
   and short enough that the log stays small."
  (* 24 60 60 1000))

(def no-session
  "What the hook writes when CLAUDE_CODE_SESSION_ID is unset: a human push."
  "-")

(defn default-log
  "The path the `pre-push` hook appends to. Kept in one place because the hook
   computes the same path in POSIX sh and the two must not drift."
  []
  (str (fs/path (or (System/getenv "XDG_CACHE_HOME")
                    (str (fs/path (System/getProperty "user.home") ".cache")))
                "pr-review-loop" "pushes.log")))

(defn- parse-line
  "One record, or nil. Seconds on the wire because POSIX `date +%s` has no
   milliseconds; milliseconds in here because the ledger and the reflog reader
   both use them.

   A 3-field line is tolerated as well as a 5-field one: the hook is a POSIX
   sh script that must never fail a push, so it cannot validate what it
   appends, and a version of it that wrote fewer fields may still be installed
   in a clone this session has not started in."
  [line]
  (let [[ts dir session ref sha] (str/split (str/trimr line) #"\t")]
    (when (and ts dir (seq dir) (re-matches #"\d+" ts))
      (cond-> {:ts (* 1000 (parse-long ts))
               :git-dir dir
               :session (if (seq (str session)) session no-session)}
        (and ref (seq ref) sha (re-matches #"[0-9a-f]{40}" (str sha)))
        (assoc :ref ref
               :sha sha
               :branch (str/replace-first ref #"^refs/heads/" ""))))))

(defn- records-since [log since-ms]
  (when (fs/exists? log)
    (->> (str/split-lines (slurp log))
         (keep parse-line)
         (filter #(>= (:ts %) since-ms))
         (sort-by :ts >))))

(defn clones-since
  "Distinct git directories touched at or after `since-ms`, newest first.

   A directory that no longer exists is dropped: a clone can be deleted, and a
   pointer to nothing would only cost the caller a failed git invocation."
  [log since-ms]
  (->> (records-since log since-ms)
       (map :git-dir)
       distinct
       (filter fs/directory?)))

(defn attempts-since
  "Ref-level attempts at or after `since-ms`, newest first — only the records
   that name a ref and a sha."
  [log since-ms]
  (filter :ref (records-since log since-ms)))

(defn by-agent?
  "True when this attempt was made from inside a Claude Code session."
  [attempt]
  (not= no-session (:session attempt)))

(defn prune!
  "Drops records older than `max-age-ms` from `log`, under its own flock.

   Called from the trigger rather than the hook: the hook must stay incapable
   of failing a push, and rewriting a file it holds open is exactly the kind of
   step that could."
  ([log] (prune! log max-age-ms))
  ([log age-ms]
   (when (fs/exists? log)
     (flock/with-file-lock
       (flock/guard-path log)
       (fn []
         (let [cutoff (- (System/currentTimeMillis) age-ms)
               kept (->> (str/split-lines (slurp log))
                         (filter #(when-let [{:keys [ts]} (parse-line %)]
                                    (>= ts cutoff))))]
           (spit log (if (seq kept) (str (str/join "\n" kept) "\n") ""))
           (count kept)))))))
