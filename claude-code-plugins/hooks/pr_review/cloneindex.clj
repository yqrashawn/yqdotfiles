(ns pr-review.cloneindex
  "Which clones were pushed recently — the one fact `pr-review.pushlog` cannot
   supply on its own.

   The reflog says everything about a push except where to find it: the
   PostToolUse hook has no working directory of its own (only the SESSION cwd,
   which two days of defects proved is not where the agent pushed from), so
   something has to name the clone. The `pre-push` hook appends one line here
   per push:

     1788929441\\t/Users/x/workspace/repo/.git

   A path under XDG_CACHE_HOME rather than TMPDIR, deliberately. TMPDIR
   differs per session — measured at three distinct values on this machine —
   and the git hook runs outside any session, so a temp path is a place the
   writer and the reader would never meet.

   Clones, not repositories: a push from a different clone of the same repo
   writes that clone's git dir, and its reflog is the only one that has the
   entry."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [pr-review.flock :as flock]))

(def ^:private max-age-ms
  "How long a clone pointer stays interesting. Long enough to span the gap
   between pushing a branch and opening its PR — the `gh pr create` path reads
   this — and short enough that the log stays small."
  (* 24 60 60 1000))

(defn default-log
  "The path the `pre-push` hook appends to. Kept in one place because the hook
   computes the same path in POSIX sh and the two must not drift."
  []
  (str (fs/path (or (System/getenv "XDG_CACHE_HOME")
                    (str (fs/path (System/getProperty "user.home") ".cache")))
                "pr-review-loop" "pushes.log")))

(defn- parse-line
  "`<epoch-seconds>\\t<git-common-dir>` as {:ts ms :git-dir path}, or nil.

   Seconds on the wire because POSIX `date +%s` has no milliseconds;
   milliseconds in here because the ledger and the reflog reader both use them."
  [line]
  (let [[ts dir] (str/split (str/trimr line) #"\t" 2)]
    (when (and ts dir (seq dir) (re-matches #"\d+" ts))
      {:ts (* 1000 (parse-long ts)) :git-dir dir})))

(defn clones-since
  "Distinct git directories pushed at or after `since-ms`, newest push first.

   A directory that no longer exists is dropped: a clone can be deleted, and a
   pointer to nothing would only cost the caller a failed git invocation."
  [log since-ms]
  (when (fs/exists? log)
    (->> (str/split-lines (slurp log))
         (keep parse-line)
         (filter #(>= (:ts %) since-ms))
         (sort-by :ts >)
         (map :git-dir)
         distinct
         (filter fs/directory?))))

(defn prune!
  "Drops entries older than `max-age-ms` from `log`, under its own flock.

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
