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

   Append-only: nothing in this namespace ever rewrites the file. See
   `tail-lines` for why.

   Under XDG_CACHE_HOME rather than TMPDIR, deliberately: TMPDIR measured at
   three distinct values on this machine, and the git hook runs outside any
   session, so a temp path is a place the writer and the reader would never
   meet."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]))

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

(def ^:private max-read-bytes
  "How much of the tail to read. Measured: 140 bytes a line, ~68 pushes a day,
   so 4MB is roughly a year of history and far more than any `since-ms` window
   asks for."
  (* 4 1024 1024))

(defn- tail-lines
  "The file's last `max-read-bytes`, split into whole lines.

   Reading the tail rather than the whole file is what lets this log have NO
   truncating writer. There used to be a `prune!` that rewrote it in place with
   `spit`, under a flock the POSIX `pre-push` hook's bare `>>` never takes:
   measured, 156 of 400 concurrent appends destroyed, and every lost record is
   a push that can no longer match a reflog entry and is silently never
   reviewed. An atomic rename would not have fixed it either — an append
   between the read and the rename lands on the inode the rename replaces.

   So the writer is append-only, forever, and the reader bounds its own work.
   `records-since` already filters by timestamp, so nothing needed the file to
   be short."
  [log]
  (let [size (fs/size log)]
    (if (<= size max-read-bytes)
      (str/split-lines (slurp log))
      (with-open [raf (java.io.RandomAccessFile. (str log) "r")]
        (.seek raf (- size max-read-bytes))
        (let [buf (byte-array max-read-bytes)
              n (.read raf buf)
              text (String. buf 0 (max n 0) "UTF-8")]
          ;; the first line is probably half a record
          (rest (str/split-lines text)))))))

(defn- records-since [log since-ms]
  (when (fs/exists? log)
    (->> (tail-lines log)
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

(defn pusher
  "The Claude session that pushed `sha` to `branch` in `git-dir`, or nil.

   Makes a handoff mechanical rather than a search. A wake is delivered to
   whichever session's tool call triggered the review, which for a retry is
   deliberately not the session that made the push — and a wake is dropped
   entirely if that session's turn has already ended. Either way someone has
   `since-ms` 0 deliberately. The caller is the retry path, whose whole point
   is that it is not time-scoped — a killed review is retried however old its
   push is — and `tail-lines` already bounds how much file that reads."
  [log git-dir branch sha]
  (->> (attempts-since log 0)
       (filter #(and (= git-dir (:git-dir %))
                     (= branch (:branch %))
                     (= sha (:sha %))
                     (by-agent? %)))
       first
       :session))
