(ns pr-review.lock
  "At most one live reviewer per clone, always on the newest pushed SHA.

   Two rapid pushes must not leave a reviewer grinding on a SHA that is
   already stale — the newer push kills the older reviewer and takes over.

   `git-dir` throughout is the clone's shared git directory (see
   pr-review.gh/git-common-dir), so every worktree of one repository
   contends for the same lock. A worktree-local lock would let one push per
   worktree run a reviewer concurrently, which is the thing this namespace
   exists to prevent.

   Nothing here may make the trigger exit anything but 0 or 2: any other code
   makes Claude Code print `Failed with non-blocking status code:` and the
   pass is silently lost. That is why the kill targets the superseded
   reviewer rather than the superseded trigger — a SIGTERMed babashka exits
   143."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [pr-review.flock :as flock]))

(defn lock-path
  [git-dir]
  (str git-dir "/pr-review.lock"))

(defn read-lock
  "Current lock record, or nil when absent, unparseable, or missing a
   usable :pid. A corrupt or incomplete lock reads as free: a half-written
   file must not wedge the loop forever."
  [git-dir]
  (let [p (lock-path git-dir)]
    (when (fs/exists? p)
      (let [parsed (try (json/parse-string (slurp p) true)
                        (catch Exception _ nil))]
        (when (:pid parsed)
          parsed)))))

(defn alive?
  [pid]
  (let [h (java.lang.ProcessHandle/of (long pid))]
    (and (.isPresent h) (.isAlive (.get h)))))

(defn kill-reviewers!
  "SIGTERM the whole process subtree *below* `pid`, and not `pid` itself.

   `pid` is the recorded trigger, and the reviewer is its `claude -p` child.
   Killing the trigger — what this used to do — left that child running to
   completion, so a supersede produced two concurrent reviewers for the two
   to six minutes a review takes: R14 unmet. It also made the losing trigger
   exit 143, a third exit code this module's contract forbids by name.

   Killing downward instead fixes both. The reviewer dies, the loser stays
   alive to reach its own `System/exit 0`, and it learns it lost by finding
   the lock record no longer names it (see `superseded?`).

   The descendant set is snapshotted before any destroy so a dying
   intermediate process cannot orphan a grandchild out of the walk."
  [pid]
  (try
    (let [opt (java.lang.ProcessHandle/of (long pid))]
      (when (.isPresent opt)
        (let [kids (vec (iterator-seq (.iterator (.descendants (.get opt)))))]
          (doseq [k kids] (.destroy k))
          (count kids))))
    (catch Exception _ nil)))

(defn- write-lock!
  [git-dir {:keys [pid pr sha]}]
  (fs/create-dirs (fs/parent (lock-path git-dir)))
  (spit (lock-path git-dir)
        (json/generate-string {:pid pid :pr pr :sha sha
                               :started (System/currentTimeMillis)})))

(defn acquire!
  "Take the reviewer lock for (`pr`, `sha`).

   :duplicate  — a live reviewer already holds this exact SHA. Caller exits 0.
   :superseded — a live reviewer held an older SHA; its reviewer was killed.
   :acquired   — the lock was free, corrupt, or held by a dead process.

   The whole read-check-write runs under a shared flock on
   `(flock/guard-path (lock-path git-dir))`, never on the lock path
   itself: acquire! rewrites (and release! deletes) that path, so a lock
   held on it would stop protecting anything the instant it's rewritten.
   A guard file that acquire! never touches keeps the flock's identity
   independent of the record's lifecycle, so two concurrent triggers in
   the same clone cannot both observe a free or dead lock and both
   proceed."
  [git-dir {:keys [pr sha]} {:keys [pid kill-fn]}]
  (flock/with-file-lock
    (flock/guard-path (lock-path git-dir))
    (fn []
      (let [pid (or pid (.pid (java.lang.ProcessHandle/current)))
            kill-fn (or kill-fn kill-reviewers!)
            held (read-lock git-dir)]
        (cond
          (and held (alive? (:pid held)) (= sha (:sha held)))
          {:status :duplicate}

          (and held (alive? (:pid held)))
          (do (kill-fn (:pid held))
              (write-lock! git-dir {:pid pid :pr pr :sha sha})
              {:status :superseded :killed-pid (:pid held)})

          :else
          (do (write-lock! git-dir {:pid pid :pr pr :sha sha})
              {:status :acquired}))))))

(defn superseded?
  "True when the lock record no longer names `pid` — another trigger took the
   reviewer slot while this one was working.

   This is how a loser learns it lost. `kill-reviewers!` kills the reviewer
   child, not the trigger, so the trigger returns from a reviewer that was
   SIGTERMed mid-answer; recording that as a pass would spend a cap slot on a
   truncated review and wake agent A with findings for a SHA that is already
   stale.

   A missing record counts as superseded too. It can only mean this trigger
   was superseded and the winner has since released, or that something
   outside the loop deleted the record; in both cases the conservative move
   is to stay quiet rather than publish a pass whose lock is gone."
  [git-dir {:keys [pid]}]
  (let [pid (or pid (.pid (java.lang.ProcessHandle/current)))]
    (not= pid (:pid (read-lock git-dir)))))

(defn release!
  "Release the lock, but only when it is still held by `pid` (default this
   process's own pid, matching acquire!'s default). Runs under the same
   guard flock acquire! uses, so a reviewer finishing normally can never
   interleave with an in-flight acquire! that is concurrently superseding
   it.

   Both halves matter together: without the flock, release! could still
   run between acquire!'s kill and its write of the new record; without
   the pid check, release! would delete whatever record it finds even
   after losing that race. Either alone lets a reviewer that has already
   been superseded delete the new holder's record — the lock then reads
   free while a reviewer is actually still running, which is exactly what
   acquire!'s duplicate/superseded logic exists to prevent."
  ([git-dir] (release! git-dir {}))
  ([git-dir {:keys [pid]}]
   (let [pid (or pid (.pid (java.lang.ProcessHandle/current)))]
     (flock/with-file-lock
       (flock/guard-path (lock-path git-dir))
       (fn []
         (let [held (read-lock git-dir)]
           (when (= pid (:pid held))
             (fs/delete-if-exists (lock-path git-dir))))))
     nil)))
