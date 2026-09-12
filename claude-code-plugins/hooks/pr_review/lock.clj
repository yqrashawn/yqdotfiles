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
  "One lock per PR, not per clone.

   A single `<git-dir>/pr-review.lock` made every PR in a clone contend for
   one reviewer slot, and `acquire!` supersedes on any sha it does not
   recognise — so pushing PR #397 killed PR #395's running reviewer and
   dropped that review with no ledger row and no message. A repository with
   five open PRs does that constantly.

   Superseding is still exactly right WITHIN a PR: two rapid pushes must not
   run two reviewers on stale shas (R14). Scoping the lock to the PR is what
   makes those two statements stop contradicting each other."
  [git-dir pr]
  (str git-dir "/pr-review." pr ".lock"))

(defn read-lock
  "Current lock record, or nil when absent, unparseable, or missing a
   usable :pid. A corrupt or incomplete lock reads as free: a half-written
   file must not wedge the loop forever."
  [git-dir pr]
  (let [p (lock-path git-dir pr)]
    (when (fs/exists? p)
      (let [parsed (try (json/parse-string (slurp p) true)
                        (catch Exception _ nil))]
        (when (:pid parsed)
          parsed)))))

(defn alive?
  [pid]
  (let [h (java.lang.ProcessHandle/of (long pid))]
    (and (.isPresent h) (.isAlive (.get h)))))

(defn started-at-of
  "The process's own start instant in milliseconds, or nil.

   A pid alone does not identify a process: the OS recycles them, so a dead
   reviewer's pid can belong to something else entirely by the time a later
   trigger reads the lock. `alive?` would say yes and `kill-reviewers!` would
   then SIGTERM that stranger's whole subtree. Pid plus start time is the
   standard identity, and the kernel supplies it."
  [pid]
  (try
    (let [h (java.lang.ProcessHandle/of (long pid))]
      (when (.isPresent h)
        (let [i (.startInstant (.info (.get h)))]
          (when (.isPresent i) (.toEpochMilli (.get i))))))
    (catch Exception _ nil)))

(defn same-process?
  "True when `pid` is still the process the lock recorded.

   A record with no `:started-at` predates this check; it is trusted, because
   refusing would wedge every lock written by an older version."
  [{:keys [pid started-at] :as held}]
  (and held
       (alive? pid)
       (or (nil? started-at) (= started-at (started-at-of pid)))))

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
  [git-dir pr-key {:keys [pid pr sha branch]}]
  (fs/create-dirs (fs/parent (lock-path git-dir pr-key)))
  (spit (lock-path git-dir pr-key)
        (json/generate-string {:pid pid :started-at (started-at-of pid)
                               :pr pr :sha sha :branch branch
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
  [git-dir {:keys [pr sha branch]} {:keys [pid kill-fn]}]
  (flock/with-file-lock
    (flock/guard-path (lock-path git-dir pr))
    (fn []
      (let [pid (or pid (.pid (java.lang.ProcessHandle/current)))
            kill-fn (or kill-fn kill-reviewers!)
            held (read-lock git-dir pr)]
        (cond
          (and (same-process? held) (= sha (:sha held)))
          {:status :duplicate}

          ;; `same-process?`, not `alive?`: the OS recycles pids, so a dead
          ;; reviewer's pid can belong to a stranger by now, and killing on
          ;; liveness alone would SIGTERM that stranger's whole subtree.
          (same-process? held)
          (do (kill-fn (:pid held))
              (write-lock! git-dir pr {:pid pid :pr pr :sha sha :branch branch})
              {:status :superseded :killed-pid (:pid held)})

          :else
          (do (write-lock! git-dir pr {:pid pid :pr pr :sha sha :branch branch})
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
  [git-dir pr {:keys [pid]}]
  (let [pid (or pid (.pid (java.lang.ProcessHandle/current)))]
    (not= pid (:pid (read-lock git-dir pr)))))

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
  ([git-dir pr] (release! git-dir pr {}))
  ([git-dir pr {:keys [pid]}]
   (let [pid (or pid (.pid (java.lang.ProcessHandle/current)))]
     (flock/with-file-lock
       (flock/guard-path (lock-path git-dir pr))
       (fn []
         (let [held (read-lock git-dir pr)]
           (when (= pid (:pid held))
             (fs/delete-if-exists (lock-path git-dir pr))))))
     nil)))

(defn abandoned
  "Reviews that STARTED and never finished, in `git-dir`: lock records whose
   holder is no longer alive.

   The signal needs nothing new on disk. `release!` deletes the record on
   every path a review can finish by, and `acquire!` overwrites it — so a
   record naming a dead pid can only mean the reviewer was killed mid-run.
   Measured: a session restart at 10:36 killed a review started at 10:32 and
   left exactly this, with no ledger row and no message, and the PR was never
   reviewed again because nothing looked for it.

   `:branch` may be absent on a record written before it was recorded; the
   caller has to tolerate that rather than skip the retry."
  [git-dir]
  (->> (try (fs/list-dir git-dir) (catch Exception _ nil))
       (keep (fn [f]
               (let [pr (some-> (re-find #"pr-review\.(\d+)\.lock$"
                                         (str (fs/file-name f)))
                                second parse-long)]
                 (when pr
                   (when-let [held (read-lock git-dir pr)]
                     (when-not (same-process? held)
                       (assoc held :pr pr)))))))
       (sort-by :started >)))
