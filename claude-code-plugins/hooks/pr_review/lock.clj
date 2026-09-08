(ns pr-review.lock
  "At most one live reviewer per clone, always on the newest pushed SHA.

   Two rapid pushes must not leave a reviewer grinding on a SHA that is
   already stale — the newer push kills the older reviewer and takes over."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [pr-review.flock :as flock]))

(defn lock-path
  [repo-root]
  (str repo-root "/.git/pr-review.lock"))

(defn read-lock
  "Current lock record, or nil when absent, unparseable, or missing a
   usable :pid. A corrupt or incomplete lock reads as free: a half-written
   file must not wedge the loop forever."
  [repo-root]
  (let [p (lock-path repo-root)]
    (when (fs/exists? p)
      (let [parsed (try (json/parse-string (slurp p) true)
                        (catch Exception _ nil))]
        (when (:pid parsed)
          parsed)))))

(defn alive?
  [pid]
  (let [h (java.lang.ProcessHandle/of (long pid))]
    (and (.isPresent h) (.isAlive (.get h)))))

(defn- default-kill!
  [pid]
  (try (p/sh ["kill" (str pid)]) (catch Exception _ nil)))

(defn- write-lock!
  [repo-root {:keys [pid pr sha]}]
  (fs/create-dirs (fs/parent (lock-path repo-root)))
  (spit (lock-path repo-root)
        (json/generate-string {:pid pid :pr pr :sha sha
                               :started (System/currentTimeMillis)})))

(defn acquire!
  "Take the reviewer lock for (`pr`, `sha`).

   :duplicate  — a live reviewer already holds this exact SHA. Caller exits 0.
   :superseded — a live reviewer held an older SHA; it was killed. Proceed.
   :acquired   — the lock was free, corrupt, or held by a dead process.

   The whole read-check-write runs under a shared flock on
   `(flock/guard-path (lock-path repo-root))`, never on the lock path
   itself: acquire! rewrites (and release! deletes) that path, so a lock
   held on it would stop protecting anything the instant it's rewritten.
   A guard file that acquire! never touches keeps the flock's identity
   independent of the record's lifecycle, so two concurrent triggers in
   the same clone cannot both observe a free or dead lock and both
   proceed."
  [repo-root {:keys [pr sha]} {:keys [pid kill-fn]}]
  (flock/with-file-lock
    (flock/guard-path (lock-path repo-root))
    (fn []
      (let [pid (or pid (.pid (java.lang.ProcessHandle/current)))
            kill-fn (or kill-fn default-kill!)
            held (read-lock repo-root)]
        (cond
          (and held (alive? (:pid held)) (= sha (:sha held)))
          {:status :duplicate}

          (and held (alive? (:pid held)))
          (do (kill-fn (:pid held))
              (write-lock! repo-root {:pid pid :pr pr :sha sha})
              {:status :superseded :killed-pid (:pid held)})

          :else
          (do (write-lock! repo-root {:pid pid :pr pr :sha sha})
              {:status :acquired}))))))

(defn release!
  [repo-root]
  (fs/delete-if-exists (lock-path repo-root))
  nil)
