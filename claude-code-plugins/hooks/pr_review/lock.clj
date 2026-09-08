(ns pr-review.lock
  "At most one live reviewer per clone, always on the newest pushed SHA.

   Two rapid pushes must not leave a reviewer grinding on a SHA that is
   already stale — the newer push kills the older reviewer and takes over."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]))

(defn lock-path
  [repo-root]
  (str repo-root "/.git/pr-review.lock"))

(defn read-lock
  "Current lock record, or nil when absent or unparseable. A corrupt lock
   reads as free: a half-written file must not wedge the loop forever."
  [repo-root]
  (let [p (lock-path repo-root)]
    (when (fs/exists? p)
      (try (json/parse-string (slurp p) true)
           (catch Exception _ nil)))))

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
   :acquired   — the lock was free, corrupt, or held by a dead process."
  [repo-root {:keys [pr sha]} {:keys [pid kill-fn]}]
  (let [pid     (or pid (.pid (java.lang.ProcessHandle/current)))
        kill-fn (or kill-fn default-kill!)
        held    (read-lock repo-root)]
    (cond
      (and held (alive? (:pid held)) (= sha (:sha held)))
      {:status :duplicate}

      (and held (alive? (:pid held)))
      (do (kill-fn (:pid held))
          (write-lock! repo-root {:pid pid :pr pr :sha sha})
          {:status :superseded :killed-pid (:pid held)})

      :else
      (do (write-lock! repo-root {:pid pid :pr pr :sha sha})
          {:status :acquired}))))

(defn release!
  [repo-root]
  (fs/delete-if-exists (lock-path repo-root))
  nil)
