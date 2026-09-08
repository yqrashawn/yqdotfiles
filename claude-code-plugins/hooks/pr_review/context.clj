(ns pr-review.context
  "Precompute everything the reviewer needs to read, so the reviewer needs no
   Bash at all.

   Dropping Bash from B removes two problems in one move: rtk cannot truncate
   a diff B never runs, and a reviewer with no shell cannot mutate the tree."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [pr-review.gh :as gh]))

(defn context-dir
  [repo-root]
  (str repo-root "/.git/pr-review-context"))

(defn changed-files
  "Repo-relative paths touched by a unified diff, in first-appearance order.

   Takes the b/ side, not the a/ side: for a rename they differ, and the
   reviewer needs the path that exists after the change."
  [diff-text]
  (->> (str/split-lines (or diff-text ""))
       (keep (fn [line]
               (when (str/starts-with? line "diff --git ")
                 (let [[_ _a b] (re-find #"^diff --git a/(.+?) b/(.+)$" line)]
                   b))))
       distinct
       vec))

(defn build!
  "Write the true diff for this push to `<context-dir>/<sha>.diff` and return
   the paths and metadata the prompt will reference.

   opts may override :merge-base-fn and :diff-fn for testing; both default to
   the real git calls in pr-review.gh."
  [repo-root {:keys [sha base-ref]} opts]
  (let [merge-base-fn (or (:merge-base-fn opts)
                          #(gh/merge-base repo-root base-ref opts))
        diff-fn       (or (:diff-fn opts)
                          #(gh/diff repo-root %1 %2 opts))
        base          (or (merge-base-fn) (str "origin/" base-ref))
        diff-text     (or (diff-fn base sha) "")
        dir           (context-dir repo-root)
        diff-path     (str dir "/" sha ".diff")]
    (fs/create-dirs dir)
    (spit diff-path diff-text)
    {:diff-path     diff-path
     :changed-files (changed-files diff-text)
     :base          base
     :sha           sha
     :diff-bytes    (count diff-text)}))

(defn prune!
  "Delete all but the `keep` newest .diff files. Returns how many were removed."
  [repo-root keep]
  (let [files (->> (fs/glob (context-dir repo-root) "*.diff")
                   (sort-by #(fs/last-modified-time %))
                   reverse
                   (drop keep))]
    (doseq [f files] (fs/delete-if-exists f))
    (count files)))
