(ns pr-review.context
  "Precompute everything the reviewer needs to read, so the reviewer needs no
   Bash at all.

   Dropping Bash from B removes two problems in one move: rtk cannot truncate
   a diff B never has to reconstruct, and the one view of the change that
   is guaranteed complete: B may run its own git commands now, but nothing
   makes it run the RIGHT range, and the review must not depend on it doing so."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [pr-review.gh :as gh]))

(defn context-dir
  "Under the clone's shared git directory (pr-review.gh/git-common-dir), not
   `<repo-root>/.git`: that path is a file in a linked worktree, so
   `fs/create-dirs` on it throws and every push from a worktree died."
  [git-dir]
  (str git-dir "/pr-review-context"))

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

(defn diff-path
  "Where one review's diff lives: `<context-dir>/<pr>-<sha>.diff`.

   The PR is part of the name, not decoration. Two open PRs can share a head
   sha — checkout.clj says so, and lock.clj made the lock per PR so their
   reviews now run CONCURRENTLY, which is exactly and only when this collides.
   Keyed by sha alone, the second `build!` replaced the file the first one's
   reviewer had already been pointed at: it read the other PR's diff while its
   own prompt still listed this PR's changed files, and the findings were
   posted here."
  [git-dir pr sha]
  (str (context-dir git-dir) "/" pr "-" sha ".diff"))

(defn build!
  "Write the true diff for this push to `(diff-path git-dir pr sha)` and return
   the paths and metadata the prompt will reference.

   :diff-failed? is true when the diff command itself could not be produced
   (e.g. an unresolved base ref) — never conflated with a genuinely empty
   diff, which reports false. The file is still written (empty, in that
   case) either way, so nothing downstream ever names a missing path.

   `repo-root` is where git runs; `git-dir` is where the result is written.
   The two differ in a linked worktree and must not be conflated.

   opts may override :merge-base-fn and :diff-fn for testing; both default to
   the real git calls in pr-review.gh."
  [repo-root git-dir {:keys [pr sha base-ref]} opts]
  (let [merge-base-fn (or (:merge-base-fn opts)
                          #(gh/merge-base repo-root base-ref opts))
        diff-fn       (or (:diff-fn opts)
                          #(gh/diff repo-root %1 %2 opts))
        base          (or (merge-base-fn) (str "origin/" base-ref))
        diff-result   (diff-fn base sha)
        diff-failed?  (nil? diff-result)
        diff-text     (or diff-result "")
        dir           (context-dir git-dir)
        path          (diff-path git-dir pr sha)]
    (fs/create-dirs dir)
    (spit path diff-text)
    {:diff-path     path
     :changed-files (changed-files diff-text)
     :base          base
     :sha           sha
     :diff-bytes    (fs/size path)
     :diff-failed?  diff-failed?}))

(defn prune!
  "Delete all but the `keep` newest .diff files OF THIS PR. Returns how many
   were removed.

   Per PR, for the same reason the name is: pruning across the whole clone let
   a review that finished delete the diff file a concurrently running review's
   prompt names, and five passes on a busy PR was enough to do it."
  [git-dir pr keep]
  (let [files (->> (fs/glob (context-dir git-dir) (str pr "-*.diff"))
                   (sort-by #(fs/last-modified-time %))
                   reverse
                   (drop keep))]
    (doseq [f files] (fs/delete-if-exists f))
    (count files)))
