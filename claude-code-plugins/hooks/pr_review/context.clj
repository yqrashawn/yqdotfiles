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
  [repo-root git-dir {:keys [pr sha base-ref since-sha]} opts]
  (let [merge-base-fn (or (:merge-base-fn opts)
                          #(gh/merge-base repo-root base-ref opts))
        diff-fn       (or (:diff-fn opts)
                          #(gh/diff repo-root %1 %2 opts))
        base          (or (merge-base-fn) (str "origin/" base-ref))
        diff-result   (diff-fn base sha)
        diff-failed?  (nil? diff-result)
        diff-text     (or diff-result "")
        dir           (context-dir git-dir)
        path          (diff-path git-dir pr sha)
        ;; On a re-review, ALSO write the diff of just the new commits.
        ;; Measured on PR #478: ten passes on a 1801-line change, each one
        ;; finding a real defect in the PREVIOUS pass's fix -- "a number that
        ;; parses is not a port", "the fail-closed unset was itself inside a
        ;; guard that failed". So the next defect lives in the fix just made,
        ;; which is exactly what this file shows, and reading 1801 lines again
        ;; to find it is what made each round trip cost a full pass.
        ;;
        ;; The same three-dot form the full diff uses, with the previously
        ;; reviewed sha as the base. For sequential pushes that sha is an
        ;; ancestor, so three-dot and two-dot are byte-identical -- measured,
        ;; 2092 bytes either way -- and when the branch was force-pushed
        ;; three-dot is the safer of the two, since two-dot would render the
        ;; whole rebase as change.
        ;;
        ;; The full diff is still written and still named in the prompt. This
        ;; narrows what the reviewer reads FIRST, never what it may read.
        incr-result   (when (and since-sha (not= since-sha sha))
                        (diff-fn since-sha sha))
        incr-path     (when incr-result
                        (str (fs/strip-ext path) ".since-"
                             (subs since-sha 0 (min 12 (count since-sha))) ".diff"))]
    (fs/create-dirs dir)
    (spit path diff-text)
    (when incr-path (spit incr-path incr-result))
    (cond-> {:diff-path     path
             :changed-files (changed-files diff-text)
             :base          base
             :sha           sha
             :diff-bytes    (fs/size path)
             :diff-failed?  diff-failed?}
      incr-path (assoc :incr-path incr-path
                       :incr-bytes (fs/size incr-path)
                       :since-sha since-sha))))

(defn prune!
  "Delete all but the `keep` newest .diff files OF THIS PR. Returns how many
   were removed.

   Per PR, for the same reason the name is: pruning across the whole clone let
   a review that finished delete the diff file a concurrently running review's
   prompt names, and five passes on a busy PR was enough to do it."
  [git-dir pr keep]
  (let [;; Grouped by SHA, because a re-review writes TWO files for one pass --
        ;; the full diff and the increment since the last pass. Counting files
        ;; made `keep` mean two and a half passes, and with a small keep it
        ;; deleted the FULL diff while keeping the increment, which is the one
        ;; the prompt tells the reviewer to open for closure-verification.
        by-sha (->> (fs/glob (context-dir git-dir) (str pr "-*.diff"))
                    (group-by #(-> (str (fs/file-name %))
                                   (subs (inc (count (str pr))))
                                   (str/split #"\.")
                                   first)))
        newest (fn [fs] (apply max (map #(.toMillis (fs/last-modified-time %)) fs)))
        doomed (->> by-sha
                    (sort-by (comp newest val))
                    reverse
                    (drop keep)
                    (mapcat val))]
    (doseq [f doomed] (fs/delete-if-exists f))
    (count doomed)))
