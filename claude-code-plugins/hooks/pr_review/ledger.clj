(ns pr-review.ledger
  "Append-only, per-clone record of review passes, and the termination rules
   read off it.

   Lives under the clone's shared git directory so it survives session
   restarts, context compaction and `claude` upgrades, needs no network, and
   works before a PR exists. Same precedent as Claude Code's own
   .git/claude-trailers.

   Every function takes `git-dir` — what pr-review.gh/git-common-dir
   resolved — never a repo root. In a linked worktree `<root>/.git` is a
   file, so a repo-root-relative ledger is unwritable there and invisible to
   every other worktree of the same clone.

   The policy predicates are pure over an already-read `passes` collection so
   one decision costs one file read. They used to each read the file
   themselves, and the one-re-raise filter called a per-fingerprint helper
   that re-slurped the whole ledger — 138 full reads for one decision at nine
   passes and fifteen findings."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.string :as str]
            [pr-review.flock :as flock])
  (:import [java.nio.file CopyOption Files StandardCopyOption]))

(def max-passes
  "Hard cap on review passes per PR. Ported from a real incident: a sibling
   repo ran twelve passes and ~40 correctness findings on one PR without
   converging, two thirds of them defects in fixes for the previous pass."
  10)

(def suppressible-categories
  "The only finding categories the one-re-raise rule may ever silence.

   The spec authorises it for follow-up grade only — \"A follow-up-grade
   finding may be re-raised once, then it stays on the list.\" A
   correctness/blocking or coverage finding still present after two passes
   has not been fixed; silencing it makes the next pass report MERGEABLE with
   the defect still in the tree, and the loop then merges broken code."
  #{"correctness/followup" "docs-accuracy" "style"})

(def ^:private max-passes-kept
  "How many passes are kept PER PR, not per file.

   A global line cap loses history for whichever PRs are oldest, and all three
   readers of this file need a PR's OWN rows: `reviewed-sha?` (drop one and a
   reviewed sha is reviewed again, spending a cap slot), `cap-reached?` (drop
   one and the cap under-counts, so the loop never terminates) and
   `suppressed-fingerprints` (drop one and a finding raised twice is raised a
   third time). Measured when this was found: a 500-line global cap against
   ~50 rows per repo per three days, so a still-open PR would start silently
   losing its cap count inside a month.

   `max-passes` is 10, so 12 keeps every row a live decision can ask for plus
   slack for a PR that was capped and then force-pushed."
  12)

(defn ledger-path
  [git-dir]
  (str git-dir "/pr-review-ledger.jsonl"))

(defn- parse-line
  [line]
  (try (json/parse-string line true)
       (catch Exception _ nil)))

(defn- read-all
  [git-dir]
  (let [p (ledger-path git-dir)]
    (if-not (fs/exists? p)
      []
      (into [] (keep parse-line) (str/split-lines (slurp p))))))

(defn read-passes
  "Every recorded pass for `pr-number`, oldest first. Unparseable lines are
   skipped: a reviewer killed mid-write must not break all later reads.

   This is the only function here that touches the filesystem. Read once,
   then hand the result to the pure predicates below."
  [git-dir pr-number]
  (filterv #(= pr-number (:pr %)) (read-all git-dir)))

(defn next-pass-number
  [passes]
  (inc (count passes)))

(defn cap-reached?
  [passes]
  (>= (count passes) max-passes))

(defn reviewed-sha?
  "True when `passes` already records a completed pass at `sha`.

   Repeat pushes of one commit are ordinary: `git push` twice, `git push
   --tags`, `--dry-run` and `--delete` all match `Bash(git push:*)`. Without
   this the same commit is reviewed again on every one of them, spending a
   cap slot each time and telling agent A nothing it has not already been
   told."
  [passes sha]
  (boolean (and sha (some #(= sha (:sha %)) passes))))

(defn fingerprint-category
  "The category segment of a `<file>:<line>:<category>` fingerprint.

   Read after the LAST colon rather than by splitting on colons: a path may
   contain one (`src/pool:v2/file.clj:34:style`), a category never does."
  [fingerprint]
  (let [s (str fingerprint)]
    (when-let [i (str/last-index-of s ":")]
      (subs s (inc i)))))

(defn suppressible?
  "Whether the one-re-raise rule is allowed to silence this fingerprint at
   all — see `suppressible-categories`."
  [fingerprint]
  (contains? suppressible-categories (fingerprint-category fingerprint)))

(defn suppressed-fingerprints
  "Fingerprints the next reviewer must not raise again: reported on two or
   more of `passes` AND of a suppressible category.

   Both conditions are load-bearing. Without the count the rule fires on a
   first sighting; without the category filter it fires on blocking and
   coverage findings, which is a false-clean path, not a convergence aid.

   Order is first-appearance so the prompt's do-not-re-raise list is stable
   between passes."
  [passes]
  (let [per-pass (mapv (comp distinct :fingerprints) passes)
        all      (vec (apply concat per-pass))
        freq     (frequencies all)]
    (->> all
         distinct
         (filterv #(and (>= (get freq % 0) 2) (suppressible? %))))))

(defn- atomic-replace!
  "Atomically replace `path`'s content with `tmp`'s.

   Renamed into place rather than written in place: `spit` truncates on
   open, so a process killed between truncate and flush would zero the
   entire ledger — exactly what lock/acquire! does to a superseded
   reviewer mid-run. Same filesystem (tmp is a sibling of path), so
   ATOMIC_MOVE is a real rename, not a copy."
  [tmp path]
  (Files/move (fs/path tmp) (fs/path path)
              (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE])))

(defn- trim-per-pr
  "Keep the newest `max-passes-kept` rows for each PR, in file order.

   Rows that do not parse are kept: they are not ours to drop, and a parse
   failure must never become a silent deletion."
  [lines]
  (let [pr-of (fn [l] (try (:pr (json/parse-string l true))
                           (catch Exception _ ::unparsed)))
        keep? (->> (map-indexed vector lines)
                   (group-by (fn [[_ l]] (pr-of l)))
                   (mapcat (fn [[pr idxs]]
                             (if (= ::unparsed pr)
                               (map first idxs)
                               (map first (take-last max-passes-kept idxs)))))
                   set)]
    (vec (keep-indexed (fn [i l] (when (keep? i) l)) lines))))

(defn append-pass!
  "Append one pass entry, stamping :ts. Trims per PR under the same
   lock so concurrent triggers cannot interleave a read-trim-write. Writes
   the full trimmed content to a temp file and renames it into place, so a
   process killed mid-write can never truncate the previously recorded
   passes — it only ever loses its own not-yet-published entry.

   Flocks `(flock/guard-path p)`, never `p` itself: `p` is the path this
   function renames over, and a lock held on a path that gets renamed away
   from under it stops protecting anything the instant the rename happens.
   See pr-review.flock's namespace docstring."
  [git-dir entry]
  (let [entry (assoc entry :ts (System/currentTimeMillis))
        p (ledger-path git-dir)]
    (flock/with-file-lock (flock/guard-path p)
      (fn []
        (let [existing (if (fs/exists? p)
                         (vec (remove str/blank? (str/split-lines (slurp p))))
                         [])
              lines (conj existing (json/generate-string entry))
              kept (trim-per-pr lines)
              tmp (str p ".tmp")]
          (spit tmp (str (str/join "\n" kept) "\n"))
          (atomic-replace! tmp p))))
    entry))
