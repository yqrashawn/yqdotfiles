(ns pr-review.ledger
  "Append-only, per-clone record of review passes.

   Lives under .git/ so it survives session restarts, context compaction and
   `claude` upgrades, needs no network, and works before a PR exists. Same
   precedent as Claude Code's own .git/claude-trailers."
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

(def ^:private max-lines
  "Ledger is trimmed to this many lines under the write lock. At ~200 bytes a
   line this bounds the file at ~100KB."
  500)

(defn ledger-path
  [repo-root]
  (str repo-root "/.git/pr-review-ledger.jsonl"))

(defn- parse-line
  [line]
  (try (json/parse-string line true)
       (catch Exception _ nil)))

(defn- read-all
  [repo-root]
  (let [p (ledger-path repo-root)]
    (if-not (fs/exists? p)
      []
      (into [] (keep parse-line) (str/split-lines (slurp p))))))

(defn read-passes
  "Every recorded pass for `pr-number`, oldest first. Unparseable lines are
   skipped: a reviewer killed mid-write must not break all later reads."
  [repo-root pr-number]
  (filterv #(= pr-number (:pr %)) (read-all repo-root)))

(defn next-pass-number
  [repo-root pr-number]
  (inc (count (read-passes repo-root pr-number))))

(defn cap-reached?
  [repo-root pr-number]
  (>= (count (read-passes repo-root pr-number)) max-passes))

(defn raise-count
  "How many passes have reported `fingerprint` for `pr-number`.
   The one-re-raise rule fires when this is already >= 2."
  [repo-root pr-number fingerprint]
  (count (filter #(contains? (set (:fingerprints %)) fingerprint)
                 (read-passes repo-root pr-number))))

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

(defn append-pass!
  "Append one pass entry, stamping :ts. Trims to `max-lines` under the same
   lock so concurrent triggers cannot interleave a read-trim-write. Writes
   the full trimmed content to a temp file and renames it into place, so a
   process killed mid-write can never truncate the previously recorded
   passes — it only ever loses its own not-yet-published entry.

   Flocks `(flock/guard-path p)`, never `p` itself: `p` is the path this
   function renames over, and a lock held on a path that gets renamed away
   from under it stops protecting anything the instant the rename happens.
   See pr-review.flock's namespace docstring."
  [repo-root entry]
  (let [entry (assoc entry :ts (System/currentTimeMillis))
        p (ledger-path repo-root)]
    (flock/with-file-lock (flock/guard-path p)
      (fn []
        (let [existing (if (fs/exists? p)
                         (vec (remove str/blank? (str/split-lines (slurp p))))
                         [])
              lines (conj existing (json/generate-string entry))
              kept (vec (take-last max-lines lines))
              tmp (str p ".tmp")]
          (spit tmp (str (str/join "\n" kept) "\n"))
          (atomic-replace! tmp p))))
    entry))
