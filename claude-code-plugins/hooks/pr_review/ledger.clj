(ns pr-review.ledger
  "Append-only, per-clone record of review passes.

   Lives under .git/ so it survives session restarts, context compaction and
   `claude` upgrades, needs no network, and works before a PR exists. Same
   precedent as Claude Code's own .git/claude-trailers."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.string :as str])
  (:import [java.io RandomAccessFile]))

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

(defn- with-file-lock
  "Run `f` while holding an exclusive lock on `path`.

   The lock is released by closing the channel, not by calling .release —
   babashka does not allow sun.nio.ch.FileLockImpl.release."
  [path f]
  (fs/create-dirs (fs/parent path))
  (when-not (fs/exists? path) (spit path ""))
  (with-open [raf (RandomAccessFile. (str path) "rw")]
    (.lock (.getChannel raf))
    (f)))

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

(defn append-pass!
  "Append one pass entry, stamping :ts. Trims to `max-lines` under the same
   lock so concurrent triggers cannot interleave a read-trim-write."
  [repo-root entry]
  (let [entry (assoc entry :ts (System/currentTimeMillis))
        p     (ledger-path repo-root)]
    (with-file-lock p
      (fn []
        (let [existing (if (fs/exists? p)
                         (vec (remove str/blank? (str/split-lines (slurp p))))
                         [])
              lines    (conj existing (json/generate-string entry))
              kept     (vec (take-last max-lines lines))]
          (spit p (str (str/join "\n" kept) "\n")))))
    entry))
