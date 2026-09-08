(ns pr-review.reviewer
  "Spawn the independent reviewer and parse what it says.

   The reviewer is a separate `claude -p` process: fresh context window, none
   of agent A's conversation history, the same machine and working tree. Its
   tool list is Read,Grep,Glob — no Bash, so nothing it runs can be rewritten
   by rtk and nothing it does can touch the tree."
  (:require [babashka.process :as p]
            [clojure.string :as str]))

(def categories
  ["correctness/blocking" "correctness/followup" "coverage"
   "docs-accuracy" "style"])

(defn claude-argv
  []
  ["claude" "-p"
   "--model" "opus"
   "--allowedTools" "Read,Grep,Glob"])

(defn- default-spawn
  [argv prompt dir]
  (let [{:keys [exit out err]} (p/sh argv {:dir dir :in prompt})]
    {:exit exit :out (or out "") :err (or err "")}))

(defn run!
  "Run the reviewer with `prompt` on stdin, in `repo-root`.
   Never throws: a spawn failure becomes a non-zero exit with the message in
   :err, so the caller can still record a pass and tell the author."
  [prompt repo-root opts]
  (let [spawn (or (:spawn-fn opts) default-spawn)]
    (try (spawn (claude-argv) prompt repo-root)
         (catch Exception e {:exit 127 :out "" :err (str (ex-message e))}))))

(defn- parse-verdict
  "The verdict line must start at column 0. Leading whitespace means the line
   is quoted or indented — e.g. an echoed copy of the core prompt's own format
   example — not the reviewer's real, final verdict. Without this anchor an
   echoed template parses as a clean pass, which is worse than the reviewer
   failing to run at all: it emits a positive signal for a review that never
   happened."
  [out]
  (when-let [[_ v] (re-find #"(?m)^VERDICT:\s*(MERGEABLE|NOT MERGEABLE)" out)]
    v))

(defn- parse-counts
  "Read the per-category count block. \"none\" means 0 — a missing key and a
   zero count must not be confusable, or a clean pass reads as an unparsed one."
  [out]
  (into {}
        (map (fn [cat]
               (let [re (re-pattern (str "(?m)^\\s*\\[" cat "\\]\\s+(none|\\d+)"))
                     [_ n] (re-find re out)]
                 [cat (cond (nil? n) 0
                            (= "none" n) 0
                            :else (parse-long n))])))
        categories))

(defn- parse-fingerprints
  "Stable identity for a finding: file:line:category, taken from numbered
   finding lines of the form `N. [category] path:line — text`. The path
   segment is non-greedy and anchors on the final `:<digits>` boundary (the
   number is followed by whitespace or end of line), so a path containing a
   space or an internal colon is still captured whole instead of truncating
   at the first space or colon inside it."
  [out]
  (->> (str/split-lines out)
       (keep (fn [line]
               (when-let [[_ cat path ln]
                          (re-find #"^\s*\d+\.\s*\[([a-z/-]+)\]\s+(.+?):(\d+)(?=\s|$)" line)]
                 (str path ":" ln ":" cat))))
       distinct
       vec))

(defn parse-output
  [out]
  (let [out (or out "")]
    (if-let [v (parse-verdict out)]
      {:verdict v
       :counts (parse-counts out)
       :fingerprints (parse-fingerprints out)
       :body out}
      {:verdict "MALFORMED"
       :counts (zipmap categories (repeat 0))
       :fingerprints []
       :body (str/trim out)})))

(defn mergeable?
  "MERGEABLE means exactly: no correctness/blocking finding, and no coverage
   finding. The verdict line is the reviewer's claim; the counts are the
   evidence, and the evidence wins."
  [{:keys [verdict counts]}]
  (and (= "MERGEABLE" verdict)
       (zero? (get counts "correctness/blocking" 0))
       (zero? (get counts "coverage" 0))))
