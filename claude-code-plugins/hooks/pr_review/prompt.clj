(ns pr-review.prompt
  "Assemble the reviewer prompt: generic core, per-repo overlay, one-shot hint,
   and the pass state that drives the FIRST/RE-REVIEW severity asymmetry."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]))

(defn overlay-path
  "Per-repo enumeration recipe. Optional — the core alone is functional."
  [repo-root]
  (str repo-root "/.claude/pr-review.md"))

(defn hint-path
  "One-shot note from agent A to the reviewer.

   Under the clone's shared git directory (pr-review.gh/git-common-dir), so
   the hint is readable from a linked worktree — where `<repo-root>/.git` is
   a file and nothing can live under it."
  [git-dir]
  (str git-dir "/pr-review-hint"))

(defn read-hint!
  "Read and delete the hint. A hint is scoped to one review; leaving it in
   place would silently steer every later pass on the PR."
  [git-dir]
  (let [p (hint-path git-dir)]
    (when (fs/exists? p)
      (let [s (str/trim (slurp p))]
        (fs/delete-if-exists p)
        (when-not (str/blank? s) s)))))

(defn- section
  [title body]
  (when-not (str/blank? (str body))
    (str "\n## " title "\n\n" body "\n")))

(defn build
  [{:keys [core repo-root git-dir ctx pr pass draft? prior-fingerprints]}]
  (let [overlay (when (fs/exists? (overlay-path repo-root))
                  (slurp (overlay-path repo-root)))
        hint    (read-hint! git-dir)
        first?  (= 1 pass)]
    (str
     core
     (section "This review"
              (str/join "\n"
                        (cond-> [(str "REPO ROOT: " repo-root)
                                 (str "PR NUMBER: " pr)
                                 (str "PASS: " (if first? "FIRST" "RE-REVIEW")
                                      " (pass " pass ")")
                                 (str "BASE: " (:base ctx))
                                 (str "HEAD: " (:sha ctx))]
                          draft? (conj "PR STATE: draft — review it as you would any other")
                          true   (conj (if (zero? (:diff-bytes ctx))
                                         "DIFF: empty — report that and stop; do not invent findings"
                                         (str "DIFF: " (:diff-bytes ctx) " bytes"))))))
     (section "How to read the change"
              (str "The complete, untruncated diff is on disk. Read it first:\n\n"
                   "    " (:diff-path ctx) "\n\n"
                   "Then read the surrounding source under the repo root for context.\n"
                   "You have Read, Grep, Glob and Bash, in a throwaway worktree checked\n"
                   "out at the commit under review and deleted when this review ends.\n"
                   "Inspect the change however is useful; `git push`, `git commit` and\n"
                   "`gh pr` are refused, because altering the PR is not your job."))
     (section "Changed files"
              (if (seq (:changed-files ctx))
                (str/join "\n" (map #(str "- " %) (:changed-files ctx)))
                "(none)"))
     (when (seq prior-fingerprints)
       (section "Already reported twice — do not re-raise"
                (str "These findings have been raised on two previous passes. They stay\n"
                     "on the follow-up list; reporting them again gates verified fixes\n"
                     "from shipping.\n\n"
                     (str/join "\n" (map #(str "- " %) prior-fingerprints)))))
     (when overlay (section "Repository-specific review notes" overlay))
     (when hint (section "Note from the author for this review" hint)))))
