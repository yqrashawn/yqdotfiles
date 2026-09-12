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

(defn- repo-slug
  "`owner/repo` out of a PR url, so the reviewer's `gh` call works from a
   worktree whose remote gh cannot infer."
  [url]
  (or (second (re-find #"github\.com/([^/]+/[^/]+)/pull/" (str url))) ""))

(defn- section
  [title body]
  (when-not (str/blank? (str body))
    (str "\n## " title "\n\n" body "\n")))

(defn build
  [{:keys [core repo-root git-dir ctx pr pass draft? prior-fingerprints pr-url
           verdict-tag]}]
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
     ;; The URL rather than a copy of the description. The reviewer has
     ;; `gh pr view`, so pasting the text in would be a second and staler
     ;; source of it -- and it can read the comments too, which a paste
     ;; cannot carry. Placed before the diff instructions so the reviewer
     ;; knows what the change CLAIMS before it starts reading what it does.
     (section "What the author says this change does"
              (when (seq (str pr-url))
                (str "Read the description and the comments before the diff:\n\n"
                     "    gh pr view " pr " --repo " (repo-slug pr-url)
                     " --comments\n\n"
                     pr-url "\n\n"
                     "That is the author's claim, not a finding and not ground"
                     " truth. Check the diff against it: a claim the change"
                     " does not deliver, or scope the body says was deferred"
                     " and was not, is a [docs-accuracy] finding.")))
     ;; Before "How to read the change", so the reviewer has the exact line it
     ;; must end on before it starts reading anything.
     (when (seq (str verdict-tag))
       (section "Your verdict line"
         (str "Write your verdict at column 0, exactly once, with THIS pass's"
              " tag:\n\n"
              "    VERDICT[" verdict-tag "]: MERGEABLE -- N follow-ups to file\n"
              "    VERDICT[" verdict-tag "]: NOT MERGEABLE -- <shortest statement"
              " of the blocking finding>\n\n"
              "Those two are indented because they are examples; yours is not"
              " indented.\n\n"
              "The count block goes directly below it, as the core prompt"
              " describes.\n\n"
              "The tag is minted for this pass and never reused, and it is"
              " REQUIRED: a reply\nwith no tagged verdict at column 0 is"
              " discarded whole and the review re-run.\nIt is the only thing"
              " that separates your answer from a previous pass's\nverdict,"
              " which you may quote while verifying closure -- put those inside"
              " a\n``` fence, which is never parsed. Never write this tag on"
              " any line but your\nown final verdict."))) 
     (section "How to read the change"
              (str "The complete, untruncated diff is on disk. Read it first:\n\n"
                   "    " (:diff-path ctx) "\n\n"
                   "Then read the surrounding source under the repo root for context.\n"
                   "You have Read, Grep, Glob and Bash, in a throwaway worktree checked\n"
                   "out at the commit under review and deleted when this review ends.\n"
                   "Inspect the change however is useful. `git push`, `git commit`\n"
                   "and every `gh` subcommand that changes the PR are refused;\n"
                   "read-only `gh pr view` is not. Your findings are the output and\n"
                   "the author decides what happens to the PR."))
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
