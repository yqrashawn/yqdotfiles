(ns pr-review.prompt-test
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [pr-review.prompt :as prompt]))

(defn- tmp-repo
  "Returns [repo-root git-dir]. The overlay is repo-root-relative
   (`.claude/pr-review.md`, a tracked file); the hint is git-dir-relative, so
   it is reachable from a linked worktree where `<repo-root>/.git` is a file."
  []
  (let [d (str (fs/create-temp-dir {:prefix "pr-review-prompt"}))
        g (str d "/.git")]
    (fs/create-dirs g)
    [d g]))

(def ^:private ctx
  {:diff-path "/r/.git/pr-review-context/abc.diff"
   :changed-files ["src/a.clj" "test/b_test.clj"]
   :base "basesha" :sha "abc" :diff-bytes 1234})

(defn- base-args [[repo git-dir]]
  {:core "CORE_TEXT" :repo-root repo :git-dir git-dir :ctx ctx :pr 370 :pass 1
   :draft? false :prior-fingerprints []})

(deftest core-is-always-included
  (is (str/includes? (prompt/build (base-args (tmp-repo))) "CORE_TEXT")))

(deftest prompt-names-the-diff-file-and-repo-root
  (let [[r g :as repo] (tmp-repo)
        out (prompt/build (base-args repo))]
    (is (str/includes? out (:diff-path ctx)))
    (is (str/includes? out r))
    (is (str/includes? out "src/a.clj"))
    (is (str/includes? out "PR NUMBER: 370"))
    (is (str/includes? out "BASE: basesha"))
    (is (str/includes? out "HEAD: abc"))))

(deftest first-pass-is-labelled-FIRST
  (let [out (prompt/build (base-args (tmp-repo)))]
    (is (str/includes? out "PASS: FIRST"))
    (is (not (str/includes? out "RE-REVIEW")))))

(deftest later-passes-are-labelled-RE-REVIEW
  (let [out (prompt/build (assoc (base-args (tmp-repo)) :pass 3))]
    (is (str/includes? out "PASS: RE-REVIEW"))
    (is (str/includes? out "pass 3")
        "the reviewer must know which pass it is to apply the severity asymmetry")))

(deftest overlay-is-included-when-present
  (let [[r g :as repo] (tmp-repo)]
    (fs/create-dirs (str r "/.claude"))
    (spit (prompt/overlay-path r) "OVERLAY_TEXT")
    (is (str/includes? (prompt/build (base-args repo)) "OVERLAY_TEXT"))))

(deftest missing-overlay-degrades-silently
  (let [out (prompt/build (base-args (tmp-repo)))]
    (is (not (str/includes? out "OVERLAY")))
    (is (str/includes? out "CORE_TEXT")
        "a repo with no .claude/pr-review.md must still get a working review")))

(deftest hint-is-included-and-consumed
  (let [[r g :as repo] (tmp-repo)]
    (spit (prompt/hint-path g) "watch the retry path")
    (let [out (prompt/build (base-args repo))]
      (is (str/includes? out "watch the retry path")))
    (is (not (fs/exists? (prompt/hint-path g)))
        "a hint is for one review; leaving it would silently apply to every later pass")))

(deftest draft-status-is-stated
  (is (str/includes? (prompt/build (assoc (base-args (tmp-repo)) :draft? true))
                     "draft")))

(deftest twice-raised-fingerprints-are-listed-as-do-not-re-raise
  (let [out (prompt/build (assoc (base-args (tmp-repo)) :pass 2
                                 :prior-fingerprints ["src/a.clj:12:correctness/followup"]))]
    (is (str/includes? out "src/a.clj:12:correctness/followup"))
    (is (str/includes? out "do not re-raise"))))

(deftest empty-diff-is-called-out
  (let [out (prompt/build (assoc-in (base-args (tmp-repo)) [:ctx :diff-bytes] 0))]
    (is (str/includes? out "empty")
        "an empty diff must be stated, or the reviewer invents findings")))

(deftest the-prompt-points-the-reviewer-at-the-pr
  (testing "the reviewer had no access to the PR description by ANY route: not
            in the diff, not in the prompt, and `gh pr view` was refused by
            the `Bash(gh pr:*)` prefix. It reviewed with no statement of
            intent to check the change against, which is most of what
            [docs-accuracy] is for"
    (let [out (prompt/build {:core "CORE" :repo-root "/r" :git-dir "/r/.git"
                             :ctx {:sha "s" :base "b" :diff-bytes 10
                                   :diff-path "/d" :changed-files ["a"]}
                             :pr 401 :pass 1
                             :pr-url "https://github.com/yqrashawn/cchp/pull/401"})]
      (is (str/includes? out "https://github.com/yqrashawn/cchp/pull/401"))
      (testing "with the exact command, repo included: gh cannot infer the
                remote from a detached worktree"
        (is (str/includes? out "gh pr view 401 --repo yqrashawn/cchp --comments")))
      (testing "framed as a claim to CHECK — the description is written by the
                agent under review"
        (is (str/includes? out "not ground truth"))
        (is (str/includes? out "[docs-accuracy]")))
      (testing "and before the diff instructions, so it knows the claim first"
        (is (< (str/index-of out "author says this change does")
               (str/index-of out "How to read the change")))))))

(deftest a-pr-with-no-url-adds-no-section
  ;; An empty heading would read as "the author said nothing", which is a
  ;; different claim from "this was not available".
  (let [out (prompt/build {:core "CORE" :repo-root "/r" :git-dir "/r/.git"
                           :ctx {:sha "s" :base "b" :diff-bytes 10
                                 :diff-path "/d" :changed-files []}
                           :pr 1 :pass 1})]
    (is (not (str/includes? out "author says this change does")))))
