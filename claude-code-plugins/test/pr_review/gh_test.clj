(ns pr-review.gh-test
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.test :refer [deftest is testing]]
            [pr-review.gh :as gh]))

(defn- stub
  "A :sh replacement that answers from `responses`, keyed by the first two
   argv elements, and records every invocation in `calls`."
  [responses calls]
  (fn [args _dir]
    (swap! calls conj args)
    (get responses (vec (take 2 args))
         {:exit 1 :out "" :err "unstubbed"})))

(deftest repo-root-uses-rev-parse
  (let [calls (atom [])
        sh (stub {["git" "rev-parse"] {:exit 0 :out "/repo\n" :err ""}} calls)]
    (is (= "/repo" (gh/repo-root "/repo/sub" {:sh sh})))
    (is (= ["git" "rev-parse" "--show-toplevel"] (first @calls)))))

(deftest repo-root-is-nil-outside-a-repo
  (let [sh (fn [_ _] {:exit 128 :out "" :err "not a git repository"})]
    (is (nil? (gh/repo-root "/tmp" {:sh sh})))))

(deftest current-branch-trims-output
  (let [sh (stub {["git" "rev-parse"] {:exit 0 :out "feat/x\n" :err ""}} (atom []))]
    (is (= "feat/x" (gh/current-branch "/repo" {:sh sh})))))

(deftest detached-head-has-no-branch
  (let [sh (fn [_ _] {:exit 0 :out "HEAD\n" :err ""})]
    (is (nil? (gh/current-branch "/repo" {:sh sh}))
        "a detached HEAD has no branch, so there is no PR to look up")))

(deftest open-pr-parses-the-first-match
  (let [calls (atom [])
        sh (stub {["gh" "pr"]
                  {:exit 0
                   :out "[{\"number\":370,\"isDraft\":true,\"baseRefName\":\"main\"}]"
                   :err ""}}
                 calls)]
    (is (= {:number 370 :isDraft true :baseRefName "main"}
           (gh/open-pr "/repo" "feat/x" {:sh sh})))
    (testing "the query is scoped to the branch and to open PRs"
      (let [argv (first @calls)]
        (is (some #{"--head"} argv))
        (is (some #{"feat/x"} argv))
        (is (some #{"--state"} argv))
        (is (some #{"open"} argv))))))

(deftest no-open-pr-returns-nil
  (let [sh (stub {["gh" "pr"] {:exit 0 :out "[]" :err ""}} (atom []))]
    (is (nil? (gh/open-pr "/repo" "feat/x" {:sh sh}))
        "a push to a branch with no open PR must be silent, not an error")))

(deftest gh-failure-returns-nil-rather-than-throwing
  (let [sh (fn [_ _] {:exit 1 :out "" :err "gh: not authenticated"})]
    (is (nil? (gh/open-pr "/repo" "feat/x" {:sh sh}))
        "an unauthenticated gh must degrade to silence, never crash the hook")))

(deftest diff-asks-git-for-a-plain-unified-three-dot-diff
  (let [calls (atom [])
        sh (stub {["git" "--no-pager"] {:exit 0 :out "DIFFTEXT" :err ""}} calls)]
    (is (= "DIFFTEXT" (gh/diff "/repo" "base1" "head1" {:sh sh})))
    (is (= ["git" "--no-pager" "diff" "--no-ext-diff" "--no-color" "base1...head1"]
           (first @calls)))
    (testing "three-dot compares against the merge base, which is what a
              review wants"
      (is (some #{"base1...head1"} (first @calls))))
    (testing "--no-ext-diff, or the user's git decides the format. This repo
              sets diff.external to difftastic side-by-side, and every review
              before this flag read that instead of a unified diff: the real
              context diff for PR #395 held 0 `diff --git` lines, so
              context/changed-files was always empty and the prompt's
              \"Changed files\" section always blank"
      (is (some #{"--no-ext-diff"} (first @calls))))))

(deftest diff-distinguishes-a-failure-from-a-real-empty-diff
  (is (nil? (gh/diff "/repo" "origin/main" "headsha"
                      {:sh (fn [_ _] {:exit 1 :out ""
                                      :err "fatal: bad revision 'origin/main'"})}))
      "a non-zero exit — e.g. an unresolved base ref — must come back as nil,
       never as \"\", or a failed diff looks exactly like a real empty one")
  (is (= "" (gh/diff "/repo" "origin/main" "headsha"
                      {:sh (fn [_ _] {:exit 0 :out "" :err ""})}))
      "a zero exit with no output is a genuinely empty diff and must still
       come back as \"\", not nil"))

(deftest git-common-dir-resolves-a-relative-answer-against-the-repo-root
  (let [calls (atom [])
        sh (stub {["git" "rev-parse"] {:exit 0 :out ".git\n" :err ""}} calls)]
    (is (= "/repo/.git" (gh/git-common-dir "/repo" {:sh sh}))
        "git answers relatively in an ordinary clone, and a relative path
         would be resolved against the hook's cwd, not the repo")
    (is (= ["git" "rev-parse" "--git-common-dir"] (first @calls))
        "--git-common-dir, not --git-dir: all worktrees of one clone must
         share one ledger and one lock, and --git-dir gives each worktree its
         own private directory")))

(deftest git-common-dir-keeps-an-absolute-answer
  (let [sh (stub {["git" "rev-parse"] {:exit 0 :out "/main/.git\n" :err ""}} (atom []))]
    (is (= "/main/.git" (gh/git-common-dir "/wt" {:sh sh}))
        "inside a worktree git answers with the main clone's .git; joining
         that onto the worktree root would invent a path that does not exist")))

(deftest git-common-dir-is-nil-when-git-fails
  (is (nil? (gh/git-common-dir "/repo" {:sh (fn [_ _] {:exit 128 :out "" :err "no"})}))
      "callers fall back to <repo-root>/.git, so a failure must be nil rather
       than a throw out of the hook"))

(deftest git-common-dir-in-a-real-worktree-points-at-the-main-clone
  (testing "the fixture no test had: a repo root whose .git is a FILE.
            fs/create-dirs on it throws FileAlreadyExistsException, which is
            what made every push from a worktree exit 1"
    (let [tmp  (str (fs/create-temp-dir {:prefix "pr-review-gh-wt"}))
          main (str tmp "/main")
          wt   (str tmp "/wt")
          git! (fn [dir & args]
                 (let [{:keys [exit err]} (p/sh (into ["git"] args) {:dir dir})]
                   (when-not (zero? exit)
                     (throw (ex-info (str "fixture git failed: " args " " err) {})))))]
      (fs/create-dirs main)
      (git! main "init" "-q")
      (git! main "config" "user.email" "t@t.t")
      (git! main "config" "user.name" "t")
      (spit (str main "/f") "hi")
      (git! main "add" "f")
      (git! main "commit" "-qm" "init")
      (git! main "worktree" "add" "-q" wt "-b" "feat")
      (is (fs/regular-file? (str wt "/.git"))
          "fixture precondition: a linked worktree's .git is a file, not a
           directory")
      (is (fs/directory? (gh/git-common-dir wt {}))
          "the resolved git dir must be a real directory, or every ledger and
           lock write under it fails")
      (is (= (str (fs/real-path (str main "/.git")))
             (str (fs/real-path (gh/git-common-dir wt {}))))
          "a worktree must resolve to the main clone's .git so both share one
           ledger, one lock and one context directory"))))
