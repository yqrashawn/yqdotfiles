(ns pr-review.workdir-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [pr-review.workdir :as workdir]))

(defn- repo
  "A directory that looks like a clone to `resolve-dir`, which stats for a
   `.git` entry and nothing more."
  []
  (let [d (str (fs/create-temp-dir {:prefix "pr-review-workdir"}))]
    (fs/create-dirs (str d "/.git"))
    d))

(defn- dir-of [command fallback] (:dir (workdir/resolve-dir command fallback)))
(defn- basis-of [command fallback] (:basis (workdir/resolve-dir command fallback)))

(deftest a-command-that-never-moves-leaves-the-payload-directory-alone
  (testing "the payload cwd is genuinely correct for a push issued where the
            session lives, which is most of them — this must stay :explicit
            so the caller does not warn about a directory it resolved fine"
    (let [r (repo)]
      (is (= {:dir r :basis :explicit}
             (workdir/resolve-dir "git push -u origin feat/x" r))))))

(deftest an-absolute-cd-wins-over-the-payload-directory
  (let [r  (repo)
        wt (str r "/wt")]
    (fs/create-dirs wt)
    (is (= wt (dir-of (str "cd " wt " && git push") r))
        "the push ran in the directory the command moved to, not the one the
         session happens to sit in")))

(deftest the-shape-that-exposed-the-defect-a-variable-then-a-cd-into-a-worktree
  (testing "the real command, measured: session cwd on branch
            docs/mydeck-design with no open PR, the worktree it pushed from on
            a branch with open PR #391. Resolving from the payload cwd found
            nothing, exited 0, and a real PR went unreviewed"
    (let [r  (repo)
          wt (str r "/wt-sse-deadline-abandon")]
      (fs/create-dirs wt)
      (is (= wt (dir-of
                 (str "SP=" r "; cd \"$SP/wt-sse-deadline-abandon\""
                      " && git push -u origin"
                      " fix/llm-logs-sse-deadline-and-abandoned-tabs 2>&1"
                      " | tail -2; grep -aE 'a;b' /dev/null")
                 r))
          "a `VAR=path` assignment, a quoted `$VAR` expansion, a pipeline and
           a trailing segment whose grep pattern contains a `;` — splitting
           through the quote or dropping the assignment both lose the
           worktree"))))

(deftest a-chain-of-assignments-resolves-in-order
  (testing "the second real shape, measured on the PR #395 push: `SP=<path>`
            on its own line, then `WT=\"$SP/wt-...\"`, then `cd \"$WT\"`.
            Treating a non-literal value as unresolvable stopped at WT, the
            directory fell back to the session cwd on a branch with no PR, and
            the push went unreviewed"
    (let [r  (repo)
          wt (str r "/wt-388-followup")]
      (fs/create-dirs wt)
      (is (= wt (dir-of
                 (str "SP=" r "\nWT=\"$SP/wt-388-followup\"; cd \"$WT\""
                      " && git add -A && git push -q origin HEAD")
                 r))
          "WT is one level removed from a literal, not two")
      (is (= wt (dir-of
                 (str "A=" r "; B=\"$A/wt\"; C=\"$B\"; D=\"$C-388-followup\";"
                      " cd \"$D\" && git push")
                 r))
          "chaining is unbounded because the reduce runs in command order"))))

(deftest an-unresolvable-link-in-the-chain-stays-unresolvable
  (testing "the safety property the chaining must not cost: a shell expands an
            unset variable to the empty string, so a name that cannot be
            resolved must poison everything derived from it rather than
            silently shortening the path"
    (let [r (repo)]
      (is (= r (dir-of "WT=\"$NOPE/wt\"; cd \"$WT\" && git push" r))
          "an unset base must not collapse to /wt")
      (is (= r (dir-of (str "SP=$(pwd); WT=\"$SP/wt\"; cd \"$WT\" && git push") r))
          "a command substitution must not become resolvable by being assigned")
      (is (= r (dir-of (str "SP=" r "; WT=\"$SP/$OTHER\"; cd \"$WT\" && git push") r))
          "one resolvable reference does not make the whole value resolvable")
      ;; Expansion succeeding is not the same as the RESULT being a path. The
      ;; substituted name is literal by construction, but the rest of the
      ;; value is not, so the expanded string has to be re-checked.
      (is (= r (dir-of (str "SP=" r "; WT=\"$SP/wt-*\"; cd \"$WT\" && git push") r))
          "a glob survived expansion and was taken for a path")
      (is (= r (dir-of (str "SP=" r "; WT=\"$SP/`date`\"; cd \"$WT\" && git push") r))
          "a backtick survived expansion and was taken for a path"))))

(deftest the-last-cd-wins-and-relative-targets-resolve-against-the-current-one
  (let [r (repo)]
    (fs/create-dirs (str r "/a/b"))
    (is (= (str r "/a/b") (dir-of "cd a && cd b && git push" r))
        "each cd moves from wherever the previous one landed; reading only
         the first would name a directory the push never ran in")))

(deftest git-dash-c-names-the-directory
  (let [r (repo)]
    (fs/create-dirs (str r "/a"))
    (is (= (str r "/a") (dir-of (str "git -C " r "/a push") r))
        "`git -C <path>` moves the push as surely as a cd does")))

(deftest a-command-substitution-is-ambiguous-never-a-guess
  (let [r (repo)]
    (is (= :ambiguous (basis-of "cd \"$(pwd)/x\" && git push" r))
        "the directory is real but unknowable from the string alone")
    (is (= r (dir-of "cd \"$(pwd)/x\" && git push" r))
        "and the fallback is reported unchanged — reviewing the wrong
         repository is worse than staying silent")))

(deftest a-variable-the-command-never-assigned-is-ambiguous
  (testing "a shell expands an unset variable to the empty string, so
            `cd \"$UNSET/x\"` would become `cd /x`. Copying that would point
            the review at a directory nobody named"
    (let [r (repo)]
      (is (= {:dir r :basis :ambiguous}
             (workdir/resolve-dir "cd \"$UNSET/x\" && git push" r))))))

(deftest the-process-environment-is-never-consulted
  (testing "$HOME is set in this hook process and says nothing about the
            shell the tool call ran in — the two are different processes, and
            an agent's `cd \"$HOME/x\"` may have used a HOME this process
            does not have"
    (let [r (repo)]
      (is (some? (System/getenv "HOME")) "fixture precondition")
      (is (= :ambiguous (basis-of "cd \"$HOME/x\" && git push" r))))))

(deftest a-directory-that-does-not-exist-falls-back
  (let [r (repo)]
    (is (= {:dir r :basis :fallback}
           (workdir/resolve-dir "cd /definitely/not/there && git push" r))
        "a path this process cannot stat is not where a push succeeded; the
         payload cwd is the better guess, and the caller is told it guessed")
    (is (= {:dir r :basis :fallback}
           (workdir/resolve-dir (str "cd " r "/not/there && git push") r))
        "including a missing path *inside* a real repository, which the repo
         check alone would wave through")))

(deftest a-real-directory-outside-any-git-repo-falls-back
  (testing "resolving to a directory with no repository above it would make
            every later git call fail, so the resolution is wrong even though
            the parse was right"
    (let [r    (repo)
          bare (str (fs/create-temp-dir {:prefix "pr-review-workdir-bare"}))]
      (is (not (fs/exists? (str bare "/.git"))) "fixture precondition")
      (is (= {:dir r :basis :fallback}
             (workdir/resolve-dir (str "cd " bare " && git push") r))))))
