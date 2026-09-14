(ns pr-review.context-test
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [pr-review.context :as context]))

(defn- tmp-repo
  "Returns [repo-root git-dir]. They are separate arguments to `build!` on
   purpose: git runs in the work tree, the context is written under the
   clone's shared git directory, and in a linked worktree those differ."
  []
  (let [d (str (fs/create-temp-dir {:prefix "pr-review-ctx"}))
        g (str d "/.git")]
    (fs/create-dirs g)
    [d g]))

(def ^:private sample-diff
  (str "diff --git a/src/a.clj b/src/a.clj\n"
       "index 111..222 100644\n"
       "--- a/src/a.clj\n"
       "+++ b/src/a.clj\n"
       "@@ -1 +1 @@\n"
       "-(def a 1)\n"
       "+(def a 2)\n"
       "diff --git a/test/b_test.clj b/test/b_test.clj\n"
       "new file mode 100644\n"
       "--- /dev/null\n"
       "+++ b/test/b_test.clj\n"
       "@@ -0,0 +1 @@\n"
       "+(ns b-test)\n"))

(defn- stub-sh [_args _dir] {:exit 0 :out "" :err ""})

(deftest build-writes-the-full-diff-to-disk
  (let [[r g] (tmp-repo)
        res (context/build! r g {:pr 370 :sha "headsha" :base-ref "main"}
                            {:merge-base-fn (constantly "basesha")
                             :diff-fn (constantly sample-diff)
                             :sh stub-sh})]
    (is (= "basesha" (:base res)))
    (is (= "headsha" (:sha res)))
    (is (fs/exists? (:diff-path res)))
    (is (= sample-diff (slurp (:diff-path res)))
        "the diff on disk must be byte-identical to git's output — the whole
         point of precomputing it is that B never runs a truncating rtk git diff")
    (is (= (count sample-diff) (:diff-bytes res)))))

(deftest diff-path-is-namespaced-by-pr-and-sha
  (testing "one file per PR AND sha. Keyed by sha alone the safety property the
            name claims -- a reviewer's diff is never overwritten under it
            mid-read -- was false in exactly the case that matters: two open PRs
            can share a head sha, and since the lock became per PR their reviews
            run concurrently, so the second build! replaced the file the first
            reviewer had been pointed at. It then read the other PR's diff while
            its own prompt still listed this PR's changed files.

            The old test passed :pr 370 into build!, which ignored it."
    (let [[r g] (tmp-repo)
          one (context/build! r g {:pr 370 :sha "abc123" :base-ref "main"}
                              {:merge-base-fn (constantly "b")
                               :diff-fn (constantly "diff for 370")
                               :sh stub-sh})
          two (context/build! r g {:pr 402 :sha "abc123" :base-ref "main"}
                              {:merge-base-fn (constantly "b")
                               :diff-fn (constantly "diff for 402")
                               :sh stub-sh})]
      (is (= (str (context/context-dir g) "/370-abc123.diff") (:diff-path one)))
      (is (not= (:diff-path one) (:diff-path two))
          "two open PRs at one sha must not share a file")
      (is (= "diff for 370" (slurp (:diff-path one)))
          "and the first reviewer's diff must still be its own after the second
           review starts"))))

(deftest changed-files-are-extracted-from-the-diff
  (let [[r g] (tmp-repo)
        res (context/build! r g {:pr 1 :sha "s" :base-ref "main"}
                            {:merge-base-fn (constantly "b")
                             :diff-fn (constantly sample-diff)
                             :sh stub-sh})]
    (is (= ["src/a.clj" "test/b_test.clj"] (:changed-files res)))))

(deftest missing-merge-base-falls-back-to-the-base-ref
  (let [[r g] (tmp-repo)
        res (context/build! r g {:pr 1 :sha "s" :base-ref "main"}
                            {:merge-base-fn (constantly nil)
                             :diff-fn (constantly "d") :sh stub-sh})]
    (is (= "origin/main" (:base res))
        "an unfetched base must still produce a reviewable range, not nil")))

(deftest empty-diff-is-reported-not-hidden
  (let [[r g] (tmp-repo)
        res (context/build! r g {:pr 1 :sha "s" :base-ref "main"}
                            {:merge-base-fn (constantly "b")
                             :diff-fn (constantly "") :sh stub-sh})]
    (is (= 0 (:diff-bytes res)))
    (is (= [] (:changed-files res)))
    (testing "the file still exists so the prompt never names a missing path"
      (is (fs/exists? (:diff-path res))))))

(deftest prune-keeps-the-newest-contexts
  (let [[r g] (tmp-repo)]
    (doseq [n ["a" "b" "c" "d"]]
      (context/build! r g {:pr 1 :sha n :base-ref "main"}
                      {:merge-base-fn (constantly "b") :diff-fn (constantly "d")
                       :sh stub-sh})
      (Thread/sleep 5))
    (is (= 2 (context/prune! g 1 2)))
    (is (= 2 (count (fs/glob (context/context-dir g) "*.diff"))))))

(deftest prune-does-not-reach-another-prs-context
  (testing "pruning across the whole clone let a review that FINISHED delete
            the diff file a concurrently running review's prompt names -- the
            reviewer then reads a path that no longer exists. Five passes on a
            busy PR is enough to push another PR's live file out of the window."
    (let [[r g] (tmp-repo)
          o {:merge-base-fn (constantly "b") :diff-fn (constantly "d")
             :sh stub-sh}
          live (:diff-path (context/build! r g {:pr 402 :sha "live" :base-ref "main"} o))]
      (doseq [n ["a" "b" "c" "d" "e" "f"]]
        (context/build! r g {:pr 370 :sha n :base-ref "main"} o)
        (Thread/sleep 5))
      (is (= 1 (context/prune! g 370 5)))
      (is (fs/exists? live)
          "PR 402's running review must still have the file its prompt names"))))

(deftest build-through-real-defaults-keeps-real-diff-bytes
  (let [[r g] (tmp-repo)
        payload "diff --git a/café.clj b/café.clj\n@@ -1 +1 @@\n-(def café 1)\n+(def café 2)\n"
        responses {["git" "merge-base"] {:exit 0 :out "basesha\n" :err ""}
                   ;; gh/diff now runs `git --no-pager diff --no-ext-diff ...`
                   ["git" "--no-pager"]  {:exit 0 :out payload :err ""}}
        sh (fn [args _dir]
             (get responses (vec (take 2 args)) {:exit 1 :out "" :err "unstubbed"}))
        res (context/build! r g {:pr 1 :sha "s" :base-ref "main"} {:sh sh})
        on-disk (fs/size (:diff-path res))]
    (testing "no :diff-fn or :merge-base-fn override, so build! runs through
              gh/merge-base and gh/diff's real default wiring"
      (is (= payload (slurp (:diff-path res)))
          "the file on disk must be byte-identical to git's raw output,
           trailing newline included — a :diff-fn stub would have hidden
           ok-out silently trimming it")
      (is (= on-disk (:diff-bytes res)))
      (is (> on-disk (count payload))
          ":diff-bytes must count UTF-8 bytes on disk, not UTF-16 code units,
           or a non-ASCII diff would under-report its own size"))))

(deftest build-flags-a-failed-diff
  (let [[r g] (tmp-repo)
        res (context/build! r g {:pr 1 :sha "s" :base-ref "main"}
                            {:merge-base-fn (constantly nil)
                             :diff-fn (constantly nil)
                             :sh stub-sh})]
    (is (true? (:diff-failed? res))
        "a nil from diff-fn means the diff command itself failed — an
         unresolved base ref, most often — and must be flagged, not
         silently written as an empty file that reads as a clean pass")
    (is (fs/exists? (:diff-path res))
        "the file is still written even on failure, so nothing downstream
         ever names a missing path")
    (is (= 0 (:diff-bytes res)))))

(deftest build-does-not-flag-a-legitimately-empty-diff
  (let [[r g] (tmp-repo)
        res (context/build! r g {:pr 1 :sha "s" :base-ref "main"}
                            {:merge-base-fn (constantly "b")
                             :diff-fn (constantly "")
                             :sh stub-sh})]
    (is (false? (:diff-failed? res))
        "a real empty diff (exit 0, no output) is not a failure — flagging
         it too would make the flag meaningless")))

;; ------------------------------------------------ the re-review increment

(deftest a-first-pass-writes-only-the-full-diff
  (let [[r g] (tmp-repo)
        c (context/build! r g {:pr 478 :sha "s1" :base-ref "main"}
                          {:merge-base-fn (constantly "b") :diff-fn (constantly "D")})]
    (is (nil? (:incr-path c)))
    (is (nil? (:incr-bytes c)))
    (is (fs/exists? (:diff-path c)))))

(deftest a-re-review-also-writes-the-diff-since-the-last-pass
  (testing "measured on PR #478: ten passes on a 1801-line change, each finding
            a real defect in the PREVIOUS pass's fix. The next defect lives in
            the new commits, and the full diff for that pass was 100292 bytes
            against 2092 for the increment — a 48x difference in what has to be
            read to reach it"
    (let [[r g] (tmp-repo)
          seen (atom [])
          c (context/build! r g {:pr 478 :sha "s2" :base-ref "main" :since-sha "s1"}
                            {:merge-base-fn (constantly "b")
                             :diff-fn (fn [base sha]
                                        (swap! seen conj [base sha])
                                        (if (= base "s1") "SMALL" "FULLFULLFULL"))})]
      (is (= [["b" "s2"] ["s1" "s2"]] @seen)
          "the full diff is taken from the merge base, the increment from the
           previously reviewed sha")
      (is (= "FULLFULLFULL" (slurp (:diff-path c))))
      (is (= "SMALL" (slurp (:incr-path c))))
      (is (= 5 (:incr-bytes c)))
      (is (= "s1" (:since-sha c)))
      (is (str/includes? (str (:incr-path c)) "since-s1")
          "the base is in the name, so two passes cannot collide"))))

(deftest a-re-push-of-the-same-sha-writes-no-increment
  ;; since-sha = sha means nothing changed; an empty increment would read as
  ;; "nothing to review" rather than "this is the whole change".
  (let [[r g] (tmp-repo)
        c (context/build! r g {:pr 478 :sha "s1" :base-ref "main" :since-sha "s1"}
                          {:merge-base-fn (constantly "b") :diff-fn (constantly "D")})]
    (is (nil? (:incr-path c)))))

(deftest prune-keeps-whole-passes-not-whole-files
  (testing "a re-review writes TWO files for one pass. Counting files made
            `keep` mean two and a half passes, and with a small keep it deleted
            the FULL diff while keeping the increment — the one the prompt
            tells the reviewer to open when verifying closure"
    (let [[_ g] (tmp-repo)
          d (context/context-dir g)]
      (fs/create-dirs d)
      (doseq [n ["478-aaa.diff" "478-aaa.since-zzz.diff"
                 "478-bbb.diff" "478-bbb.since-aaa.diff"
                 "479-ccc.diff"]]
        (spit (str d "/" n) "x")
        (Thread/sleep 5))
      (is (= 2 (context/prune! g 478 1)))
      (let [left (set (map (comp str fs/file-name) (fs/list-dir d)))]
        (is (contains? left "478-bbb.diff") "the newest pass's full diff must survive")
        (is (contains? left "478-bbb.since-aaa.diff") "and its increment")
        (is (not (contains? left "478-aaa.diff")))
        (is (contains? left "479-ccc.diff") "another PR is never touched")))))

