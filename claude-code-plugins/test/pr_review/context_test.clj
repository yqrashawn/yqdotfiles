(ns pr-review.context-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [pr-review.context :as context]))

(defn- tmp-repo []
  (let [d (str (fs/create-temp-dir {:prefix "pr-review-ctx"}))]
    (fs/create-dirs (str d "/.git"))
    d))

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
  (let [r (tmp-repo)
        res (context/build! r {:pr 370 :sha "headsha" :base-ref "main"}
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

(deftest diff-path-is-namespaced-by-sha
  (let [r (tmp-repo)
        res (context/build! r {:pr 370 :sha "abc123" :base-ref "main"}
                            {:merge-base-fn (constantly "b") :diff-fn (constantly "d")
                             :sh stub-sh})]
    (is (= (str (context/context-dir r) "/abc123.diff") (:diff-path res))
        "one file per SHA so a superseded reviewer's diff is never overwritten
         under it mid-read")))

(deftest changed-files-are-extracted-from-the-diff
  (let [r (tmp-repo)
        res (context/build! r {:pr 1 :sha "s" :base-ref "main"}
                            {:merge-base-fn (constantly "b")
                             :diff-fn (constantly sample-diff)
                             :sh stub-sh})]
    (is (= ["src/a.clj" "test/b_test.clj"] (:changed-files res)))))

(deftest missing-merge-base-falls-back-to-the-base-ref
  (let [r (tmp-repo)
        res (context/build! r {:pr 1 :sha "s" :base-ref "main"}
                            {:merge-base-fn (constantly nil)
                             :diff-fn (constantly "d") :sh stub-sh})]
    (is (= "origin/main" (:base res))
        "an unfetched base must still produce a reviewable range, not nil")))

(deftest empty-diff-is-reported-not-hidden
  (let [r (tmp-repo)
        res (context/build! r {:pr 1 :sha "s" :base-ref "main"}
                            {:merge-base-fn (constantly "b")
                             :diff-fn (constantly "") :sh stub-sh})]
    (is (= 0 (:diff-bytes res)))
    (is (= [] (:changed-files res)))
    (testing "the file still exists so the prompt never names a missing path"
      (is (fs/exists? (:diff-path res))))))

(deftest prune-keeps-the-newest-contexts
  (let [r (tmp-repo)]
    (doseq [n ["a" "b" "c" "d"]]
      (context/build! r {:pr 1 :sha n :base-ref "main"}
                      {:merge-base-fn (constantly "b") :diff-fn (constantly "d")
                       :sh stub-sh})
      (Thread/sleep 5))
    (is (= 2 (context/prune! r 2)))
    (is (= 2 (count (fs/glob (context/context-dir r) "*.diff"))))))
