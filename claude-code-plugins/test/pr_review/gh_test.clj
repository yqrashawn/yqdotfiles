(ns pr-review.gh-test
  (:require [clojure.test :refer [deftest is testing]]
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

(deftest diff-asks-for-the-three-dot-range
  (let [calls (atom [])
        sh (stub {["git" "diff"] {:exit 0 :out "DIFFTEXT" :err ""}} calls)]
    (is (= "DIFFTEXT" (gh/diff "/repo" "base1" "head1" {:sh sh})))
    (is (= ["git" "diff" "base1...head1"] (first @calls))
        "three-dot compares against the merge base, which is what a review wants")))
