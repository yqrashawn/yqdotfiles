(ns pr-review.hooksjson-test
  "Asserts the hook manifest's contract against the documented harness
   behaviour, because nothing else does and its defaults are invisible."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def ^:private manifest
  (delay
    (json/parse-string
     (slurp (str (fs/path (or (some-> (System/getProperty "babashka.config") fs/parent str)
                              (System/getProperty "user.dir"))
                          "hooks" "hooks.json")))
     true)))

(defn- post-tool-use [] (mapcat :hooks (get-in @manifest [:hooks :PostToolUse])))

(deftest every-review-trigger-sets-an-explicit-timeout
  (testing "Claude Code enforces `timeout` on an asyncRewake hook — the docs
            say so explicitly, unlike a plain async hook — and the default for
            a command hook is 600s. Reviews measured 390-497s from context
            diff to ledger row, plus setup, and they grow because the reviewer
            runs test suites. Leaving it unset put every review at 65-90% of a
            budget nobody had chosen"
    (let [hs (post-tool-use)]
      (is (seq hs))
      (doseq [h hs]
        (is (number? (:timeout h))
            (str "no explicit timeout on " (:if h) " — the 600s default applies"))
        (is (> (:timeout h) 600)
            "the point is to be above the default, not to restate it")))))

(deftest the-trigger-still-wakes-rather-than-only-reporting
  (testing "asyncRewake, not async: a plain async hook's output is delivered
            on the next conversation turn and waits if the session is idle.
            Only an asyncRewake hook exiting 2 wakes an idle session, which is
            the whole delivery mechanism this loop depends on"
    (doseq [h (post-tool-use)]
      (is (true? (:asyncRewake h)))
      (is (not (:async h)) "plain async would never wake an idle session"))))

(deftest the-skill-and-the-wake-agree-on-what-each-category-obliges
  (testing "A reads both. They disagreed: SKILL.md said to defer
            [docs-accuracy] and [style] 'until the PR is otherwise mergeable'
            and never mentioned [coverage] at all, while the wake said all
            four go to a follow-up PR. The unstated coverage disposition is
            why PR #410 kept fixing coverage nits and pushing instead of
            merging"
    (let [skill (slurp (str (fs/path (or (some-> (System/getProperty "babashka.config") fs/parent str)
                                         (System/getProperty "user.dir"))
                                     "skills" "pr-review-loop" "SKILL.md")))]
      (is (str/includes? skill "Only **correctness** findings are work you owe"))
      (is (str/includes? skill "[coverage]") "coverage must have a stated disposition")
      (is (str/includes? skill "your judgment"))
      (is (str/includes? skill "No follow-up PR is owed"))
      (is (str/includes? skill "does not actually check")
          "the carve-out has to survive in the skill too")
      (testing "and the skill must not still claim the reviewer has no shell"
        (is (not (str/includes? skill "No shell")))
        (is (str/includes? skill "and `Bash`")
            "A was told the reviewer could not run tests while its findings
             said 'Verified by RUNNING'")))))
