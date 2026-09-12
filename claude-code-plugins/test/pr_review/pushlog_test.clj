(ns pr-review.pushlog-test
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [pr-review.pushlog :as pushlog]))

(defn- sha [c] (str/join (repeat 40 c)))

(defn- git-dir
  "A directory holding just the reflog tree `pushes-since` reads."
  []
  (str (fs/create-temp-dir {:prefix "pr-review-pushlog"})))

(defn- reflog!
  "Writes reflog lines for `remote/branch`. Each entry is [old new ts-seconds
   message]."
  [gd remote branch entries]
  (let [f (fs/path gd "logs" "refs" "remotes" remote branch)]
    (fs/create-dirs (fs/parent f))
    (spit (str f)
          (str/join "\n"
                    (for [[old new ts msg] entries]
                      (format "%s %s Some One <a@b.c> %d +0800\t%s" old new ts msg))))
    (str f)))

(deftest a-push-line-parses
  (is (= {:old-sha (sha \a) :new-sha (sha \b) :ts 1788929441000
          :message "update by push"}
         (pushlog/parse-line
          (format "%s %s Some One <a@b.c> 1788929441 +0800\tupdate by push"
                  (sha \a) (sha \b)))))
  (testing "seconds are converted to the ledger's milliseconds"
    (is (= 1788929441000 (:ts (pushlog/parse-line
                               (format "%s %s N <a@b> 1788929441 +0800\tupdate by push"
                                       (sha \a) (sha \b))))))))

(deftest lines-that-are-not-reflog-entries-are-nil
  (doseq [[label line] {"empty"            ""
                        "no tab"           (format "%s %s N <a@b> 1 +0800 update by push" (sha \a) (sha \b))
                        "short sha"        "abc def N <a@b> 1 +0800\tupdate by push"
                        "no timestamp"     (format "%s %s N <a@b> +0800\tupdate by push" (sha \a) (sha \b))
                        "prose"            "update by push"}]
    (testing label
      (is (nil? (pushlog/parse-line line))))))

(deftest an-identity-containing-digits-and-spaces-still-parses
  ;; The `.*` before the timestamp is greedy, so a name holding its own
  ;; numbers must not be able to shift which group the timestamp comes from.
  (is (= 1788929441000
         (:ts (pushlog/parse-line
               (format "%s %s Person 2 von 3 <a+1@b.co> 1788929441 +0800\tupdate by push"
                       (sha \a) (sha \b)))))))

(deftest only-pushes-are-returned-never-fetches
  ;; The discriminator the whole namespace rests on: this clone holds 72 push
  ;; entries against 297 fetch entries, and a fetch is not something to review.
  (let [gd (git-dir)]
    (reflog! gd "origin" "topic"
             [[(sha \0) (sha \a) 1000 "update by push"]
              [(sha \a) (sha \b) 2000 "fetch --all: storing ref"]
              [(sha \a) (sha \c) 3000 "fetch origin --quiet: storing ref"]
              [(sha \c) (sha \d) 4000 "fetch --all: fast-forward"]
              [(sha \d) (sha \e) 5000 "update by push"]])
    (is (= [(sha \e) (sha \a)]
           (map :new-sha (pushlog/pushes-since gd 0))))))

(deftest entries-older-than-the-window-are-excluded
  (let [gd (git-dir)]
    (reflog! gd "origin" "topic"
             [[(sha \0) (sha \a) 1000 "update by push"]
              [(sha \a) (sha \b) 5000 "update by push"]])
    (is (= [(sha \b) (sha \a)] (map :new-sha (pushlog/pushes-since gd 0))))
    (is (= [(sha \b)] (map :new-sha (pushlog/pushes-since gd 2000000))))
    (is (empty? (pushlog/pushes-since gd 9000000)))))

(deftest results-are-newest-first-across-branches
  ;; `decide` takes the newest candidate, so the order is part of the contract
  ;; rather than an accident of directory walk order.
  (let [gd (git-dir)]
    (reflog! gd "origin" "older" [[(sha \0) (sha \a) 1000 "update by push"]])
    (reflog! gd "origin" "newest" [[(sha \0) (sha \b) 3000 "update by push"]])
    (reflog! gd "origin" "middle" [[(sha \0) (sha \c) 2000 "update by push"]])
    (is (= ["newest" "middle" "older"] (map :branch (pushlog/pushes-since gd 0))))))

(deftest a-branch-name-with-slashes-keeps-them-and-the-remote-is-one-segment
  ;; Every branch in this workflow is `fix/...`, so splitting the ref wrongly
  ;; would mean no branch ever matched a PR.
  (let [gd (git-dir)]
    (reflog! gd "origin" "fix/llm-logs/deep/name" [[(sha \0) (sha \a) 1000 "update by push"]])
    (reflog! gd "upstream" "fix/other" [[(sha \0) (sha \b) 2000 "update by push"]])
    (is (= #{{:remote "origin" :branch "fix/llm-logs/deep/name"}
             {:remote "upstream" :branch "fix/other"}}
           (set (map #(select-keys % [:remote :branch]) (pushlog/pushes-since gd 0)))))))

(deftest the-remotes-default-branch-symref-is-not-a-push
  ;; `logs/refs/remotes/origin/HEAD` tracks the remote's default branch; it is
  ;; never a push target, and treating it as one would name branch "HEAD".
  (let [gd (git-dir)]
    (reflog! gd "origin" "HEAD" [[(sha \0) (sha \a) 1000 "update by push"]])
    (reflog! gd "origin" "real" [[(sha \0) (sha \b) 1000 "update by push"]])
    (is (= ["real"] (map :branch (pushlog/pushes-since gd 0))))))

(deftest a-missing-reflog-tree-is-empty-not-an-error
  ;; A fresh clone that has never pushed has no logs/refs/remotes at all.
  (is (empty? (pushlog/pushes-since (git-dir) 0))))

(deftest the-two-pushes-the-command-parser-missed
  (testing "verbatim from claude-code-http-proxy: the #395 and #397 pushes that
            the shell-command parser resolved to the wrong repository, and
            which therefore went unreviewed. Both are plainly here"
    (let [gd (git-dir)]
      (reflog! gd "origin" "fix/slack-per-file-hold-attempts"
               [["0000000000000000000000000000000000000000"
                 "44e4bbf94bb87ff581e312f083c0ec852f4845ce" 1788921172 "update by push"]
                ["44e4bbf94bb87ff581e312f083c0ec852f4845ce"
                 "ceab8f69668700036cd8e7c6ebe9705b02437f30" 1788929441 "update by push"]])
      (reflog! gd "origin" "fix/finish-the-monotonic-sweep"
               [["ae9d0a12b5b85615b44b432b477f4bb9b211bba3"
                 "0993247b000000000000000000000000000000aa" 1788925000 "update by push"]])
      (let [ps (pushlog/pushes-since gd 0)]
        (is (= 3 (count ps)))
        (is (= {:remote "origin"
                :branch "fix/slack-per-file-hold-attempts"
                :old-sha "44e4bbf94bb87ff581e312f083c0ec852f4845ce"
                :new-sha "ceab8f69668700036cd8e7c6ebe9705b02437f30"
                :ts 1788929441000}
               (first ps))
            "the newest push is the #395 commit that was never reviewed")
        (is (= "0993247b000000000000000000000000000000aa" (:new-sha (second ps)))
            "and #397's second commit is the next one back")))))
