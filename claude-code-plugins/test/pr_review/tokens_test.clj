(ns pr-review.tokens-test
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [pr-review.reviewer :as reviewer]
            [pr-review.tokens :as tokens]))

(defn- tmp-state
  "A state path of this test's own. Every stateful test redefs `state-path` to
   one of these: the real one is shared with whatever reviews the machine is
   running, and a test that advanced THAT cursor would rotate a real review's
   token."
  []
  (str (fs/path (fs/create-temp-dir {:prefix "prl-tokens"}) "tokens.edn")))

;;; Where the pool comes from

(deftest an-env-file-is-read-the-way-source-would-read-it
  (testing "the live line wins over the several disabled ones above it —
            which is the shape of the file this actually reads"
    (let [f (str (fs/path (fs/create-temp-dir {:prefix "prl-env"}) ".env.local"))]
      (spit f (str "# personal\n"
                   "# CLAUDE_TOKENS=disabled-1,disabled-2\n"
                   "OTHER=x\n"
                   "CLAUDE_TOKENS=a,b,c\n"))
      (is (= "a,b,c" (tokens/env-file-var f "CLAUDE_TOKENS")))
      (is (= ["a" "b" "c"] (tokens/pool {:env-file f})))))

  (testing "the LAST assignment wins, because `source` runs top to bottom"
    (let [f (str (fs/path (fs/create-temp-dir {:prefix "prl-env"}) ".env.local"))]
      (spit f "CLAUDE_TOKENS=first\nCLAUDE_TOKENS=second\n")
      (is (= "second" (tokens/env-file-var f "CLAUDE_TOKENS")))))

  (testing "quoting and trailing comments, as the shell resolves them"
    (let [f (str (fs/path (fs/create-temp-dir {:prefix "prl-env"}) ".env.local"))]
      (spit f (str "A='x,y'\n"
                   "B=\"p,q\"\n"
                   "C=m,n # jason, 0g\n"
                   "D=z\n"))
      (is (= "x,y" (tokens/env-file-var f "A")) "single quotes come off")
      (is (= "p,q" (tokens/env-file-var f "B")) "double quotes come off")
      (is (= "m,n" (tokens/env-file-var f "C"))
          "an unquoted ` #' starts a comment, or the pool grows a junk token")
      (is (= "z" (tokens/env-file-var f "D")))))

  (testing "a missing file is nil, not a throw: no pool must never be the
            thing that stops a review starting"
    (is (nil? (tokens/env-file-var "/no/such/file" "CLAUDE_TOKENS")))
    (is (= [] (tokens/pool {:env-file "/no/such/file"})))))

(deftest a-blank-pool-is-empty-not-a-one-token-pool
  (testing "`(str/split \"\" #\",\")` is [\"\"] — an empty CLAUDE_TOKENS that
            parsed to one blank token would be handed to the CLI as a
            credential"
    (let [f (str (fs/path (fs/create-temp-dir {:prefix "prl-env"}) ".env.local"))]
      (spit f "CLAUDE_TOKENS=\n")
      (is (= [] (tokens/pool {:env-file f})))))
  (testing "and separators alone are not tokens"
    (let [f (str (fs/path (fs/create-temp-dir {:prefix "prl-env"}) ".env.local"))]
      (spit f "CLAUDE_TOKENS= , ,a, \n")
      (is (= ["a"] (tokens/pool {:env-file f}))))))

;;; Rotation

(deftest consecutive-reviews-do-not-all-run-on-the-first-token
  (testing "the point of the whole file: one token is one account's limit, and
            before this every review spent the same one. The cursor lives in a
            FILE because each review is its own process — an in-process atom,
            which is how cchp does it, would be discarded before the next one"
    (with-redefs [tokens/state-path (constantly (tmp-state))]
      (is (= ["a" "b" "c" "a"]
             (mapv (fn [_] (tokens/select! ["a" "b" "c"] 0)) (range 4)))))))

(deftest a-parked-token-is-skipped-until-its-park-ends
  (let [path (tmp-state)]
    (with-redefs [tokens/state-path (constantly path)]
      (tokens/park! "b" 1000)
      (testing "b is passed over while parked"
        (is (= ["a" "c" "a" "c"]
               (mapv (fn [_] (tokens/select! ["a" "b" "c"] 2000)) (range 4)))))
      (testing "and comes back once the hour is up"
        (is (contains? (set (mapv (fn [_] (tokens/select! ["a" "b" "c"]
                                                          (+ 1000 (* 2 60 60 1000))))
                                  (range 3)))
                       "b"))))))

(deftest every-token-parked-still-returns-one
  (testing "a park is a one-hour GUESS — the CLI's prose banner carries no
            reset this can parse. Refusing to run would turn that guess into a
            review that never happens, and a wrong guess costs one MALFORMED
            attempt, which the ledger charges no pass for"
    (let [path (tmp-state)]
      (with-redefs [tokens/state-path (constantly path)]
        (tokens/park! "a" 1000)
        (tokens/park! "b" 5000)
        (is (= "a" (tokens/select! ["a" "b"] 2000))
            "the one whose park ends soonest")))))

(deftest the-state-file-never-holds-a-token
  (testing "it is a plain file in the user's cache. The token has exactly one
            home; a second copy is a second place to leak it from"
    (let [path (tmp-state)
          tok  "sk-ant-oat01-SECRET-VALUE"]
      (with-redefs [tokens/state-path (constantly path)]
        (tokens/select! [tok "other"] 0)
        (tokens/park! tok 0)
        (let [raw (slurp path)]
          (is (not (str/includes? raw tok)) "not the token")
          (is (str/includes? raw (tokens/token-key tok))
              "the hash IS there — or this test passes on an empty file"))))))

(deftest an-unwritable-cache-costs-rotation-and-not-the-review
  (testing "falls back to the first token, which is what every review used to
            run on"
    (with-redefs [tokens/state-path (constantly "/proc/nope/tokens.edn")]
      (is (= "a" (tokens/select! ["a" "b"] 0)))))
  (testing "and an empty pool is nil, the signal to use the pinned file"
    (is (nil? (tokens/select! [] 0)))))

;;; Limit banners

(deftest a-limit-banner-is-recognised-and-ordinary-prose-is-not
  (is (tokens/limited?
       "You've hit your limit · resets 3am (Asia/Shanghai)")
      "the subscription banner")
  (is (tokens/limited?
       "You've hit your session limit · resets 8:30pm (Asia/Shanghai)")
      "the session-window variant: one extra word, and minutes in the time")
  (is (tokens/limited?
       (str "You've hit your org" "'s monthly spend limit. Ask an admin."))
      "the org seat banner")
  (is (not (tokens/limited? "the rate limit handling here looks wrong"))
      "a review that DISCUSSES limits must not park the token it ran on")
  (is (not (tokens/limited? nil))))

;;; The wiring into the reviewer

(deftest the-token-in-the-child-is-the-token-that-gets-redacted
  (testing "with a rotating pool, selecting twice is not selecting twice as
            slowly — it is two DIFFERENT tokens. The child would run on one and
            `redact` would scrub the other, leaving the credential actually in
            the child's environment in the PR comment"
    (with-redefs [tokens/state-path (constantly (tmp-state))
                  tokens/pool (constantly ["tok-one-aaaaaaaa" "tok-two-bbbbbbbb"])]
      (let [res (reviewer/run!
                 "P" "."
                 {:spawn-fn (fn [_ _ _ _]
                              ;; what the CHILD would be given
                              {:exit 0
                               :out (str "tok=" (get (reviewer/spawn-env)
                                                     "CLAUDE_CODE_OAUTH_TOKEN"))})})]
        (is (= "tok=[redacted]" (:out res)))))))

(deftest the-pool-does-not-reach-the-child
  (testing "the reviewer is itself a Claude Code session and would inherit the
            pool cchp passes down. `redact` scrubs only the ONE token it was
            given, so every other token in the pool would be unredacted in
            anything the reviewer printed"
    (with-redefs [tokens/state-path (constantly (tmp-state))
                  tokens/pool (constantly ["tok-one-aaaaaaaa" "tok-two-bbbbbbbb"])]
      (let [spawn @#'reviewer/default-spawn
            res (binding [reviewer/*token* "tok-one-aaaaaaaa"]
                  (spawn ["sh" "-c" "echo \"[$CLAUDE_TOKENS]\""] "" "." nil))]
        (is (= "[]" (str/trim (:out res))))))))

(deftest with-no-pool-the-pinned-file-still-runs-the-review
  (testing "every installation without a CLAUDE_TOKENS pool keeps the
            behaviour it had before rotation existed"
    (let [f (str (fs/path (fs/create-temp-dir {:prefix "prl-tok"}) "t"))]
      (spit f "sk-pinned\n")
      (with-redefs [tokens/pool (constantly [])
                    reviewer/default-token-file f]
        (is (= "sk-pinned" (reviewer/select-token)))
        (is (= "sk-pinned" (get (reviewer/spawn-env) "CLAUDE_CODE_OAUTH_TOKEN"))))
      (with-redefs [tokens/pool (constantly [])
                    reviewer/default-token-file (str f ".absent")]
        (is (nil? (reviewer/select-token)))
        (is (not (contains? (reviewer/spawn-env) "CLAUDE_CODE_OAUTH_TOKEN"))
            "absent must still mean ABSENT, so the parent's auth is inherited")))))
