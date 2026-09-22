(ns pr-review.tokens-test
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [pr-review.reviewer :as reviewer]
            [pr-review.test-env :as test-env]
            [pr-review.tokens :as tokens]))

(use-fixtures :once test-env/hermetic-tokens)

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

(deftest a-quoted-value-with-a-trailing-comment-is-still-the-value
  (testing "quotes AND a comment on one line. Handled as two independent
            rules, `CLAUDE_TOKENS=\"a,b\" # jason` parsed to [\"\\\"a\"
            \"b\\\"\"] — and a mangled pool is worse than none: it is
            non-empty, so `select!` returns a token, `(or (select!)
            (oauth-token))` short-circuits, the pinned-file fallback never
            runs, and every review authenticates with a broken credential.
            The file this reads is documented as carrying exactly these
            comments"
    (let [f (str (fs/path (fs/create-temp-dir {:prefix "prl-env"}) ".env.local"))]
      (spit f (str "DQ=\"a,b\" # jason, 0g\n"
                   "SQ='c,d' # personal\n"
                   "BARE=e,f # jason\n"))
      (is (= "a,b" (tokens/env-file-var f "DQ")) "double-quoted, then a comment")
      (is (= "c,d" (tokens/env-file-var f "SQ")) "single-quoted, then a comment")
      (is (= "e,f" (tokens/env-file-var f "BARE")) "unquoted, then a comment")))

  (testing "and the pool that comes out of it holds no quote characters —
            a token with a stray quote is a token that does not authenticate"
    (let [f (str (fs/path (fs/create-temp-dir {:prefix "prl-env"}) ".env.local"))]
      (spit f "CLAUDE_TOKENS=\"tok-aaaa,tok-bbbb\" # jason, 0g\n")
      (is (= ["tok-aaaa" "tok-bbbb"] (tokens/pool {:env-file f}))))))

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
  (testing "a park can be wrong in either direction — a parsed reset from
            another machine's clock, or `park-ms` when the prose named none.
            Refusing to run would turn that into a review that never happens,
            and a wrong park costs one MALFORMED attempt, which the ledger
            charges no pass for"
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
          tok "sk-ant-oat01-SECRET-VALUE"]
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

;;; How long a park lasts

(defn- shanghai-ms
  "An epoch-ms for a wall-clock instant in Asia/Shanghai, so these tests do
   not depend on the machine's zone."
  [y m d hh mm]
  (-> (java.time.LocalDate/of y m d)
      (.atTime (java.time.LocalTime/of hh mm))
      (.atZone (java.time.ZoneId/of "Asia/Shanghai"))
      .toInstant
      .toEpochMilli))

(deftest a-park-runs-to-the-reset-the-banner-states
  (testing "the measured case, which is why this exists: an org seat parked at
            22:24 on a banner saying `resets 3am` came back into rotation at
            23:24 — 3h36m before the account was actually live again. One hour
            is a guess; the banner is not"
    (let [now (shanghai-ms 2026 9 21 22 24)]
      (is (= (shanghai-ms 2026 9 22 3 0)
             (tokens/reset-at-ms "resets 3am (Asia/Shanghai)" now))
          "the NEXT 3am, not today's, which is already past")))

  (testing "minutes, and the two ends of the 12-hour clock, which is where an
            am/pm conversion goes wrong"
    (let [now (shanghai-ms 2026 9 21 13 0)]
      (is (= (shanghai-ms 2026 9 21 20 30)
             (tokens/reset-at-ms "resets 8:30pm (Asia/Shanghai)" now)))
      (is (= (shanghai-ms 2026 9 22 0 0)
             (tokens/reset-at-ms "resets 12am (Asia/Shanghai)" now))
          "12am is midnight, hour 0 — not hour 12")
      (is (= (shanghai-ms 2026 9 21 0 0)
             (tokens/reset-at-ms "resets 12am (Asia/Shanghai)"
                                 (shanghai-ms 2026 9 20 13 0)))
          "the NEXT midnight for a caller a day earlier — never a past one,
           which the `.isAfter` filter makes impossible")
      (is (= (shanghai-ms 2026 9 22 12 0)
             (tokens/reset-at-ms "resets 12pm (Asia/Shanghai)" now))
          "12pm is NOON, hour 12 — tomorrow's, since `now` is 13:00 and noon
           today is past. The other end of the clock, and the branch that
           stayed untested while this block's own description claimed both:
           mapping 12pm to hour 0 would give 2026-09-22T00:00 here")))

  (testing "a DATED reset, which the org banner uses, and which carries no year"
    (let [now (shanghai-ms 2026 9 21 22 24)]
      (is (= (shanghai-ms 2026 9 25 3 0)
             (tokens/reset-at-ms "resets Sep 25 at 3am (Asia/Shanghai)" now)))))

  (testing "the ZONE is the CLI's, not this machine's"
    (let [now (shanghai-ms 2026 9 22 8 0)]
      (is (= (shanghai-ms 2026 9 22 15 0)
             (tokens/reset-at-ms "resets 3am (America/New_York)" now))
          "3am New York is 3pm Shanghai the same day")))

  (testing "nil for anything that cannot be placed on a clock, or that lands
            outside the window any real limit uses. The caller has the hour to
            fall back on, and a park of years is how reviews stop with nothing
            in the logs to say why"
    (let [now (shanghai-ms 2026 9 21 22 24)]
      (is (nil? (tokens/reset-at-ms "resets Dec 25 at 3am (Asia/Shanghai)" now))
          "beyond 8 days: disbelieved, not clamped")
      (is (nil? (tokens/reset-at-ms "resets 3am (Mars/Olympus)" now))
          "not a zone")
      (is (nil? (tokens/reset-at-ms "the limit resets eventually" now)))
      (is (nil? (tokens/reset-at-ms nil now)))
      (is (nil? (tokens/reset-at-ms "resets 99am (Asia/Shanghai)" now))
          "not an hour"))))

(deftest quoted-prose-cannot-lengthen-a-park
  (testing "what arrives at `reset-at-ms` is the reviewer's WHOLE output, and
            the reviewer reads repositories and quotes what it finds —
            including, on this repository, the fixtures in this file. Taking
            the FIRST `resets …` let a quotation decide the park: measured, a
            quoted `Sep 28 at 3am` above a genuine `resets 3am` gave a
            138.7-hour park where the banner said 18.7, and nothing can
            correct it — there is no unpark and `park!` refuses to shorten.

            So the rule is the EARLIEST of every match, which makes a quoted
            instant able only to SHORTEN a park. Both orderings are asserted:
            `re-find` takes the first, so a quotation BELOW the banner passes
            even with the defect armed."
    (let [now (shanghai-ms 2026 9 21 22 24)
          real (shanghai-ms 2026 9 22 3 0)
          banner "You have hit it · resets 3am (Asia/Shanghai)"
          far "the diff quotes resets Sep 28 at 3am (Asia/Shanghai)"]
      (is (= real (tokens/reset-at-ms (str far "\n" banner) now))
          "quotation ABOVE the banner")
      (is (= real (tokens/reset-at-ms (str banner "\n" far) now))
          "and BELOW it")
      (is (= real (tokens/reset-at-ms banner now))
          "and the banner alone is unchanged")))

  (testing "a quoted EARLIER instant may shorten the park, which is the
            deliberate direction: it costs one retry, which re-parks"
    (let [now (shanghai-ms 2026 9 21 22 24)
          soon (shanghai-ms 2026 9 21 23 0)]
      (is (= soon (tokens/reset-at-ms
                   (str "quoting resets 11pm (Asia/Shanghai)\n"
                        "You have hit it · resets 3am (Asia/Shanghai)")
                   now))))))

(deftest a-park-lasts-until-the-stated-reset
  (let [path (tmp-state)
        now (shanghai-ms 2026 9 21 22 24)
        reset (shanghai-ms 2026 9 22 3 0)]
    (with-redefs [tokens/state-path (constantly path)]
      (tokens/park! "a" now "You've hit it · resets 3am (Asia/Shanghai)")
      (is (= "b" (tokens/select! ["a" "b"] (+ now (* 2 60 60 1000))))
          "two hours later — an hour-long park would have handed `a` back")
      (is (= "b" (tokens/select! ["a" "b"] (- reset 60000)))
          "and a minute before the stated reset")
      (is (= "a" (tokens/select! ["a" "b"] (+ reset 60000)))
          "but after it, `a` is back"))))

(deftest a-second-park-never-shortens-the-first
  (testing "a banner-less rejection while a parsed park is still running would
            otherwise replace a multi-day park with an hour and hand the spent
            account straight back out.

            Asserted on the STORED INSTANT, not through `select!`: with both
            tokens available `select!` returns whichever the cursor is on, so
            `(= \"b\" (select! …))` is satisfied by cursor position and passed
            with the shortening bug armed — measured. And in its OWN state
            file with no `select!` before it, because `select!` prunes: a
            select after the reset drops the park, and then there is nothing
            left for a later park to shorten — which is how the first version
            of this test failed for a reason that was not the bug."
    (let [path (tmp-state)
          now (shanghai-ms 2026 9 21 22 24)
          reset (shanghai-ms 2026 9 22 3 0)]
      (with-redefs [tokens/state-path (constantly path)]
        (tokens/park! "a" now "You've hit it · resets 3am (Asia/Shanghai)")
        (tokens/park! "a" (+ now 60000) nil)
        (let [stored (get-in (edn/read-string (slurp path))
                             [:parked (tokens/token-key "a")])]
          (is (= reset (long stored))
              "the stated reset, not (+ now 60000 one-hour)")
          (is (> (long stored) (+ now 60000 (* 60 60 1000)))
              "and strictly later than what the second park alone would give"))))))

(deftest the-banner-reaches-park-through-run
  (testing "the wiring, which is the part no unit test above covers: `run!`
            must hand the reviewer's OUTPUT to `park!`, or the parse has
            nothing to read and every park is the one-hour guess again"
    (let [path (tmp-state)
          selected "tok-selected-aaaaaaaa"]
      (with-redefs [tokens/state-path (constantly path)
                    tokens/known-tokens (constantly [selected])
                    reviewer/default-token-file "/no/such/pinned/file"]
        (let [before (System/currentTimeMillis)
              _ (reviewer/run!
                 "P" "."
                 {:token-fn (constantly selected)
                  ;; what a spent account actually returns: exit 1, banner on
                  ;; stdout. The park is gated on the non-zero exit.
                  :spawn-fn (fn [_ _ _ _]
                              {:exit 1
                               :out "You've hit your limit · resets 3am (Asia/Shanghai)"
                               :err ""})})
              stored (get-in (edn/read-string (slurp path))
                             [:parked (tokens/token-key selected)])]
          (is stored "the token was parked at all")
          (is (= (long stored)
                 (tokens/reset-at-ms "resets 3am (Asia/Shanghai)" before))
              "to the instant the banner named — not to now + one hour")
          (is (not= (long stored) (+ before (* 60 60 1000)))
              "stated differently, because the two are only equal if the
               banner never arrived"))))))

;;; Limit banners

(deftest a-limit-banner-is-recognised-and-ordinary-prose-is-not
  (is (tokens/limited?
       "You've hit your limit · resets 3am (Asia/Shanghai)")
      "the subscription banner")
  (is (tokens/limited?
       "You've hit your session limit · resets 8:30pm (Asia/Shanghai)")
      "the session-window variant: one extra word, and minutes in the time")
  (is (tokens/limited?
       "You've hit your 5-hour limit · resets 3am (Asia/Shanghai)")
      "a HYPHENATED window. cchp's `\\w+` does not match this one, and an
       unmatched banner hands the spent token back out on the next rotation")
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
            pool cchp passes down. Keeping it out of the child is the first
            line: `redact` covers what is PRINTED, and a variable the child
            never has is one the child cannot print.

            Asserted on the MAP, not on what a child prints. The earlier
            version ran `echo \"[$CLAUDE_TOKENS]\"` and expected `[]` — which
            is also what a child prints when the strip is DELETED and the test
            process has no CLAUDE_TOKENS of its own, which is every plain
            shell. Measured: with the strip removed, that assertion passed
            unset and failed only when the variable was set, so the one check
            guarding pool isolation was decided by the tester's environment.
            A child probe cannot fix that from in here — a process cannot add
            CLAUDE_TOKENS to its OWN environment to be inherited — and
            `spawn-env` is where the decision is made anyway."
    (with-redefs [tokens/state-path (constantly (tmp-state))
                  tokens/pool (constantly ["tok-one-aaaaaaaa" "tok-two-bbbbbbbb"])]
      (let [env (binding [reviewer/*token* "tok-one-aaaaaaaa"] (reviewer/spawn-env))]
        (is (contains? env "CLAUDE_TOKENS")
            "the key must be PRESENT: `:extra-env` overrides only what the map
             names, so an absent key passes the inherited pool straight through")
        (is (nil? (get env "CLAUDE_TOKENS"))
            "and nil, which `:extra-env` gives the child as an empty value —
             measured, it does not drop the variable")))))

(deftest a-pool-token-this-run-did-not-select-is-redacted-too
  (testing "the reviewer has Read, Grep, Glob and Bash, and `pr-review.tokens`
            puts the pool's FILE PATH into source it is routinely asked to
            read. A reviewer that `cat`s that file while checking the parser
            prints every account's credential, and scrubbing only the selected
            one leaves the other N-1 in the PR comment, the findings file and
            the streamed stderr — all three of which are republished"
    (let [selected "tok-selected-aaaaaaaa"
          other "tok-other-bbbbbbbb"
          d (str (fs/create-temp-dir {:prefix "prl-red"}))
          errf (str (fs/path d "err"))]
      (with-redefs [tokens/state-path (constantly (tmp-state))
                    tokens/known-tokens (constantly [selected other])]
        (let [res (reviewer/run!
                   "P" "."
                   {:token-fn (constantly selected)
                    :err-file errf
                    :spawn-fn (fn [_ _ _ ef]
                                (spit ef (str "cat .env.local: " other "\n"))
                                {:exit 0
                                 :out (str "the pool is " selected " and " other)
                                 :err (str "stderr had " other)})})]
          (is (not (str/includes? (:out res) other)) "the PR comment")
          (is (not (str/includes? (:err res) other)) "the failure message")
          (is (not (str/includes? (slurp errf) other))
              "and the streamed file, the copy that survives a kill")
          (is (= "the pool is [redacted] and [redacted]" (:out res))
              "both, and redacted rather than merely absent"))))))

(deftest a-disabled-pool-in-the-env-file-is-still-a-live-credential
  (testing "`pool` skips comment lines, and rightly — a disabled line must not
            be authenticated with. But those lines hold PRIOR pools: live
            tokens for real accounts. `known-tokens` answers the other
            question, which strings would be a leak if printed, and one `cat`
            of a path this source names prints every one of them"
    (let [f (str (fs/path (fs/create-temp-dir {:prefix "prl-known"}) ".env.local"))]
      (spit f (str "# jason, 0g\n"
                   "# CLAUDE_TOKENS=tok-retired-aaaa,tok-retired-bbbb\n"
                   "#CLAUDE_TOKENS=tok-retired-cccc\n"
                   "CLAUDE_TOKENS=tok-live-dddd,tok-live-eeee\n"))
      (is (= ["tok-live-dddd" "tok-live-eeee"] (tokens/pool {:env-file f}))
          "the POOL is the live line only — nothing else may be used to log in")
      (is (= #{"tok-retired-aaaa" "tok-retired-bbbb" "tok-retired-cccc"
               "tok-live-dddd" "tok-live-eeee"}
             (set (tokens/known-tokens f)))
          "the MARKER SET is every one of them, `#` or not"))))

(deftest a-retired-token-the-reviewer-prints-is-redacted
  (testing "the whole point of the marker set, through `run!`: the reviewer
            `cat`s the env file while checking the parser and prints a pool
            that is no longer in use"
    (let [retired "tok-retired-aaaaaaaa"
          f (str (fs/path (fs/create-temp-dir {:prefix "prl-known"}) ".env.local"))
          _ (spit f (str "# CLAUDE_TOKENS=" retired "\n"
                         "CLAUDE_TOKENS=tok-live-bbbbbbbb\n"))
          ;; the VALUE, read AFTER the file exists and BEFORE the redef.
          ;; Read before the spit it is empty; read lazily inside the stub it
          ;; calls itself, and that is a StackOverflowError, which
          ;; `credentials`' `(catch Exception)` does not catch — both
          ;; measured, one as a green-looking failure and one as an uncaught
          ;; test error
          from-file (tokens/known-tokens f)]
      (is (some #{retired} from-file)
          "the fixture must actually contain the retired token, or the
           assertion below passes on an empty marker set")
      (with-redefs [tokens/state-path (constantly (tmp-state))
                    tokens/known-tokens (constantly from-file)
                    reviewer/default-token-file "/no/such/pinned/file"]
        (let [res (reviewer/run!
                   "P" "."
                   {:token-fn (constantly "tok-live-bbbbbbbb")
                    :spawn-fn (fn [_ _ _ _]
                                {:exit 0
                                 :out (str "cat .env.local: " retired)})})]
          (is (= "cat .env.local: [redacted]" (:out res))))))))

(deftest the-pinned-fallback-token-is-redacted-even-when-the-pool-is-used
  (testing "a REGRESSION introduced by rotation, not a pre-existing gap:
            before it, the selected token always WAS the pinned one, so it was
            always in the marker set. With a non-empty pool the pinned file is
            in neither the pool nor the selection, and it became the one
            credential this process holds and does not scrub — reachable by
            the same route as the pool, since `reviewer/default-token-file`
            builds its path in source the reviewer is asked to read"
    (let [pinned "sk-ant-oat01-PINNED-SECRET"
          f (str (fs/path (fs/create-temp-dir {:prefix "prl-pin"}) "t"))
          selected "tok-selected-aaaaaaaa"]
      (spit f (str pinned "\n"))
      (with-redefs [tokens/state-path (constantly (tmp-state))
                    tokens/known-tokens (constantly [selected "tok-other-bbbbbbbb"])
                    reviewer/default-token-file f]
        (let [res (reviewer/run!
                   "P" "."
                   {:token-fn (constantly selected)
                    :spawn-fn (fn [_ _ _ _]
                                {:exit 0
                                 :out (str "cat default-cc-token: " pinned)})})]
          (is (= "cat default-cc-token: [redacted]" (:out res))))))))

(deftest a-pool-edited-mid-review-does-not-unmark-the-token-in-flight
  (testing "the secrets are computed ONCE, at the start of the run. Read again
            at redaction time, a pool the operator edited while the review ran
            — minutes, for a real one — would leave the departed member
            verbatim in the output of the review still running on it"
    (let [departing "tok-departing-aaaaaaaa"
          live (atom [departing "tok-staying-bbbbbbbb"])]
      (with-redefs [tokens/state-path (constantly (tmp-state))
                    tokens/known-tokens (fn [& _] @live)
                    reviewer/default-token-file "/no/such/pinned/file"]
        (let [res (reviewer/run!
                   "P" "."
                   {:token-fn (constantly "tok-staying-bbbbbbbb")
                    :spawn-fn (fn [_ _ _ _]
                                ;; the operator edits .env.local mid-review
                                (reset! live ["tok-staying-bbbbbbbb"])
                                {:exit 0 :out (str "leaked " departing)})})]
          (is (= "leaked [redacted]" (:out res))))))))

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
