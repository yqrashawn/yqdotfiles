(ns pr-review.tokens-test
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [pr-review.reviewer :as reviewer]
            [pr-review.test-env :as test-env]
            [pr-review.tokens :as tokens]))

(use-fixtures :once test-env/hermetic-tokens)

(defn- plugin-root
  "This plugin's directory, found through the classpath rather than through
   the working directory: `bb test` runs here, but the suite is runnable from
   the repo root too, and a relative `slurp` errors there rather than
   failing."
  []
  (-> (io/resource "pr_review/tokens.clj") .getPath fs/path
      fs/parent fs/parent fs/parent))

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
          tok "state-file-credential-aaaa"]
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

(defn- banner
  "A limit banner carrying `tail`. `reset-at-ms` reads a candidate only from
   the line a BANNER starts on, so a bare `resets …` is prose to it and parses
   to nil — which is the property, and it means every parser test here has to
   state a banner rather than a tail.

   Two openings because the patterns differ: the subscription one requires a
   numeric time right after `resets`, so a DATED tail needs the org opening,
   which matches on its clause alone.

   BOTH are assembled from halves, and the `limited?` invariant below is what
   holds that: a file `limited?` matches is a file the reviewer can quote back
   as a live limit, and this file is one the reviewer reads. A comment used to
   assert the same discipline and did not keep it."
  ([tail]
   (if (re-find #"^resets \d" tail)
     (str "You've hit your" " limit · " tail)
     (str "You've hit your org" "'s monthly spend limit · " tail)))
  ([qualifier tail]
   (str "You've hit your " qualifier " limit · " tail)))

(def ^:private real-org-banner
  "The org-seat banner this loop actually received, 2026-09-21, when the
   account it was running on hit its limit mid-review. Split for the reason
   `banner` gives.

   It is here because `reset-at-ms` reads the reset from the banner's own
   LINE, and until this sample existed that was an assumption about a shape
   nobody had captured: `limit-patterns` matches the opening clause only, so
   if the real banner put its reset elsewhere the parse would return nil and
   every org-limit park would silently be the one-hour fallback this work
   exists to replace. One line, three `·`-separated segments, reset in the
   last one."
  (str "You've hit your org" "'s monthly spend limit · ask your admin to"
       " raise it at claude.ai/admin-settings/usage · your weekly limit"
       " resets 3am (Asia/Shanghai)"))

(deftest a-park-runs-to-the-reset-the-banner-states
  (testing "the measured case, which is why this exists: an org seat parked at
            22:24 on a banner saying `resets 3am` came back into rotation at
            23:24 — 3h36m before the account was actually live again. One hour
            is a guess; the banner is not"
    (let [now (shanghai-ms 2026 9 21 22 24)]
      (is (= (shanghai-ms 2026 9 22 3 0)
             (tokens/reset-at-ms (banner "resets 3am (Asia/Shanghai)") now))
          "the NEXT 3am, not today's, which is already past")))

  (testing "minutes, and the two ends of the 12-hour clock, which is where an
            am/pm conversion goes wrong"
    (let [now (shanghai-ms 2026 9 21 13 0)]
      (is (= (shanghai-ms 2026 9 21 20 30)
             (tokens/reset-at-ms (banner "resets 8:30pm (Asia/Shanghai)") now)))
      (is (= (shanghai-ms 2026 9 22 0 0)
             (tokens/reset-at-ms (banner "resets 12am (Asia/Shanghai)") now))
          "12am is midnight, hour 0 — not hour 12")
      (is (= (shanghai-ms 2026 9 21 0 0)
             (tokens/reset-at-ms (banner "resets 12am (Asia/Shanghai)")
                                 (shanghai-ms 2026 9 20 13 0)))
          "the NEXT midnight for a caller a day earlier — never a past one,
           which the `.isAfter` filter makes impossible")
      (is (= (shanghai-ms 2026 9 22 12 0)
             (tokens/reset-at-ms (banner "resets 12pm (Asia/Shanghai)") now))
          "12pm is NOON, hour 12 — tomorrow's, since `now` is 13:00 and noon
           today is past. The other end of the clock, and the branch that
           stayed untested while this block's own description claimed both:
           mapping 12pm to hour 0 would give 2026-09-22T00:00 here")))

  (testing "a DATED reset, which the org banner uses, and which carries no year"
    (let [now (shanghai-ms 2026 9 21 22 24)]
      (is (= (shanghai-ms 2026 9 25 3 0)
             (tokens/reset-at-ms (banner "resets Sep 25 at 3am (Asia/Shanghai)") now)))))

  (testing "the ZONE is the CLI's, not this machine's"
    (let [now (shanghai-ms 2026 9 22 8 0)]
      (is (= (shanghai-ms 2026 9 22 15 0)
             (tokens/reset-at-ms (banner "resets 3am (America/New_York)") now))
          "3am New York is 3pm Shanghai the same day")))

  (testing "nil for anything that cannot be placed on a clock, or that lands
            outside the window any real limit uses. The caller has the hour to
            fall back on, and a park of years is how reviews stop with nothing
            in the logs to say why"
    (let [now (shanghai-ms 2026 9 21 22 24)]
      (is (nil? (tokens/reset-at-ms (banner "resets Dec 25 at 3am (Asia/Shanghai)") now))
          "beyond 8 days: disbelieved, not clamped")
      (is (nil? (tokens/reset-at-ms (banner "resets 3am (Mars/Olympus)") now))
          "not a zone")
      (is (nil? (tokens/reset-at-ms "the limit resets eventually" now)))
      (is (nil? (tokens/reset-at-ms nil now)))
      (is (nil? (tokens/reset-at-ms (banner "resets 99am (Asia/Shanghai)") now))
          "not an hour"))))

(deftest the-real-org-banner-parses
  (testing "the shape rule 1 rests on, checked against the banner that was
            actually received rather than against an invented fixture. If the
            org banner put its reset on another line, every org-limit park
            would quietly be the one-hour fallback"
    (let [now (shanghai-ms 2026 9 21 22 24)]
      (is (tokens/limited? real-org-banner)
          "recognised as a limit at all")
      (is (= (shanghai-ms 2026 9 22 3 0)
             (tokens/reset-at-ms real-org-banner now))
          "and its reset is read out of it — 3am, two segments after the
           clause `limit-patterns` matches, on the same line"))))

(defn- plugin-root
  "The plugin directory, however `bb` was invoked. `babashka.config` is the
   path of the `bb.edn` in use, so its parent is the root whatever the cwd
   is; `user.dir` is the fallback for a REPL started without one.

   Copied from `hooksjson-test`, which needed it for the same reason, and
   needed here for a sharper one: a cwd-relative glob that finds nothing
   returns an empty seq rather than throwing, so the invariant below would
   PASS by checking zero files."
  []
  (or (some-> (System/getProperty "babashka.config") fs/parent str)
      (System/getProperty "user.dir")))

(deftest no-plugin-file-matches-a-limit-banner
  (testing "the reviewer reads this repository. A file that `limited?` matches
            is a file that, quoted back in a review, says a token is spent —
            and with the reset on the same line, says when it resets too.

            The invariant is exactly `limited?`, not \"contains banner-ish
            words\": the subscription pattern needs the reset tail with it,
            and the org pattern needs only its clause. Both are what a
            quotation would carry.

            Every file a recursive `fs/glob` reaches under the plugin — 41
            of them,
            which is everything but the hidden `.claude-plugin/` entries the
            glob skips by default. Not just the two `.clj` directories a
            narrower version globbed: `review_core.md` is the review prompt
            itself, and `commands/` and `skills/` are read and quoted as
            readily as source is.

            An invariant rather than a comment, because the comment version
            was asserted and not kept: on the commit this PR branched from,
            `limited?` on this file returned TRUE."
    (let [root (plugin-root)
          files (->> (fs/glob root "**")
                     (filter fs/regular-file?)
                     (remove #(str/includes? (str %) "/.git/")))]
      ;; Both guards, because the failure to avoid is a PASS that checked
      ;; nothing. A glob that finds no files returns an empty seq rather than
      ;; throwing, so `doseq` over it is silently vacuous: every per-file
      ;; assertion disappears and the run still reports 0 failures.
      ;;
      ;; The root is asserted to BE the plugin, not merely non-empty: under
      ;; `bb -cp hooks:test` with no `--config` there is no `babashka.config`
      ;; and `user.dir` is whatever the caller stood in, which would send this
      ;; scanning an unrelated tree and passing on it. `bb test` from the
      ;; repo root cannot reach here at all — the task's own file filter finds
      ;; nothing and `require` throws — so this covers the eval path.
      (is (fs/directory? (fs/path root "hooks" "pr_review"))
          (str root " is not the plugin root — this test would scan the wrong"
               " tree and pass on it"))
      (is (< 20 (count files))
          (str "only " (count files) " files found under " root
               " — the glob is not reaching the plugin tree"))
      ;; `when`, because `is` does not short-circuit: with a wrong root the
      ;; guard above reports it and the scan then ran anyway — measured at 799
      ;; assertions over an unrelated tree against 121 here.
      (doseq [f (when (fs/directory? (fs/path root "hooks" "pr_review")) files)]
        (is (not (tokens/limited? (try (slurp (str f)) (catch Exception _ ""))))
            (str f " matches `limited?` — assemble the banner from halves at"
                 " runtime, as `banner` and `real-org-banner` do"))))))

(deftest quoted-prose-does-not-set-the-park
  (testing "what arrives at `reset-at-ms` is the reviewer's WHOLE output, and
            the reviewer reads repositories and quotes what it finds —
            including, on this repository, the fixtures in this file. A
            candidate is read only from the LINE a banner starts on, so a
            quotation anywhere else contributes nothing, whichever side of the
            banner it is on and whether or not the banner has a reset tail of
            its own"
    (let [now (shanghai-ms 2026 9 21 22 24)
          real (shanghai-ms 2026 9 22 3 0)
          personal (banner "resets 3am (Asia/Shanghai)")
          ;; DERIVED from the captured sample, not rebuilt beside it: a
          ;; hand-written copy drifts from the real banner, and the whole
          ;; point of `real-org-banner` is that the shape is not invented.
          ;; `org-full` IS the sample; `org` is it with the reset segment cut
          ;; off, which is the clause `limit-patterns` matches on its own.
          org-full real-org-banner
          org (subs real-org-banner 0 (str/index-of real-org-banner " · your weekly"))
          quoted "review text quoting resets Sep 28 at 3am (Asia/Shanghai)"]
      (is (= real (tokens/reset-at-ms personal now))
          "the banner alone")
      (is (= real (tokens/reset-at-ms (str quoted "\n" personal) now))
          "a quotation ABOVE it — this is the one taking the first match got
           wrong, at 138.7h")
      (is (= real (tokens/reset-at-ms (str personal "\n" quoted) now))
          "and BELOW it")
      (is (= real (tokens/reset-at-ms (str org-full "\n" quoted) now))
          "the org banner's own tail, not the quotation below it")
      (is (nil? (tokens/reset-at-ms (str org "\n" quoted) now))
          "and when the banner has NO tail of its own, the quotation does not
           stand in for it — nil, so the caller falls back to `park-ms`. This
           is the one anchoring-without-the-line-bound got wrong, at 138.6h
           where the fallback is one hour")
      (is (nil? (tokens/reset-at-ms quoted now))
          "no banner at all: prose parks nothing")))

  (testing "a bogus hour is refused rather than placed on the clock. Only
            reachable from quoted text, and there it would SHORTEN a park
            below what the banner asked. `LocalTime/of` caught far less of it
            than it looks — measured against the old mapping, `am` passed
            everything to 23 and was first rejected at 24, `pm` first at 13,
            and `0am` gave midnight"
    (let [now (shanghai-ms 2026 9 21 22 24)]
      (is (nil? (tokens/reset-at-ms
                 (banner "resets 0am (Asia/Shanghai)") now)))
      (is (nil? (tokens/reset-at-ms
                 (banner "resets 13am (Asia/Shanghai)") now)))))

  (testing "with two banner lines, the SOONEST wins: a stale earlier banner
            must not extend a park.

            BOTH lines have to be banners the patterns actually match, or
            there is one candidate and the rule is unexercised — the first
            version of this used a dated tail behind the subscription opening,
            which that pattern does not match, and a control swapping
            `first` for `last` passed."
    (let [now (shanghai-ms 2026 9 21 22 24)]
      (is (= (shanghai-ms 2026 9 21 23 0)
             (tokens/reset-at-ms
              (str (banner "resets 3am (Asia/Shanghai)") "\n"
                   (banner "resets 11pm (Asia/Shanghai)"))
              now))
          "11pm tonight, not 3am tomorrow")
      (is (= (shanghai-ms 2026 9 21 23 0)
             (tokens/reset-at-ms
              (str (banner "resets 11pm (Asia/Shanghai)") "\n"
                   (banner "resets 3am (Asia/Shanghai)"))
              now))
          "and the same whichever order they arrive in"))))

(deftest a-park-lasts-until-the-stated-reset
  (let [path (tmp-state)
        now (shanghai-ms 2026 9 21 22 24)
        reset (shanghai-ms 2026 9 22 3 0)]
    (with-redefs [tokens/state-path (constantly path)]
      (tokens/park! "a" now (banner "resets 3am (Asia/Shanghai)"))
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
        (tokens/park! "a" now (banner "resets 3am (Asia/Shanghai)"))
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
                    tokens/known-secrets (constantly [selected])
                    reviewer/default-token-file "/no/such/pinned/file"]
        (let [before (System/currentTimeMillis)
              _ (reviewer/run!
                 "P" "."
                 {:token-fn (constantly selected)
                  ;; what a spent account actually returns: exit 1, banner on
                  ;; stdout. The park is gated on the non-zero exit.
                  :spawn-fn (fn [_ _ _ _]
                              {:exit 1
                               :out (banner "resets 3am (Asia/Shanghai)")
                               :err ""})})
              stored (get-in (edn/read-string (slurp path))
                             [:parked (tokens/token-key selected)])]
          (is stored "the token was parked at all")
          (is (= (long stored)
                 (tokens/reset-at-ms (banner "resets 3am (Asia/Shanghai)") before))
              "to the instant the banner named — not to now + one hour")
          (is (not= (long stored) (+ before (* 60 60 1000)))
              "stated differently, because the two are only equal if the
               banner never arrived"))))))

;;; Limit banners

(deftest a-limit-banner-is-recognised-and-ordinary-prose-is-not
  (is (tokens/limited?
       (banner "resets 3am (Asia/Shanghai)"))
      "the subscription banner")
  (is (tokens/limited?
       (banner "session" "resets 8:30pm (Asia/Shanghai)"))
      "the session-window variant: one extra word, and minutes in the time")
  (is (tokens/limited?
       (banner "5-hour" "resets 3am (Asia/Shanghai)"))
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
                    tokens/known-secrets (constantly [selected other])]
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
            tokens for real accounts. `known-secrets` answers the other
            question, which strings would be a leak if printed, and one `cat`
            of a path this source names prints every one of them.

            The spellings below are not decoration. Each of `# #`, `set` and
            an `OLD_` prefix was measured dropping a credential out of a
            version of this function that matched on the KEY; keying on the
            file is what makes the list finite."
    (let [f (str (fs/path (fs/create-temp-dir {:prefix "prl-known"}) ".env.local"))]
      (spit f (str "# jason, 0g\n"
                   "# CLAUDE_TOKENS=tok-retired-aaaaaaaa,tok-retired-bbbbbbbb\n"
                   "#CLAUDE_TOKENS=tok-retired-cccccccc\n"
                   "# # CLAUDE_TOKENS=tok-retired-dddddddd\n"
                   "# set CLAUDE_TOKENS=tok-retired-eeeeeeee\n"
                   "# OLD_CLAUDE_TOKENS=tok-retired-ffffffff\n"
                   "ANTHROPIC_API_KEY=key-not-a-claude-token\n"
                   "export CLAUDE_TOKENS=tok-live-gggggggg,tok-live-hhhhhhhh\n"))
      (is (= ["tok-live-gggggggg" "tok-live-hhhhhhhh"] (tokens/pool {:env-file f}))
          "the POOL is the live line only — nothing else may be used to log in")
      (is (= #{"tok-retired-aaaaaaaa" "tok-retired-bbbbbbbb"
               "tok-retired-aaaaaaaa,tok-retired-bbbbbbbb"
               "tok-retired-cccccccc" "tok-retired-dddddddd"
               "tok-retired-eeeeeeee" "tok-retired-ffffffff"
               "key-not-a-claude-token"
               "tok-live-gggggggg" "tok-live-hhhhhhhh"
               "tok-live-gggggggg,tok-live-hhhhhhhh"}
             (set (tokens/known-secrets f)))
          "the MARKER SET is every value in the file, `#` or not and whatever
           the key — including the credential that is not a Claude token, and
           including each comma list whole, because a leak can print the line")
      (is (some #{"key-not-a-claude-token"} (tokens/known-secrets f))
          "stated on its own: keying on CLAUDE_TOKENS left every other
           credential in this file unmarked, and cchp's `.env.local` holds
           several"))))

(deftest a-placeholder-in-a-credential-file-is-not-a-marker
  (testing "the cost of marking every assignment rather than every
            CLAUDE_TOKENS-keyed one: credential files are where placeholders
            live, and `redact` substitutes a marker EVERYWHERE it appears, so
            a placeholder-turned-marker garbles review text that has nothing
            to do with a credential"
    (let [f (str (fs/path (fs/create-temp-dir {:prefix "prl-ph"}) ".env.local"))]
      (spit f (str "# CLAUDE_TOKENS=REPLACE_ME_WITH_TOKEN\n"
                   "CLAUDE_TOKENS=<your-token-here-goes>\n"
                   "SHORT=abc123\n"
                   "REAL=tok-real-aaaaaaaaaa1\n"
                   "this line = prose, not an assignment\n"))
      (is (= #{"tok-real-aaaaaaaaaa1"} (set (tokens/known-secrets f)))
          "only the one that could be a credential")
      (is (every? tokens/marker-worthy?
                  ["AKIAIOSFODNN7EXAMPLE"
                   "JBSWY3DPEHPK3PXPJBSWY3DPEHPK3PXP"])
          "and an upper-case credential is NOT a placeholder: an AWS key id
           and a base32 TOTP secret are both spelled that way, and a rule
           rejecting every `[A-Z0-9_]+` value took them out of the marker set
           while `credential-shape-re` does not cover them either — neither
           layer held them")
      (is (not-any? tokens/marker-worthy?
                    ["https://api.anthropic.com/v1"
                     "/Users/x/workspace/home/claude-code-http-proxy"])
          "a URL or a path is configuration, and a marker is substituted
           everywhere it appears")
      (is (= "keep REPLACE_ME_WITH_TOKEN and <your-token-here-goes>"
             (reviewer/redact "keep REPLACE_ME_WITH_TOKEN and <your-token-here-goes>"
                              (tokens/known-secrets f)))
          "and the review text is left alone, which is the whole point of the
           gate"))))

(deftest a-credential-shape-is-redacted-without-being-named
  (testing "the FLOOR. Three review passes each found one more source outside
            the named set; a shape does not have a source. This is the
            reversal `redact`'s docstring states, and it is what makes a
            fourth unenumerated source not a leak"
    (let [unknown (str "sk-ant-" "oat01-nEVERnAMEDbyTHISpROCESS0123456789")]
      (is (str/includes? (reviewer/redact (str "printed " unknown) [])
                         "[redacted]")
          "no `secrets` at all, and it is still scrubbed")
      (is (not (str/includes? (reviewer/redact (str "printed " unknown) []) unknown)))
      (is (= "a review that discusses sk-ant- prefixes is untouched"
             (reviewer/redact "a review that discusses sk-ant- prefixes is untouched" []))
          "the prefix alone is prose: the shape needs a credential-length tail")
      (is (empty? (->> (fs/glob (plugin-root) "**/*.{clj,edn,md}")
                       (filter #(re-find tokens/credential-shape-re (slurp (str %))))
                       (map str)))
          "and nothing in this plugin matches the pattern — not just the two
           hook files. The reviewer reads the REPOSITORY, so a fixture that
           matches comes back as `[redacted]` in a review of this tree, and a
           test fixture that the shape floor scrubs cannot guard the named
           layer (which is how three tests here came to pass on the floor
           alone). #176 was the same mistake with worse consequences.

           Anchored on `plugin-root`, not on the working directory: this
           suite is runnable from the repo root, where a relative path errors
           rather than fails."))))

(deftest the-marker-set-covers-every-named-file-not-one-of-them
  (testing "the override says which file the POOL comes from. It does not make
            the other file's contents stop being credentials, and the default
            file's path is a literal in source the reviewer is asked to read.
            `or` between the two left one of them unmarked"
    (is (= ["/tmp/override.env" tokens/default-env-file]
           (tokens/secret-files "/tmp/override.env"))
        "BOTH, override first")
    (is (= [tokens/default-env-file] (tokens/secret-files ""))
        "an empty override is not a path — `or` read it as one and dropped the
         default file")
    (is (= [tokens/default-env-file] (tokens/secret-files nil)))))

(deftest the-pool-reads-one-file-and-an-empty-override-is-not-one
  (testing "`pool` picks ONE file, where `secret-files` marks both — the pool
            is what this logs in with. But the override was read with a bare
            `or`, so PR_REVIEW_TOKENS_ENV_FILE=\"\" named a file that does not
            exist, found no pool in it and fell back to the pinned token
            without saying so"
    (is (= "/tmp/override.env" (tokens/pool-file "/tmp/override.env")))
    (is (= tokens/default-env-file (tokens/pool-file "")) "empty is not a path")
    (is (= tokens/default-env-file (tokens/pool-file nil)))))

(deftest the-no-argument-marker-set-is-every-file-and-the-environment
  (testing "the arity `credentials` actually calls, which every `run!`-level
            test stubs away. Both seams are functions so this is falsifiable
            without the tester's own environment deciding it"
    (let [d (fs/create-temp-dir {:prefix "prl-two"})
          f1 (str (fs/path d "one.env"))
          f2 (str (fs/path d "two.env"))]
      (spit f1 "CLAUDE_TOKENS=tok-from-file-one-aaaa\n")
      (spit f2 "# CLAUDE_TOKENS=tok-from-file-two-bbbb\n")
      (with-redefs [tokens/secret-files (constantly [f1 f2])
                    tokens/env-secrets (constantly ["tok-from-the-environment"])]
        (is (= #{"tok-from-file-one-aaaa" "tok-from-file-two-bbbb"
                 "tok-from-the-environment"}
               (set (tokens/known-secrets)))
            "the union, not the first non-empty one"))
      (with-redefs [tokens/env-secrets (constantly ["tok-from-the-environment"])]
        (is (= ["tok-from-file-one-aaaa"] (tokens/known-secrets f1))
            "and an EXPLICIT path is that file alone: an ambient variable
             winning over the argument makes the argument a no-op wherever it
             is set, which is every reviewer cchp spawns")))))

(deftest credentials-reads-the-real-marker-set
  (testing "the seam every other test stubs: `reviewer/credentials` →
            `tokens/known-secrets` → a file on disk. Stubbed at
            `known-secrets` everywhere else, so nothing exercised the call
            itself"
    (let [f (str (fs/path (fs/create-temp-dir {:prefix "prl-seam"}) ".env.local"))]
      (spit f "# CLAUDE_TOKENS=tok-retired-from-the-file\n")
      (with-redefs [tokens/secret-files (constantly [f])
                    tokens/env-secrets (constantly [])
                    reviewer/default-token-file "/no/such/pinned/file"]
        (is (some #{"tok-retired-from-the-file"} (reviewer/credentials nil))
            "the file's value reaches the redactor with nothing stubbed
             between them")))))

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
          from-file (tokens/known-secrets f)]
      (is (some #{retired} from-file)
          "the fixture must actually contain the retired token, or the
           assertion below passes on an empty marker set")
      (with-redefs [tokens/state-path (constantly (tmp-state))
                    tokens/known-secrets (constantly from-file)
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
    (let [;; NOT `sk-ant-…` shaped, deliberately: `redact`'s shape floor
          ;; scrubs that whatever `credentials` does, and this test guards
          ;; the NAMED layer — with a shaped fixture it stayed green with
          ;; `(oauth-token)` deleted out of `credentials`, which is the exact
          ;; regression its docstring above describes.
          pinned "pinned-credential-aaaaaaaa"
          f (str (fs/path (fs/create-temp-dir {:prefix "prl-pin"}) "t"))
          selected "tok-selected-aaaaaaaa"]
      (spit f (str pinned "\n"))
      (with-redefs [tokens/state-path (constantly (tmp-state))
                    tokens/known-secrets (constantly [selected "tok-other-bbbbbbbb"])
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
                    tokens/known-secrets (fn [& _] @live)
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
