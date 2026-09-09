(ns pr-review.trigger-test
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [pr-review.ledger :as ledger]
            [pr-review.lock :as lock]
            [pr-review.trigger :as trigger]))

(defn- tmp-repo
  "Returns [repo-root git-dir] for an ordinary clone."
  []
  (let [d (str (fs/create-temp-dir {:prefix "pr-review-trigger"}))
        g (str d "/.git")]
    (fs/create-dirs g)
    [d g]))

(defn- a-push
  [branch new-sha ts]
  {:remote "origin" :branch branch :old-sha "oldsha" :new-sha new-sha :ts ts})

(defn- a-pr
  "`head` is the PR's headRefOid, which `decide` matches against the sha git
   recorded — so it is required, not defaulted."
  [n head & {:keys [draft?]}]
  {:number n :isDraft (boolean draft?) :baseRefName "main" :headRefOid head})

(defn- opts
  "Wire every collaborator `decide` reads to a stub.

   `pushes` maps git-dir -> [push]; `prs` maps branch -> pr. `attempts`
   defaults to one agent attempt per push, from `session`, so the common case
   stays readable — pass it explicitly to exercise provenance. Both stubs
   honour `since` themselves, because the lookback window is part of what is
   under test rather than incidental to it."
  [& {:keys [pushes prs worktree now attempts session]}]
  (let [pushes (or pushes {})
        session (or session "sess-1")
        atts (or attempts
                 (vec (for [[gd ps] pushes p ps]
                        {:ts (:ts p) :git-dir gd :session session
                         :ref (str "refs/heads/" (:branch p))
                         :branch (:branch p) :sha (:new-sha p)})))]
    {:log-fn           (constantly "/nonexistent/pushes.log")
     :attempts-fn      (fn [_log since] (filterv #(>= (:ts %) since) atts))
     :pushes-fn        (fn [gd since] (filterv #(>= (:ts %) since) (get pushes gd)))
     :main-worktree-fn (fn [gd _] (or worktree gd))
     :open-pr-fn       (fn [_root branch _] (get prs branch))
     :now-fn           (constantly (or now 1000000))}))

(defn- an-attempt
  [gd branch sha ts & {:keys [session]}]
  {:ts ts :git-dir gd :session (or session "sess-1")
   :ref (str "refs/heads/" branch) :branch branch :sha sha})

(defn- input
  "A PostToolUse payload. Only the command and the duration are read now —
   notably NOT `cwd`, which is the session's directory and was the source of
   the defect this design removes."
  ([] (input "git push -u origin feat/x"))
  ([cmd] {:tool_input {:command cmd} :duration_ms 1000 :session_id "sess-1"}))

(defn- row
  [pr sha n & {:keys [fingerprints verdict]}]
  {:pr pr :sha sha :pass n :verdict (or verdict "NOT_MERGEABLE")
   :blocking 1 :followup 0 :coverage 0 :fingerprints (vec fingerprints)})

(defn- clean-reply
  "A well-formed reviewer reply with no findings."
  []
  (str "VERDICT: MERGEABLE — nothing to fix\n\n"
       "  [correctness/blocking]  none\n"
       "  [correctness/followup]  none\n"
       "  [coverage]              none\n"
       "  [docs-accuracy]         none\n"
       "  [style]                 none\n"))

(defn- review-opts
  "opts for `review!`: a stubbed diff, a stubbed reviewer and a stubbed
   checkout, so nothing shells out to a real `claude -p` or `git worktree`."
  [& {:keys [out exit err pid spawn-fn checkout]}]
  {:merge-base-fn (constantly "basesha")
   :diff-fn (constantly "diff --git a/a b/a\n")
   :pid (or pid 4242)
   :with-checkout-fn (fn [_gd _sha _parent _opts f]
                       (f (if (contains? #{:none} checkout) nil (or checkout "/review-root"))))
   :spawn-fn (or spawn-fn
                 (fn [_ _ _ _] {:exit (or exit 0)
                              :out (or out (clean-reply))
                              :err (or err "")}))})

;; ------------------------------------------------------------ the R2 gate

(deftest a-push-no-agent-made-is-never-reviewed
  (testing "R2, and it no longer rests on command text. The `pre-push` hook
            records the pushing session, so a push made by the user in a
            terminal carries none and cannot be attributed to an agent
            however push-shaped the command that follows it"
    (let [o (opts :pushes {"/g" [(a-push "feat/x" "newsha" 999999)]}
                  :prs {"feat/x" (a-pr 370 "newsha")}
                  :attempts [(an-attempt "/g" "feat/x" "newsha" 999999
                                         :session "-")])]
      (is (= :silent (:action (trigger/decide (input) o))))
      (is (= 0 (:candidates (trigger/decide (input) o)))))))

(deftest an-attempt-with-no-reflog-entry-is-a-push-that-never-landed
  (testing "a `pre-push` hook runs BEFORE the push, so an attempt proves only
            that one was tried. Git writes the reflog entry on success alone,
            which is what makes a rejected push — wrong credentials, a
            non-fast-forward, a hook further down the chain saying no —
            unreviewable rather than reviewed as if it had landed"
    (let [o (opts :pushes {}          ; nothing in any reflog
                  :prs {"feat/x" (a-pr 370 "newsha")}
                  :attempts [(an-attempt "/g" "feat/x" "newsha" 999999)])]
      (is (= :silent (:action (trigger/decide (input) o))))
      (is (= 0 (:candidates (trigger/decide (input) o)))))))

(deftest an-attempt-at-a-different-sha-than-landed-is-not-a-match
  (testing "the attempt and the reflog must agree on the sha, or a superseded
            attempt would vouch for a push it did not make"
    (let [o (opts :pushes {"/g" [(a-push "feat/x" "landed" 999999)]}
                  :prs {"feat/x" (a-pr 370 "landed")}
                  :attempts [(an-attempt "/g" "feat/x" "attempted" 999999)])]
      (is (= :silent (:action (trigger/decide (input) o)))))))

(deftest a-push-with-no-trigger-verb-is-reviewed-only-for-its-own-session
  (testing "`git -C /x push` contains no `git push`, and was 1 of 260 real
            commands lost to that. The session that pushed still gets its
            review; a different session running an unrelated command does not
            — two accidental reviews of PR #395 came from exactly that"
    (let [mk (fn [sess] (opts :pushes {"/g" [(a-push "feat/x" "newsha" 999999)]}
                              :prs {"feat/x" (a-pr 370 "newsha")}
                              :session sess))
          cmd (input "git -C /x push")]
      (is (= :review (:action (trigger/decide cmd (mk "sess-1"))))
          "the pushing session's own later command still triggers")
      (is (= :silent (:action (trigger/decide cmd (mk "sess-other"))))
          "another session's unrelated command must not")
      (testing "and with a verb present, any agent's push is in scope"
        (is (= :review (:action (trigger/decide (input "git push")
                                                (mk "sess-other")))))))))

(deftest the-command-still-selects-the-lookback
  (let [o (opts :pushes {"/g" [(a-push "feat/x" "newsha" 999999)]}
                :prs {"feat/x" (a-pr 370 "newsha")})]
    (doseq [[cmd verb] {"git push -u origin feat/x" :push
                        "rtk git push" :push
                        "cd /x && git push 2>&1 | tail -2" :push
                        "git  push" :push
                        "gh pr create --base main" :create
                        "rtk gh pr create --title x" :create
                        "cd /x && gh  pr  create" :create
                        "git status" nil}]
      (testing cmd
        (is (= verb (:trigger (trigger/decide (input cmd) o))))))))

;; ------------------------------------------------------------- the window

(deftest a-push-older-than-the-tool-call-window-is-not-attributed-to-it
  (let [o (fn [ts] (opts :pushes {"/g" [(a-push "feat/x" "newsha" ts)]}
                         :prs {"feat/x" (a-pr 370 "newsha")}
                         :now 1000000))]
    (is (= :review (:action (trigger/decide (input) (o 999000))))
        "a push seconds before the call returned is this call's push")
    (is (= :silent (:action (trigger/decide (input) (o 800000))))
        "a push three minutes earlier is not")))

(deftest gh-pr-create-looks-back-past-its-own-tool-call
  (testing "`gh pr create` pushes nothing, so the entry it needs belongs to
            the push that made the branch — measured at 29s and 69s earlier in
            the two real cases. Without the longer lookback, PR creation could
            never be reviewed at all"
    (let [o (opts :pushes {"/g" [(a-push "feat/x" "newsha" 900000)]}
                  :prs {"feat/x" (a-pr 370 "newsha")}
                  :now 1000000)]
      (is (= :silent (:action (trigger/decide (input "git push") o)))
          "100s is outside a push's own window")
      (is (= :review (:action (trigger/decide (input "gh pr create") o)))
          "but well inside the create lookback"))))

;; ------------------------------------------------------------- the match

(deftest a-pushed-sha-that-is-not-the-prs-head-is-not-reviewed
  (testing "this is the `verify the sha reached the remote` test. A rejected
            push leaves no reflog entry, but a SUPERSEDED one leaves a stale
            entry whose sha the PR no longer points at — reviewing it would
            record a pass against a commit the PR does not contain"
    (is (= :silent
           (:action (trigger/decide
                     (input)
                     (opts :pushes {"/g" [(a-push "feat/x" "stalesha" 999999)]}
                           :prs {"feat/x" (a-pr 370 "differentsha")})))))))

(deftest a-branch-with-no-open-pr-is-silent
  (is (= :silent
         (:action (trigger/decide
                   (input)
                   (opts :pushes {"/g" [(a-push "feat/x" "newsha" 999999)]}
                         :prs {}))))))

(deftest with-no-recorded-push-at-all-nothing-happens
  (testing "before the pre-push hook is installed anywhere, or in a clone that
            has never pushed"
    (let [d (trigger/decide (input) (opts))]
      (is (= :silent (:action d)))
      (is (= 0 (:candidates d))))))

(deftest the-newest-push-across-every-clone-wins
  (testing "two clones of the same repo push independently; the decision takes
            the newest candidate overall, not the first clone's"
    (let [d (trigger/decide
             (input)
             (opts :pushes {"/g1" [(a-push "feat/old" "oldest" 999000)]
                            "/g2" [(a-push "feat/new" "newest" 999900)]}
                   :prs {"feat/old" (a-pr 1 "oldest")
                         "feat/new" (a-pr 2 "newest")}))]
      (is (= :review (:action d)))
      (is (= 2 (:pr d)))
      (is (= "feat/new" (:branch d)))
      (is (= "/g2" (:git-dir d)) "the ledger must live in the pushing clone")
      (is (= 2 (:candidates d))))))

(deftest the-decision-carries-the-clone-that-pushed-not-the-session
  (testing "the whole point. The ledger, lock and context all live under the
            git dir the push came from; two days of defects were this value
            being the session's repository instead"
    (let [d (trigger/decide (input)
                            (opts :pushes {"/pushed/.git" [(a-push "feat/x" "s" 999999)]}
                                  :prs {"feat/x" (a-pr 370 "s")}
                                  :worktree "/pushed"))]
      (is (= "/pushed/.git" (:git-dir d)))
      (is (= "/pushed" (:repo-root d))))))

;; ---------------------------------------------------------- ledger policy

(deftest open-pr-yields-a-review-with-pass-one
  (let [d (trigger/decide (input)
                          (opts :pushes {"/g" [(a-push "feat/x" "newsha" 999999)]}
                                :prs {"feat/x" (a-pr 370 "newsha")}))]
    (is (= :review (:action d)))
    (is (= 370 (:pr d)))
    (is (= 1 (:pass d)))
    (is (= "newsha" (:sha d)))
    (is (= "main" (:base-ref d)))
    (is (false? (:draft? d)))))

(deftest drafts-are-reviewed
  (is (= :review
         (:action (trigger/decide
                   (input)
                   (opts :pushes {"/g" [(a-push "feat/x" "newsha" 999999)]}
                         :prs {"feat/x" (a-pr 370 "newsha" :draft? true)}))))))

(deftest pass-number-comes-from-the-ledger
  (let [[_ g] (tmp-repo)]
    (ledger/append-pass! g (row 370 "sha1" 1))
    (ledger/append-pass! g (row 370 "sha2" 2))
    (is (= 3 (:pass (trigger/decide
                     (input)
                     (opts :pushes {g [(a-push "feat/x" "sha3" 999999)]}
                           :prs {"feat/x" (a-pr 370 "sha3")})))))))

(deftest cap-stops-the-loop
  (let [[_ g] (tmp-repo)]
    (doseq [n (range 1 (inc ledger/max-passes))]
      (ledger/append-pass! g (row 370 (str "sha" n) n)))
    (is (= :cap-reached
           (:action (trigger/decide
                     (input)
                     (opts :pushes {g [(a-push "feat/x" "shaN" 999999)]}
                           :prs {"feat/x" (a-pr 370 "shaN")})))))))

(deftest an-already-reviewed-sha-is-silent
  (testing "repeat pushes of one commit are ordinary — `git push` twice,
            `--tags`, `--dry-run` — and re-reviewing tells agent A nothing
            while spending a cap slot"
    (let [[_ g] (tmp-repo)]
      (ledger/append-pass! g (row 370 "samesha" 1))
      (let [d (trigger/decide (input)
                              (opts :pushes {g [(a-push "feat/x" "samesha" 999999)]}
                                    :prs {"feat/x" (a-pr 370 "samesha")}))]
        (is (= :silent (:action d)))
        (is (= 1 (:candidates d))
            "the push was seen, and rejected on the ledger rather than lost")))))

(deftest twice-raised-followup-fingerprints-are-carried-into-the-decision
  (let [[_ g] (tmp-repo)
        fp "src/a.clj:1:correctness/followup"]
    (ledger/append-pass! g (row 370 "s1" 1 :fingerprints [fp]))
    (ledger/append-pass! g (row 370 "s2" 2 :fingerprints [fp]))
    (is (= [fp] (:prior-fingerprints
                 (trigger/decide (input)
                                 (opts :pushes {g [(a-push "feat/x" "s3" 999999)]}
                                       :prs {"feat/x" (a-pr 370 "s3")})))))))

(deftest a-blocking-finding-is-never-put-on-the-do-not-re-raise-list
  (testing "a blocking finding raised twice and still unfixed must keep being
            raised; suppressing it would emit a positive signal on unreviewed
            code, which is the worst failure this system has"
    (let [[_ g] (tmp-repo)
          blocking "src/a.clj:1:correctness/blocking"
          followup "src/b.clj:2:correctness/followup"]
      (ledger/append-pass! g (row 370 "s1" 1 :fingerprints [blocking followup]))
      (ledger/append-pass! g (row 370 "s2" 2 :fingerprints [blocking followup]))
      (let [prior (:prior-fingerprints
                   (trigger/decide (input)
                                   (opts :pushes {g [(a-push "feat/x" "s3" 999999)]}
                                         :prs {"feat/x" (a-pr 370 "s3")})))]
        (is (= [followup] prior))
        (is (not (some #{blocking} prior)))))))

(deftest decide-reads-the-ledger-once
  (let [[_ g] (tmp-repo)
        reads (atom 0)]
    (ledger/append-pass! g (row 370 "s1" 1))
    (with-redefs [ledger/read-passes (let [orig ledger/read-passes]
                                       (fn [& args] (swap! reads inc) (apply orig args)))]
      (trigger/decide (input) (opts :pushes {g [(a-push "feat/x" "s2" 999999)]}
                                    :prs {"feat/x" (a-pr 370 "s2")})))
    (is (= 1 @reads)
        "the policy predicates are pure over an already-read collection; each
         re-reading the file cost 138 full reads for one decision")))

(deftest findings-message-is-self-describing
  (testing "the harness wrapper text is fixed and useless, so the first line
            must identify repo, PR and pass on its own"
    (let [msg (trigger/findings-message
               {:branch "feat/x" :pr 370 :pass 2}
               {:verdict "NOT MERGEABLE" :body "BODY"
                :counts {"correctness/blocking" 1}})]
      (is (str/starts-with? msg "pr-review-loop"))
      (is (str/includes? msg "PR #370"))
      (is (str/includes? msg "pass 2"))
      (is (str/includes? msg "BODY"))
      (is (str/includes? msg "feat/x PR #370")
          "the branch must appear right before the PR number — a regression
           that dropped it would still satisfy every assertion above it. The
           branch, not the clone directory: every review now runs in a
           throwaway checkout whose name says nothing about the work"))))

(deftest findings-message-carries-parse-warnings
  (let [msg (trigger/findings-message
             {:branch "feat/x" :pr 1 :pass 1}
             {:verdict "NOT MERGEABLE" :body "BODY" :counts {}}
             ["PARSE WARNING TEXT"])]
    (is (str/includes? msg "PARSE WARNING TEXT")
        "a review whose findings carry no fingerprints cannot converge; agent
         A has to be told")))

;; --------------------------------------------------------------- review!

(deftest a-completed-review-records-one-pass-and-wakes-the-session
  (let [[r g] (tmp-repo)
        d {:repo-root r :git-dir g :pr 370 :pass 1 :sha "headsha"
           :branch "feat/x" :base-ref "main" :draft? false
           :prior-fingerprints []}
        result (#'trigger/review! d (review-opts))]
    (is (= 2 (:exit result)))
    (is (= ["headsha"] (mapv :sha (ledger/read-passes g 370))))
    (is (= "MERGEABLE" (:verdict (first (ledger/read-passes g 370)))))
    (is (nil? (lock/read-lock g 370)) "the lock is released on the success path")))

(deftest the-ledger-row-records-the-reconciled-verdict-not-the-claimed-one
  (testing "mergeable? had zero production call sites: any output whose count
            block contradicted its verdict line yielded a MERGEABLE headline
            and a self-contradictory ledger row (verdict MERGEABLE,
            blocking 1)"
    (let [[r g] (tmp-repo)
          contradictory (str "VERDICT: MERGEABLE — nothing to fix\n\n"
                             "  [correctness/blocking]  1\n"
                             "  [correctness/followup]  none\n"
                             "  [coverage]              none\n"
                             "  [docs-accuracy]         none\n"
                             "  [style]                 none\n\n"
                             "1. [correctness/blocking] src/a.clj:7 — boom\n")
          d {:repo-root r :git-dir g :pr 1 :pass 1 :sha "s" :base-ref "main"
             :draft? false :prior-fingerprints []}
          result (#'trigger/review! d (review-opts :out contradictory))]
      (is (= 2 (:exit result)))
      (is (str/includes? (:message result) "NOT MERGEABLE")
          "the headline agent A reads must be the reconciled verdict")
      (is (= "NOT MERGEABLE" (:verdict (first (ledger/read-passes g 1))))
          "and so must the ledger row, or the next pass reads a clean history")
      (is (= 1 (:blocking (first (ledger/read-passes g 1))))))))

(deftest unresolved-base-ref-skips-the-reviewer-and-the-ledger
  (let [[r g] (tmp-repo)
        d {:repo-root r :git-dir g :pr 370 :pass 1 :sha "headsha" :base-ref "main"
           :draft? false :prior-fingerprints []}
        ;; :spawn-fn is a safety net, not the point of the test: if a
        ;; regression ever let review! reach the reviewer on this path, this
        ;; stub keeps the test from shelling out to a real `claude -p`.
        result (#'trigger/review! d (assoc (review-opts)
                                           :merge-base-fn (constantly nil)
                                           :diff-fn (constantly nil)))]
    (is (= 2 (:exit result)))
    (is (str/includes? (:message result) "main")
        "the diagnostic must name the unresolved base ref")
    (is (str/includes? (:message result) "git fetch origin main")
        "the diagnostic must give the concrete command that fixes it")
    (is (empty? (ledger/read-passes g 370))
        "no findings were produced; spending one of the ten cap slots on a
         pass that never reviewed anything would let a PR reach \"cap
         reached\" without a single real review")))

(deftest a-crashed-reviewer-does-not-consume-a-cap-slot
  (testing "six pushes against an expired token wrote six MALFORMED rows and,
            with four real passes, permanently exhausted the PR's budget. The
            unresolved-base-ref path already refuses to record a pass that
            reviewed nothing; the treatment must be identical"
    (let [[r g] (tmp-repo)
          d {:repo-root r :git-dir g :pr 370 :pass 1 :sha "headsha"
             :branch "feat/x" :base-ref "main" :draft? false
             :prior-fingerprints []}
          result (#'trigger/review! d (review-opts :exit 1 :out ""
                                                   :err "OAuth token has expired"))]
      (is (= 2 (:exit result)) "agent A must still be woken")
      (is (str/includes? (:message result) "OAuth token has expired")
          "the real diagnosis is in the reviewer's stderr and is otherwise unread")
      (is (str/includes? (:message result) "exited 1")
          "the exit code must be named")
      (is (str/includes? (:message result) "did not consume")
          "and the message must say no slot was spent, because none was")
      (is (empty? (ledger/read-passes g 370))))))

(deftest a-malformed-reply-does-not-consume-a-cap-slot
  (let [[r g] (tmp-repo)
        d {:repo-root r :git-dir g :pr 370 :pass 1 :sha "headsha"
           :branch "feat/x" :base-ref "main" :draft? false
           :prior-fingerprints []}
        result (#'trigger/review! d (review-opts :out "I could not read the diff"))]
    (is (= 2 (:exit result)))
    (is (str/includes? (:message result) "MALFORMED"))
    (is (str/includes? (:message result) "I could not read the diff"))
    (is (empty? (ledger/read-passes g 370))
        "a reply with no verdict produced no findings, so it buys no slot")))

(deftest a-superseded-trigger-goes-quiet-and-records-nothing
  (testing "kill-reviewers! kills the reviewer child, not the trigger, so the
            loser returns from a reviewer that was SIGTERMed mid-answer.
            Recording that would spend a slot and wake agent A with findings
            for a SHA that is already stale"
    (let [[r g] (tmp-repo)
          d {:repo-root r :git-dir g :pr 370 :pass 1 :sha "old"
             :base-ref "main" :draft? false :prior-fingerprints []}
          ;; A newer push takes the lock while our reviewer is running.
          steal (fn [_ _ _ _]
                  (lock/acquire! g {:pr 370 :sha "new" :branch "feat/x"} {:pid 9999})
                  {:exit 143 :out "" :err "terminated"})
          result (#'trigger/review! d (review-opts :spawn-fn steal))]
      (is (= 0 (:exit result)) "silence, not a wake")
      (is (nil? (:message result)))
      (is (empty? (ledger/read-passes g 370)))
      (is (= 9999 (:pid (lock/read-lock g 370)))
          "and the loser must not have deleted the winner's lock record"))))

(deftest a-duplicate-push-is-silent
  (let [[r g] (tmp-repo)
        self (.pid (java.lang.ProcessHandle/current))]
    (spit (lock/lock-path g 370)
          (str "{\"pid\":" self ",\"pr\":370,\"sha\":\"headsha\",\"started\":1}"))
    (let [d {:repo-root r :git-dir g :pr 370 :pass 1 :sha "headsha"
             :branch "feat/x" :base-ref "main" :draft? false
             :prior-fingerprints []}
          result (#'trigger/review! d (review-opts :pid 1))]
      (is (= 0 (:exit result)))
      (is (empty? (ledger/read-passes g 370))))))

;; -------------------------------------------------- the exit-code contract

(deftest review-exit-is-always-zero-or-two
  (testing "the one property this module exists to guarantee, and it had no
            test. Any other exit code makes Claude Code print `Failed with
            non-blocking status code:` and the review pass is silently lost"
    (let [[r g] (tmp-repo)
          d {:repo-root r :git-dir g :pr 370 :pass 1 :sha "headsha"
             :branch "feat/x" :base-ref "main" :draft? false
             :prior-fingerprints []}
          check (fn [label thunk]
                  (is (contains? #{0 2} (:exit (thunk))) label))]
      (check "clean review" #(#'trigger/review! d (review-opts)))
      (check "reviewer crash" #(#'trigger/review! d (review-opts :exit 127 :err "no claude")))
      (check "malformed reply" #(#'trigger/review! d (review-opts :out "nope")))
      (check "unresolved base ref"
             #(#'trigger/review! d (assoc (review-opts) :diff-fn (constantly nil)
                                          :merge-base-fn (constantly nil))))
      (check "throwing lock/acquire!"
             (fn [] (with-redefs [lock/acquire! (fn [& _] (throw (ex-info "flock exploded" {})))]
                      (#'trigger/review! d (review-opts)))))
      (check "throwing lock/release!"
             (fn [] (with-redefs [lock/release! (fn [& _] (throw (ex-info "release exploded" {})))]
                      (#'trigger/review! d (review-opts)))))
      (check "throwing context/build!"
             (fn [] (#'trigger/review!
                     d (assoc (review-opts)
                              :diff-fn (fn [& _] (throw (ex-info "git exploded" {})))))))
      (check "throwing ledger/append-pass!"
             (fn [] (with-redefs [ledger/append-pass! (fn [& _] (throw (ex-info "disk full" {})))]
                      (#'trigger/review! d (review-opts))))))))

(deftest a-throwing-acquire-still-wakes-the-session-with-the-diagnosis
  (let [[r g] (tmp-repo)
        d {:repo-root r :git-dir g :pr 370 :pass 1 :sha "headsha"
           :branch "feat/x" :base-ref "main" :draft? false
           :prior-fingerprints []}
        result (with-redefs [lock/acquire! (fn [& _] (throw (ex-info "flock exploded" {})))]
                 (#'trigger/review! d (review-opts)))]
    (is (= 2 (:exit result))
        "acquire! used to be evaluated in the `case` head, OUTSIDE the try, so
         a throw there escaped review! and -main and babashka exited 1")
    (is (str/includes? (:message result) "flock exploded"))
    (is (str/includes? (:message result) "PR #370"))))

(deftest a-throwing-release-does-not-swallow-a-completed-review
  (let [[r g] (tmp-repo)
        d {:repo-root r :git-dir g :pr 370 :pass 1 :sha "headsha"
           :branch "feat/x" :base-ref "main" :draft? false
           :prior-fingerprints []}
        result (with-redefs [lock/release! (fn [& _] (throw (ex-info "release exploded" {})))]
                 (#'trigger/review! d (review-opts)))]
    (is (= 2 (:exit result)))
    (is (str/includes? (:message result) "MERGEABLE")
        "a throw out of a `finally` replaces the value the body already
         computed, so an unwrapped release! discards a completed review's
         findings and reports a crash instead")
    (is (= 1 (count (ledger/read-passes g 370))))))


;; ------------------------------------------------------ the pinned checkout

(deftest a-review-that-cannot-be-pinned-to-its-sha-is-refused
  (testing "falling back to the agent's live worktree is the defect the
            checkout exists to remove — the agent is still editing it — so a
            failed checkout must refuse rather than quietly restore it, and
            must not spend a ledger slot"
    (let [[r g] (tmp-repo)
          d {:repo-root r :git-dir g :pr 370 :pass 1 :sha "headsha0123456"
             :branch "feat/x" :base-ref "main" :draft? false
             :prior-fingerprints []}
          result (#'trigger/review! d (review-opts :checkout :none))]
      (is (= 2 (:exit result)) "agent A still has to be told")
      (is (str/includes? (:message result) "could not check out"))
      (is (str/includes? (:message result) "No ledger row"))
      (is (empty? (ledger/read-passes g 370)))
      (is (nil? (lock/read-lock g 370)) "and the lock is still released"))))

(deftest the-reviewer-runs-in-the-checkout-not-the-agents-tree
  (let [[r g] (tmp-repo)
        seen (atom nil)
        d {:repo-root r :git-dir g :pr 370 :pass 1 :sha "headsha"
           :branch "feat/x" :base-ref "main" :draft? false
           :prior-fingerprints []}]
    (#'trigger/review! d (assoc (review-opts :checkout "/pinned-tree")
                                ;; spawn is (argv prompt dir)
                                :spawn-fn (fn [_argv _prompt dir _err]
                                            (reset! seen dir)
                                            {:exit 0 :out (clean-reply) :err ""})))
    (is (= "/pinned-tree" @seen)
        "the reviewer was pointed at the agent's worktree, which mutates
         under it — two runs of one pass were not reproducible")))

(deftest a-killed-review-is-retried-outside-every-window
  (testing "measured: a session restart at 10:36 killed a review started at
            10:32. It left a lock naming a dead pid, no ledger row and no
            message, and the PR was never reviewed again because nothing
            looked for it. The push is minutes or hours old and the session
            that made it is gone, so any window or session test means it is
            never retried"
    (let [o (-> (opts :pushes {}            ; nothing recent in any reflog
                      :prs {"feat/x" (a-pr 370 "killedsha")})
                (assoc :clones-fn (fn [_log _since] ["/g"])
                       :abandoned-fn (fn [_gd]
                                       [{:pr 370 :sha "killedsha"
                                         :branch "feat/x" :pid 999999
                                         :started 1}])))
          d (trigger/decide (input "git status") o)]
      (is (= :review (:action d)) "an interrupted review must be picked up")
      (is (true? (:retry? d)))
      (is (= 370 (:pr d)))
      (is (= "killedsha" (:sha d)))
      (is (= 1 (:pass d)) "and it must still be pass 1 — none was recorded"))))

(deftest a-retry-survives-a-clone-index-entry-days-old
  (testing "no stubs on the index or the lock: a real pushes.log timestamped
            three days back and a real lock naming a dead pid. Applying the
            fresh path's lookback here would mean an interrupted review is
            never retried, which is the whole defect"
    (let [[_ g] (tmp-repo)
          log (str (fs/path (fs/create-temp-dir {:prefix "pr-review-oldidx"})
                            "pushes.log"))
          three-days-ago (- (quot (System/currentTimeMillis) 1000) (* 3 86400))]
      (spit log (format "%d\t%s\tsess-old\n" three-days-ago g))
      (lock/acquire! g {:pr 370 :sha "killedsha" :branch "feat/x"} {:pid 999999})
      (let [d (trigger/decide
               (input "git status")
               {:log-fn (constantly log)
                ;; tmp-repo's .git holds no repository, so `git worktree list`
                ;; cannot answer for it; that resolution is covered by
                ;; gh-test and by the real-clone end-to-end below.
                :main-worktree-fn (fn [gd _] (str (fs/parent gd)))
                :open-pr-fn (fn [_ branch _]
                              (when (= "feat/x" branch) (a-pr 370 "killedsha")))
                :now-fn (constantly (System/currentTimeMillis))})]
        (is (= :review (:action d)))
        (is (true? (:retry? d)))
        (is (= 370 (:pr d)))
        (is (= "killedsha" (:sha d)))))))

(deftest a-retry-is-not-offered-once-the-pr-has-moved-on
  (testing "an abandoned review is only worth retrying while it is still the
            PR's head with no ledger row; `actionable` decides that, so the
            retry path cannot resurrect stale work"
    (let [[_ g] (tmp-repo)
          mk (fn [prs] (-> (opts :pushes {} :prs prs)
                           (assoc :clones-fn (fn [_ _] [g])
                                  :abandoned-fn (fn [_] [{:pr 370 :sha "killedsha"
                                                          :branch "feat/x" :pid 999999
                                                          :started 1}]))))]
      (is (= :silent (:action (trigger/decide (input) (mk {}))))
          "the PR was closed")
      (is (= :silent (:action (trigger/decide (input) (mk {"feat/x" (a-pr 370 "newer")}))))
          "the PR head moved past the killed review's sha")
      (ledger/append-pass! g (row 370 "killedsha" 1))
      (is (= :silent (:action (trigger/decide (input) (mk {"feat/x" (a-pr 370 "killedsha")}))))
          "the review did in fact complete and record a row"))))

(deftest a-live-review-is-never-treated-as-abandoned
  ;; `lock/abandoned` filters on pid liveness; this asserts the trigger does
  ;; not second-guess it into retrying work still in flight.
  (let [o (-> (opts :pushes {} :prs {"feat/x" (a-pr 370 "sha")})
              (assoc :clones-fn (fn [_ _] ["/g"])
                     :abandoned-fn (fn [_] [])))]
    (is (= :silent (:action (trigger/decide (input) o))))))

(deftest the-lock-record-carries-the-branch-a-retry-needs
  (testing "the wiring the stubs hid. `abandoned-candidates` skips any record
            without a branch — it has no other way to find the PR — so a
            review! that does not thread :branch into acquire! makes the whole
            retry path dead code. Measured in production: two killed reviews
            left locks with branch=null while `lock/abandoned` returned both
            and `decide` returned 0 candidates.

            The lock is read from INSIDE the reviewer, which is the only
            moment review! holds it. An earlier version of this test
            re-acquired the lock itself and so asserted nothing about
            review!'s own arguments"
    (let [[r g] (tmp-repo)
          held (atom :never-ran)
          d {:repo-root r :git-dir g :pr 370 :pass 1 :sha "headsha"
             :branch "feat/x" :base-ref "main" :draft? false
             :prior-fingerprints []}]
      (#'trigger/review!
       d (assoc (review-opts)
                :spawn-fn (fn [_ _ _ _]
                            (reset! held (lock/read-lock g 370))
                            {:exit 0 :out (clean-reply) :err ""})))
      (is (not= :never-ran @held) "the reviewer never ran, so nothing was checked")
      (is (= "feat/x" (:branch @held))
          "review! must pass :branch to acquire!, or no killed review is ever retried")
      (is (= "headsha" (:sha @held)))
      (is (= 370 (:pr @held))))))

(deftest a-real-abandoned-lock-is-retried-end-to-end
  (testing "no abandoned-fn stub: a real lock written by the real acquire!,
            read by the real lock/abandoned, through the real decide"
    (let [[_ g] (tmp-repo)
          log (str (fs/path (fs/create-temp-dir {:prefix "pr-review-retrye2e"})
                            "pushes.log"))]
      (spit log (format "%d\t%s\tsess-x\n" (quot (System/currentTimeMillis) 1000) g))
      (lock/acquire! g {:pr 370 :sha "killedsha" :branch "feat/x"} {:pid 999999})
      (let [d (trigger/decide
               (input "echo not-a-push")
               {:log-fn (constantly log)
                :main-worktree-fn (fn [gd _] (str (fs/parent gd)))
                :open-pr-fn (fn [_ b _] (when (= "feat/x" b) (a-pr 370 "killedsha")))
                :now-fn (constantly (System/currentTimeMillis))})]
        (is (= :review (:action d)))
        (is (true? (:retry? d)))
        (is (= 370 (:pr d)))
        (is (= "feat/x" (:branch d)))))))

;; -------------------------------------------------------- the real history

(deftest the-two-pushes-the-command-parser-missed-are-reviewable
  (testing "verbatim from claude-code-http-proxy. Both pushes resolved to the
            wrong repository under command parsing and went unreviewed; both
            match their PR head exactly, which is all this rule asks"
    (let [[_ g] (tmp-repo)
          pr395 "ceab8f69668700036cd8e7c6ebe9705b02437f30"
          pr397 "0993247b000000000000000000000000000000aa"
          o (opts :pushes {g [(a-push "fix/slack-per-file-hold-attempts" pr395 999900)
                              (a-push "fix/finish-the-monotonic-sweep" pr397 999800)]}
                  :prs {"fix/slack-per-file-hold-attempts" (a-pr 395 pr395)
                        "fix/finish-the-monotonic-sweep" (a-pr 397 pr397)})]
      (let [d (trigger/decide (input) o)]
        (is (= :review (:action d)))
        (is (= 395 (:pr d)) "the newest push is #395's")
        (is (= pr395 (:sha d))))
      (testing "and once #395 has a row, the next trigger takes #397"
        (ledger/append-pass! g (row 395 pr395 1))
        (let [d (trigger/decide (input) o)]
          (is (= :review (:action d)))
          (is (= 397 (:pr d)))
          (is (= pr397 (:sha d))))))))

;; --------------------------------------------------------------- end to end

(deftest a-real-clone-with-a-real-reflog-decides-a-review
  (testing "no stubs between the reflog and the decision: a real clone, a real
            pre-push-shaped reflog entry, and a real clone index"
    (let [root (str (fs/create-temp-dir {:prefix "pr-review-e2e"}))]
      (p/sh ["git" "init" "-q" "--initial-branch=main" root])
      (let [g (str (fs/path root ".git"))
            sha (str/join (repeat 40 \a))
            ref (fs/path g "logs" "refs" "remotes" "origin" "feat" "x")
            log (str (fs/path (fs/create-temp-dir {:prefix "pr-review-e2e-log"})
                              "pushes.log"))
            now (System/currentTimeMillis)]
        (fs/create-dirs (fs/parent ref))
        (spit (str ref)
              (format "%s %s N <a@b> %d +0800\tupdate by push\n"
                      (str/join (repeat 40 \0)) sha (quot now 1000)))
        ;; exactly what the pre-push hook writes: a clone line, then one
        ;; attempt line per ref, with the pushing session's id
        (spit log (str (format "%d\t%s\tsess-1\n" (quot now 1000) g)
                       (format "%d\t%s\tsess-1\trefs/heads/feat/x\t%s\n"
                               (quot now 1000) g sha)))
        (let [d (trigger/decide
                 (input)
                 {:log-fn (constantly log)
                  :open-pr-fn (fn [_ branch _]
                                (when (= "feat/x" branch) (a-pr 370 sha)))
                  :now-fn (constantly now)})]
          (is (= :review (:action d)))
          (is (= 370 (:pr d)))
          (is (= "feat/x" (:branch d)))
          (is (= sha (:sha d)))
          (is (= (str (fs/canonicalize g)) (str (fs/canonicalize (:git-dir d))))
              "git-dir must be the clone the index named")
          (is (= (str (fs/canonicalize root)) (str (fs/canonicalize (:repo-root d))))
              "repo-root must come from `git worktree list`, not <git-dir>/.."))))))
