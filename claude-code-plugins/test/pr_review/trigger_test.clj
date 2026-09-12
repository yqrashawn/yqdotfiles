(ns pr-review.trigger-test
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [pr-review.context :as context]
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
     ;; Never really sleep in tests: the sha-mismatch path waits for GitHub to
     ;; catch up, and a suite that pays that is a suite nobody runs.
     :sleep-fn         (fn [_] nil)
     :rand-fn          (fn [_] 0)
     :reviewer-env-fn  (constantly nil)
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

(def ^:private last-checkout-pr (atom ::never-called))

(defn- review-opts
  "opts for `review!`: a stubbed diff, a stubbed reviewer and a stubbed
   checkout, so nothing shells out to a real `claude -p` or `git worktree`."
  [& {:keys [out exit err pid spawn-fn checkout]}]
  {:post-comment-fn (fn [_root n _body _opts]
                      (str "https://github.com/o/r/pull/" n "#issuecomment-1"))
   :merge-base-fn (constantly "basesha")
   :diff-fn (constantly "diff --git a/a b/a\n")
   :pid (or pid 4242)
   :with-checkout-fn (fn [_gd pr _sha _parent _opts f]
                       ;; the PR is RECORDED, not discarded. Discarding it is
                       ;; why `run-review!` passing an unbound `pr` — which
                       ;; resolved to clojure.core/pr — went unseen by a green
                       ;; suite while two PRs at one sha shared a directory.
                       (reset! last-checkout-pr pr)
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

(deftest a-review-must-not-review
  (testing "the reviewer is a plain `claude -p`, so it loads this very plugin
            and its Bash calls fire this hook. Measured: a review-trigger
            process spawns inside the reviewer, and the reviewer uses Bash
            heavily now — it runs test suites.

            Most of those triggers would go silent, but the abandoned-review
            path is deliberately neither session-scoped nor verb-gated, so one
            can find outstanding work and start a REVIEW INSIDE A REVIEW: a
            grandchild that dies when the outer reviewer exits, leaving a dead
            lock and a leaked worktree. PR #406's worktree was last written at
            21:11:33 and PR #405's review completed at 21:11:35"
    (let [o (assoc (opts :pushes {"/g" [(a-push "feat/x" "newsha" 999999)]}
                         :prs {"feat/x" (a-pr 370 "newsha")})
                   :reviewer-env-fn (constantly "1"))
          d (trigger/decide (input) o)]
      (is (= :silent (:action d)))
      (is (str/includes? (:reason d) "inside a reviewer")))
    (testing "and it refuses before doing ANY work — no git, no gh, no ledger"
      (let [touched (atom [])
            o (assoc (opts) :reviewer-env-fn (constantly "1")
                     :attempts-fn (fn [& _] (swap! touched conj :attempts) [])
                     :pushes-fn (fn [& _] (swap! touched conj :pushes) [])
                     :clones-fn (fn [& _] (swap! touched conj :clones) [])
                     :abandoned-fn (fn [& _] (swap! touched conj :abandoned) []))]
        (trigger/decide (input) o)
        (is (empty? @touched))))))

(deftest outside-a-reviewer-the-marker-is-absent-and-work-proceeds
  ;; Guards the default: reading the env var wrongly would make the loop inert
  ;; everywhere, which is the failure mode that costs the most and shows least.
  (let [o (opts :pushes {"/g" [(a-push "feat/x" "newsha" 999999)]}
                :prs {"feat/x" (a-pr 370 "newsha")})]
    (is (= :review (:action (trigger/decide (input) o))))))

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

(deftest a-lagging-pr-head-is-waited-for-not-dropped
  (testing "measured: a push recorded at 20:25:31 had its trigger fire at
            20:25:32, and the PR's updated_at shows GitHub moved the head at
            20:25:34. `gh pr list` takes ~1.3s on top, so the query lands
            inside the propagation window. The mismatch made the trigger drop
            the push in silence — and with no lock written, nothing retried it,
            so agent A's `pass 2 will run on it` never happened"
    (let [calls (atom 0)
          slept (atom 0)
          o (assoc (opts :pushes {"/g" [(a-push "feat/x" "newsha" 999999)]})
                   ;; GitHub answers with the OLD head twice, then catches up
                   :open-pr-fn (fn [_ _ _]
                                 (swap! calls inc)
                                 (a-pr 370 (if (< @calls 3) "oldsha" "newsha")))
                   :sleep-fn (fn [ms] (swap! slept + ms)))
          d (trigger/decide (input) o)]
      (is (= :review (:action d)))
      (is (= "newsha" (:sha d)))
      (is (= 3 @calls) "it must re-ask, not accept the first answer")
      (is (pos? @slept) "and wait between asks"))))

(deftest the-wait-backs-off-and-is-jittered
  (testing "a fixed delay BEFORE asking — the obvious alternative — pays its
            full cost on every trigger including those with nothing to review,
            and still drops the push whenever GitHub takes longer than the
            guess. Backing off costs nothing when the head is already right
            and returns as soon as GitHub catches up.

            The jitter is for concurrent pushes, which this workflow does
            constantly: three sessions pushing within a minute would otherwise
            line their retries up and ask GitHub in lockstep"
    (let [waits (atom [])
          o (assoc (opts :pushes {"/g" [(a-push "feat/x" "newsha" 999999)]})
                   :open-pr-fn (fn [_ _ _] (a-pr 370 "never-matches"))
                   :sleep-fn (fn [ms] (swap! waits conj ms))
                   :rand-fn (constantly 0))]
      (trigger/decide (input) o)
      (is (= [1000 2000 4000 8000 8000] @waits)
          "exponential from 1s, capped at 8s")
      (is (= 23000 (reduce + @waits)) "about 23s of patience, plus jitter"))
    (testing "and the jitter really is added"
      (let [waits (atom [])
            o (assoc (opts :pushes {"/g" [(a-push "feat/x" "newsha" 999999)]})
                     :open-pr-fn (fn [_ _ _] (a-pr 370 "never-matches"))
                     :sleep-fn (fn [ms] (swap! waits conj ms))
                     :rand-fn (constantly 777))]
        (trigger/decide (input) o)
        (is (every? #(= 777 (mod % 1000)) @waits))))))

(deftest a-superseded-push-is-dropped-once-the-attempts-run-out
  (testing "the same mismatch has two causes needing opposite handling —
            GitHub has not caught up (wait) or this push was superseded by a
            newer one (drop). Only time tells them apart, so it waits a
            bounded amount and then drops; the newer push has its own trigger"
    (let [calls (atom 0)
          o (assoc (opts :pushes {"/g" [(a-push "feat/x" "stalesha" 999999)]})
                   :open-pr-fn (fn [_ _ _] (swap! calls inc) (a-pr 370 "newer")))
          d (trigger/decide (input) o)]
      (is (= :silent (:action d)))
      (is (= 6 @calls) "bounded, and it does not spin"))))

(deftest a-branch-with-no-open-pr-does-not-wait-at-all
  ;; No PR is a final answer, not a lag: waiting on it would put 8 seconds
  ;; into every Bash call whose branch has no PR.
  (let [slept (atom 0)
        o (assoc (opts :pushes {"/g" [(a-push "feat/x" "newsha" 999999)]} :prs {})
                 :sleep-fn (fn [ms] (swap! slept + ms)))]
    (is (= :silent (:action (trigger/decide (input) o))))
    (is (zero? @slept))))

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

(deftest the-trigger-records-that-it-asked-for-a-wake
  (testing "nothing recorded the trigger's own exit, so a missing wake was
            indistinguishable between the trigger never getting there and the
            harness dropping it. Measured on PR #409: the review completed,
            wrote its ledger row and findings file, and no session's queue
            ever received the message — while a different review's wake WAS
            delivered 57 seconds later into the same session, idle since seven
            minutes before. Both explanations fit, and they need different
            fixes"
    (let [[_ g] (tmp-repo)
          rec (resolve 'pr-review.trigger/record-wake!)]
      (rec {:git-dir g :pr 409 :action :review :retry? true} 2 "findings" "sess-a")
      (rec {:git-dir g :pr 410 :action :silent} 0 nil "sess-a")
      (let [lines (str/split-lines (slurp (trigger/wake-log-path g)))]
        (is (= 2 (count lines)))
        (is (str/includes? (first lines) "409\treview\tretry\tsess-a\texit=2\tmsg=8")
            "exit=2 with a non-zero message length is the trigger saying it
             asked for a wake")
        (is (str/includes? (second lines) "410\tsilent\tfresh\tsess-a\texit=0\tmsg=0"))))))

(deftest finish-both-wakes-the-session-and-records-that-it-did
  (testing "the wiring, not the function: -main calls System/exit so nothing
            in it is testable, and a control that deleted the wake record from
            -main failed no test at all. finish! is the seam"
    (let [[_ g] (tmp-repo)
          d {:git-dir g :pr 409 :action :review :retry? true}
          err (java.io.StringWriter.)
          exit (binding [*err* err]
                 (trigger/finish! d {:exit 2 :message "FINDINGS"} "sess-a"))]
      (is (= 2 exit) "the exit code must pass straight through")
      (is (str/includes? (str err) "FINDINGS")
          "the wake goes on stderr — the harness discards stdout")
      (is (str/includes? (slurp (trigger/wake-log-path g)) "exit=2\tmsg=8")
          "and the same call must record that it asked"))))

(deftest finish-on-a-silent-decision-writes-no-wake-but-still-records
  (let [[_ g] (tmp-repo)
        err (java.io.StringWriter.)
        exit (binding [*err* err]
               (trigger/finish! {:git-dir g :pr 410 :action :silent}
                                {:exit 0 :message nil} "sess-a"))]
    (is (= 0 exit))
    (is (= "" (str err)) "silence means nothing on stderr")
    (is (str/includes? (slurp (trigger/wake-log-path g)) "exit=0\tmsg=0")
        "a silent decision is still worth a line — it says the trigger ran")))

(deftest recording-a-wake-can-never-change-the-exit-code
  (testing "housekeeping runs after the exit code is decided; a failure here
            must not become a failed hook, which is how a pass gets lost"
    (let [rec (resolve 'pr-review.trigger/record-wake!)]
      (is (nil? (rec {:git-dir "/nonexistent/deep/path" :pr 1 :action :review}
                     2 "m" "s"))
          "an unwritable path must be swallowed")
      (is (nil? (rec {:pr 1 :action :silent} 0 nil nil))
          "and a decision with no git-dir has nowhere to write, which is fine"))))

(deftest a-mergeable-verdict-tells-the-agent-it-may-merge
  (testing "measured on PR #410: two MERGEABLE passes, both delivered, and the
            agent fixed the [coverage] finding and pushed again both times
            instead of merging — because nothing said MERGEABLE meant it may.
            On a test-only PR every pass finds another coverage nit, and
            coverage findings are never suppressed by the re-raise rules, so
            it cannot converge on its own"
    (let [msg (trigger/findings-message
               {:branch "test/x" :pr 410 :pass 2 :git-dir "/g"}
               {:verdict "MERGEABLE" :body "BODY"
                :counts {"correctness/blocking" 0 "coverage" 1}})]
      (is (str/includes? msg "you may merge"))
      (is (str/includes? msg "Only correctness findings are work you owe")
          "the categories are not equal, and saying so is what ends the loop")
      (is (str/includes? msg "your judgment")
          "coverage, docs and style are offered, not assigned")
      (is (str/includes? msg "agree STRONGLY")
          "mere agreement is too low a bar — conceding a nit still costs a
           commit and earns another pass carrying the next one")
      (is (str/includes? msg "No follow-up PR is owed")
          "or A carries every nit forward forever instead of dropping it")
      (is (str/includes? msg "follow-up PR; do not fix it here")
          "correctness/followup is the one that DOES oblige a new PR")
      (is (str/includes? msg "certifies a property it does not check")
          "the carve-out: a test that cannot fail emits a false safety
           signal, so that coverage finding is not merely optional")
      (is (str/includes? msg "does not terminate")
          "the reason another fix push is the wrong move has to be given")
      (is (not (str/includes? msg "verify each blocking finding"))
          "the NOT MERGEABLE instruction must not also appear"))))

(deftest the-wake-names-the-session-that-pushed
  (testing "PR #409's review completed 15 minutes after its session went idle
            and no session received the wake, so whoever does read the
            findings has to be told whose PR it is"
    (let [msg (trigger/findings-message
               {:branch "test/x" :pr 410 :pass 1 :git-dir "/g"
                :pushed-by "334b7e62-afb0-44b4-82a1-cff0dc86ff20"}
               {:verdict "MERGEABLE" :body "BODY" :counts {}})]
      (is (str/includes? msg "pushed by session 334b7e62-afb0-44b4-82a1-cff0dc86ff20"))
      (is (str/includes? msg "hand that path over")))
    (testing "and says nothing misleading when there is no session to name"
      (let [msg (trigger/findings-message
                 {:branch "test/x" :pr 410 :pass 1 :git-dir "/g"}
                 {:verdict "MERGEABLE" :body "BODY" :counts {}})]
        (is (not (str/includes? msg "pushed by session")))
        (is (str/includes? msg "/g/pr-review.410.findings.md"))))))

(deftest a-not-mergeable-verdict-tells-the-agent-to-fix-first
  (let [msg (trigger/findings-message
             {:branch "test/x" :pr 410 :pass 1 :git-dir "/g"}
             {:verdict "NOT MERGEABLE" :body "BODY"
              :counts {"correctness/blocking" 1}})]
    (is (str/includes? msg "at least one [correctness/blocking] finding stands"))
    (is (str/includes? msg "fix the class"))
    (is (not (str/includes? msg "you may merge"))
        "a blocking finding must never carry a merge instruction")))

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

(deftest the-findings-text-is-written-where-another-session-can-read-it
  (testing "the findings lived only in the wake. The ledger keeps verdict,
            counts and fingerprints — enough to DECIDE, not enough to READ —
            so a review whose wake was lost, or one run by hand in a different
            session, could not be handed to whoever is working the PR"
    (let [[r g] (tmp-repo)
          d {:repo-root r :git-dir g :pr 370 :pass 1 :sha "headsha"
             :branch "feat/x" :base-ref "main" :draft? false
             :prior-fingerprints []}
          result (#'trigger/review! d (review-opts))
          path (trigger/findings-path g 370)]
      (is (fs/exists? path) "no findings file was written")
      (is (str/starts-with? (:message result) (slurp path))
          "the file must hold what the wake said, or the handoff is lossy —
           the wake adds only where the review was posted, which is about this
           machine rather than about the review")
      (is (str/includes? (slurp path) "PR #370"))
      (testing "and the wake points at it, so agent A can hand the path on"
        (is (str/includes? (:message result) path))))))

(deftest the-review-is-posted-to-the-pr-by-the-trigger
  (testing "the reviewer generates the review once and never posts it —
            `Bash(gh pr:*)` is denied to it — so the trigger posts, after the
            ledger row and the findings file are already on disk"
    (let [[r g] (tmp-repo)
          seen (atom nil)
          d {:repo-root r :git-dir g :pr 370 :pass 1 :sha "headsha0123456"
             :branch "feat/x" :base-ref "main" :draft? false
             :prior-fingerprints []}
          result (#'trigger/review!
                  d (assoc (review-opts)
                           :post-comment-fn (fn [root n body _]
                                              (reset! seen {:root root :n n :body body})
                                              "https://x/1")))]
      (is (= r (:root @seen)) "posted from the clone, not the throwaway checkout")
      (is (= 370 (:n @seen)))
      (is (str/includes? (:body @seen) "MERGEABLE") "the verdict must be in it")
      (is (str/includes? (:body @seen) "headsha01234")
          "and the sha, so a reader knows which push this reviewed")
      (is (str/includes? (:body @seen) "pass 1"))
      (testing "and the comment carries none of the wake's instructions to A"
        (is (not (str/includes? (:body @seen) "pr-review-loop skill")))
        (is (not (str/includes? (:body @seen) "hand that path over"))))
      (testing "the wake reports where it went"
        (is (str/includes? (:message result) "https://x/1"))))))

(deftest a-failed-post-does-not-lose-the-review
  (testing "posting is last and cosmetic: the ledger row and the findings file
            are already written, so a GitHub outage costs the convenience of
            reading the review there and nothing else"
    (let [[r g] (tmp-repo)
          d {:repo-root r :git-dir g :pr 370 :pass 1 :sha "headsha"
             :branch "feat/x" :base-ref "main" :draft? false
             :prior-fingerprints []}
          result (#'trigger/review!
                  d (assoc (review-opts) :post-comment-fn (fn [& _] nil)))]
      (is (= 2 (:exit result)) "agent A is still woken")
      (is (= 1 (count (ledger/read-passes g 370))) "the pass still counts")
      (is (fs/exists? (trigger/findings-path g 370)))
      (is (str/includes? (:message result) "FAILED")
          "and A is told the comment is not there, rather than assuming it is"))))

(deftest a-refused-review-posts-nothing
  ;; No ledger row means no pass happened; a comment would claim otherwise.
  (let [[r g] (tmp-repo)
        posted (atom 0)
        d {:repo-root r :git-dir g :pr 371 :pass 1 :sha "headsha0123456"
           :branch "feat/x" :base-ref "main" :draft? false
           :prior-fingerprints []}]
    (#'trigger/review! d (assoc (review-opts :checkout :none)
                                :post-comment-fn (fn [& _] (swap! posted inc) "u")))
    (is (zero? @posted))))

(deftest a-refused-review-writes-no-findings-file
  ;; A stale file from an earlier pass must not be mistaken for this one's.
  (let [[r g] (tmp-repo)
        d {:repo-root r :git-dir g :pr 371 :pass 1 :sha "headsha0123456"
           :branch "feat/x" :base-ref "main" :draft? false
           :prior-fingerprints []}]
    (#'trigger/review! d (review-opts :checkout :none))
    (is (not (fs/exists? (trigger/findings-path g 371))))))

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
               {;; :reviewer-env-fn is not optional here. Without it `decide`
                ;; short-circuits to :silent whenever
                ;; PR_REVIEW_LOOP_REVIEWER is set in the AMBIENT
                ;; environment — which it is inside this loop's own
                ;; reviewer, and review_core.md permits that reviewer
                ;; to run the suite. It then gets a red suite it is
                ;; told not to attribute to the diff.
                :reviewer-env-fn (constantly nil)
                :log-fn (constantly log)
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

(deftest abandoned-per-pr-artifacts-are-swept-and-live-ones-are-not
  (testing "every retention rule here became per PR — the lock, the checkout,
            the diff, the ledger rows — and nothing bounds the number of PRs.
            `context/prune!` globbed `*.diff` before this branch and bounded the
            whole directory at 5; per PR it keeps 5 for each PR that ever ran,
            forever. Measured: 33 PRs in one clone, this PR's own diff 667 KB.
            The findings and stderr files are per PR and nothing deleted them at
            all.

            Age, not PR state: a running review's files are minutes old, so the
            sweep cannot reach one, and it needs no GitHub call to decide."
    (let [[_ g] (tmp-repo)
          ctx (context/context-dir g)
          old (- (System/currentTimeMillis) (* 20 24 60 60 1000))
          write! (fn [path stale?]
                   (fs/create-dirs (fs/parent path))
                   (spit path "x")
                   (when stale? (fs/set-last-modified-time path old))
                   path)
          dead-diff  (write! (str ctx "/370-abc.diff") true)
          dead-find  (write! (str g "/pr-review.370.findings.md") true)
          dead-err   (write! (str g "/pr-review.370.stderr") true)
          live-diff  (write! (str ctx "/402-def.diff") false)
          live-find  (write! (str g "/pr-review.402.findings.md") false)
          keep-other (write! (str g "/some-other-file") true)]
      (is (= 3 (#'trigger/sweep-abandoned-artifacts! g)))
      (is (not-any? fs/exists? [dead-diff dead-find dead-err]))
      (is (every? fs/exists? [live-diff live-find])
          "a PR still being reviewed must keep the files its prompt names")
      (is (fs/exists? keep-other)
          "and the sweep must only touch files this loop owns"))))

(deftest a-retry-names-the-session-that-pushed
  (testing "the retry path is the ONE that needs `:pushed-by`, and it was the
            one path that never set it. A reflog candidate already carries the
            session that pushed; a retry is deliberately delivered to whichever
            session is running, which is routinely not the one whose PR it is,
            so without this the wake says only 'a PR needs attention' to a
            session that has never heard of it.

            Real pushes.log, real lock, no stub between them: `attempts/pusher`
            had zero call sites and this is the caller it was written for."
    (let [[_ g] (tmp-repo)
          log (str (fs/path (fs/create-temp-dir {:prefix "pr-review-pusher"})
                            "pushes.log"))
          sha (apply str (repeat 40 "a"))
          three-days-ago (- (quot (System/currentTimeMillis) 1000) (* 3 86400))]
      (spit log (format "%d\t%s\tsess-pushed-it\trefs/heads/feat/x\t%s\n"
                        three-days-ago g sha))
      (lock/acquire! g {:pr 370 :sha sha :branch "feat/x"} {:pid 999999})
      (let [d (trigger/decide
               (input "git status")
               {:reviewer-env-fn (constantly nil)
                :log-fn (constantly log)
                :main-worktree-fn (fn [gd _] (str (fs/parent gd)))
                :open-pr-fn (fn [_ branch _]
                              (when (= "feat/x" branch) (a-pr 370 sha)))
                :now-fn (constantly (System/currentTimeMillis))})]
        (is (= :review (:action d)))
        (is (true? (:retry? d)))
        (is (= "sess-pushed-it" (:pushed-by d))
            "or the wake cannot say whose PR it is — on the only path where the
             woken session is not the pusher")))))

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
               {;; :reviewer-env-fn is not optional here. Without it `decide`
                ;; short-circuits to :silent whenever
                ;; PR_REVIEW_LOOP_REVIEWER is set in the AMBIENT
                ;; environment — which it is inside this loop's own
                ;; reviewer, and review_core.md permits that reviewer
                ;; to run the suite. It then gets a red suite it is
                ;; told not to attribute to the diff.
                :reviewer-env-fn (constantly nil)
                :log-fn (constantly log)
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
                 {;; :reviewer-env-fn is not optional here. Without it `decide`
                ;; short-circuits to :silent whenever
                ;; PR_REVIEW_LOOP_REVIEWER is set in the AMBIENT
                ;; environment — which it is inside this loop's own
                ;; reviewer, and review_core.md permits that reviewer
                ;; to run the suite. It then gets a red suite it is
                ;; told not to attribute to the diff.
                :reviewer-env-fn (constantly nil)
                :log-fn (constantly log)
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

(deftest the-automatic-path-carries-the-pr-url-and-the-pusher
  (testing "both were wired only in manual.clj, so on EVERY hook-triggered
            review the prompt's \"What the author says this change does\"
            section was silently absent (prompt/build gates on :pr-url) and the
            wake never named the pushing session. The tests that claimed to
            cover them passed the keys straight to prompt/build and
            findings-message, so neither could fail for the reason it claimed.

            :pushed-by matters more than it looks: abandoned-candidates is
            deliberately neither session- nor verb-scoped, so a wake routinely
            lands in a session that did not push"
    (let [d (trigger/decide
             (input)
             (opts :pushes {"/g" [(a-push "feat/x" "newsha" 999999)]}
                   :prs {"feat/x" (assoc (a-pr 370 "newsha")
                                         :url "https://github.com/o/r/pull/370")}
                   :session "sess-pusher"))]
      (is (= :review (:action d)))
      (is (= "https://github.com/o/r/pull/370" (:pr-url d))
          "or the reviewer is never told where to read the PR")
      (is (= "sess-pusher" (:pushed-by d))
          "or a wake cannot say whose push it was"))))

(deftest a-capped-decision-carries-them-too
  ;; :cap-reached wakes the author as well; it must not lose the metadata.
  (let [[_ g] (tmp-repo)]
    (doseq [n (range 1 (inc ledger/max-passes))]
      (ledger/append-pass! g (row 370 (str "sha" n) n)))
    (let [d (trigger/decide
             (input)
             (opts :pushes {g [(a-push "feat/x" "shaN" 999999)]}
                   :prs {"feat/x" (assoc (a-pr 370 "shaN")
                                         :url "https://github.com/o/r/pull/370")}
                   :session "sess-pusher"))]
      (is (= :cap-reached (:action d)))
      (is (= "https://github.com/o/r/pull/370" (:pr-url d)))
      (is (= "sess-pusher" (:pushed-by d))))))

(deftest run-review-passes-the-real-pr-to-the-checkout
  (testing "the wiring, not the function. `run-review!` destructured
            {:keys [git-dir sha]} and then passed a bare `pr`, which is not
            bound there and resolved to clojure.core/pr — the path became
            `pr-review-sci.impl.io$pr@78cc4ec-<sha>`, identical for every PR.
            Two PRs at one sha shared a directory exactly as before, and the
            name no longer matched `review-dir-re`, so `prune-stale!` could
            never reclaim a real checkout either.

            The suite stayed green because the stub discarded the argument and
            checkout_test called `add!` with literal PR numbers — the property
            was certified on a path production never takes."
    (let [[r g] (tmp-repo)
          d {:repo-root r :git-dir g :pr 401 :pass 1 :sha "headsha"
             :branch "feat/x" :base-ref "main" :draft? false
             :prior-fingerprints []}]
      (reset! last-checkout-pr ::never-called)
      (#'trigger/review! d (review-opts))
      (is (= 401 @last-checkout-pr)
          "the checkout must be told which PR it is for"))))

