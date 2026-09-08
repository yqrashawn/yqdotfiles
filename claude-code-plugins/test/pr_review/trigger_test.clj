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

(defn- opts
  "Wire every collaborator to a stub so `decide` is exercised in isolation.

   :branch is read via `contains?`, not `(or branch \"feat/x\")`: the latter
   cannot distinguish an explicit `:branch nil` (simulating detached HEAD)
   from the key being omitted (the normal-branch default), so a caller
   passing `:branch nil` would silently get \"feat/x\" back instead of nil."
  [repo git-dir & {:keys [pr sha] :as kvs}]
  {:repo-root-fn (constantly repo)
   :git-dir-fn   (constantly git-dir)
   :branch-fn    (constantly (if (contains? kvs :branch) (:branch kvs) "feat/x"))
   :head-sha-fn  (constantly (or sha "headsha"))
   :open-pr-fn   (constantly pr)})

(defn- a-pr [n & {:keys [draft?]}]
  {:number n :isDraft (boolean draft?) :baseRefName "main"})

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
  "opts for `review!`: a stubbed diff and a stubbed reviewer, so nothing
   shells out to a real `claude -p`."
  [& {:keys [out exit err pid spawn-fn]}]
  (cond-> {:merge-base-fn (constantly "basesha")
           :diff-fn (constantly "diff --git a/a b/a\n")
           :pid (or pid 4242)
           :spawn-fn (or spawn-fn
                         (fn [_ _ _] {:exit (or exit 0)
                                      :out (or out (clean-reply))
                                      :err (or err "")}))}
    true identity))

;; ----------------------------------------------------------------- decide

(deftest no-repo-is-silent
  (let [d (trigger/decide {:cwd "/tmp"}
                          (assoc (opts nil nil) :repo-root-fn (constantly nil)))]
    (is (= :silent (:action d)))
    (is (str/includes? (:reason d) "not a git repo"))))

(deftest detached-head-is-silent
  (let [[r g] (tmp-repo)
        d (trigger/decide {:cwd r} (opts r g :branch nil))]
    (is (= :silent (:action d)))
    (is (str/includes? (:reason d) "detached HEAD")
        "must name detached HEAD specifically: this fixture's :open-pr-fn is
         already nil, so deleting the detached-HEAD branch of `decide` would
         still fall through to :silent via the no-open-PR branch, and this
         test would not catch it without a :reason assertion")))

(deftest no-open-pr-is-silent
  (let [[r g] (tmp-repo)
        d (trigger/decide {:cwd r} (opts r g :pr nil))]
    (is (= :silent (:action d)))
    (is (str/includes? (:reason d) "no open PR")
        "a push to a branch with no PR is the human's normal workflow, not an error")))

(deftest open-pr-yields-a-review-with-pass-one
  (let [[r g] (tmp-repo)
        d (trigger/decide {:cwd r} (opts r g :pr (a-pr 370)))]
    (is (= :review (:action d)))
    (is (= 370 (:pr d)))
    (is (= 1 (:pass d)))
    (is (= "headsha" (:sha d)))
    (is (= "main" (:base-ref d)))
    (is (false? (:draft? d)))
    (is (= g (:git-dir d))
        "the decision carries the resolved git dir; every state module reads
         it instead of assuming <repo-root>/.git")))

(deftest drafts-are-reviewed
  (let [[r g] (tmp-repo)
        d (trigger/decide {:cwd r} (opts r g :pr (a-pr 9 :draft? true)))]
    (is (= :review (:action d)))
    (is (true? (:draft? d)))))

(deftest pass-number-comes-from-the-ledger
  (let [[r g] (tmp-repo)]
    (doseq [n [1 2]] (ledger/append-pass! g (row 370 (str n) n)))
    (is (= 3 (:pass (trigger/decide {:cwd r} (opts r g :pr (a-pr 370))))))))

(deftest cap-stops-the-loop
  (let [[r g] (tmp-repo)]
    (doseq [n (range 1 (inc ledger/max-passes))]
      (ledger/append-pass! g (row 370 (str n) n)))
    (let [d (trigger/decide {:cwd r} (opts r g :pr (a-pr 370)))]
      (is (= :cap-reached (:action d)))
      (is (str/includes? (:reason d) "10")))))

(deftest an-already-reviewed-sha-is-silent
  (testing "repeat pushes of one commit are ordinary — `git push` twice,
            `--tags`, `--dry-run` and `--delete` all match
            `Bash(git push:*)`. Counting rows and never consulting the :sha
            the ledger faithfully records re-reviewed the same commit on every
            one of them, one cap slot each"
    (let [[r g] (tmp-repo)]
      (ledger/append-pass! g (row 370 "headsha" 1))
      (let [d (trigger/decide {:cwd r} (opts r g :pr (a-pr 370)))]
        (is (= :silent (:action d)))
        (is (str/includes? (:reason d) "headsha")
            "the reason must name the SHA that was already reviewed"))
      (testing "a new commit on the same PR still gets reviewed"
        (is (= :review (:action (trigger/decide
                                 {:cwd r} (opts r g :pr (a-pr 370) :sha "newsha")))))))))

(deftest twice-raised-followup-fingerprints-are-carried-into-the-decision
  (let [[r g] (tmp-repo)
        fp "src/a.clj:1:correctness/followup"]
    (doseq [n [1 2]]
      (ledger/append-pass! g (row 370 (str n) n :fingerprints [fp] :verdict "MERGEABLE")))
    (is (= [fp] (:prior-fingerprints
                 (trigger/decide {:cwd r} (opts r g :pr (a-pr 370) :sha "s3")))))))

(deftest a-blocking-finding-is-never-put-on-the-do-not-re-raise-list
  (testing "the filter used to key on the raise count alone, and the prompt
            then told the reviewer not to report those again. Two pushes that
            do not close a blocking defect, line number unchanged: pass 3
            reports MERGEABLE and the skill merges broken code"
    (let [[r g] (tmp-repo)
          blocking "src/a.clj:1:correctness/blocking"
          coverage "src/a.clj:2:coverage"
          followup "src/a.clj:3:correctness/followup"]
      (doseq [n [1 2]]
        (ledger/append-pass!
         g (row 370 (str n) n :fingerprints [blocking coverage followup])))
      (let [prior (:prior-fingerprints
                   (trigger/decide {:cwd r} (opts r g :pr (a-pr 370) :sha "s3")))]
        (is (= [followup] prior))
        (is (not-any? #{blocking} prior)
            "a blocking finding present after two passes has not been fixed")
        (is (not-any? #{coverage} prior))))))

(deftest decide-reads-the-ledger-once
  (testing "the one-re-raise filter used to call a per-fingerprint helper
            that re-slurped the whole ledger, on top of separate reads for
            the cap and the pass number: 138 full file reads for one decision
            at nine passes and fifteen findings"
    (let [[r g] (tmp-repo)
          fps (mapv #(str "src/f" % ".clj:1:style") (range 15))]
      (doseq [n (range 1 10)]
        (ledger/append-pass! g (row 370 (str n) n :fingerprints fps)))
      (let [reads (atom 0)
            orig  slurp]
        (with-redefs [slurp (fn [& args] (swap! reads inc) (apply orig args))]
          (is (= :review (:action (trigger/decide
                                   {:cwd r} (opts r g :pr (a-pr 370) :sha "fresh"))))))
        (is (= 1 @reads)
            (str "one decision must cost exactly one read of the ledger; got "
                 @reads))))))

;; ---------------------------------------------------------------- messages

(deftest findings-message-is-self-describing
  (testing "the harness wrapper text is fixed and useless, so the first line
            must identify repo, PR and pass on its own"
    (let [msg (trigger/findings-message
               {:repo-root "/r" :pr 370 :pass 2}
               {:verdict "NOT MERGEABLE" :body "BODY"
                :counts {"correctness/blocking" 1}})]
      (is (str/starts-with? msg "pr-review-loop"))
      (is (str/includes? msg "PR #370"))
      (is (str/includes? msg "pass 2"))
      (is (str/includes? msg "BODY"))
      (is (str/includes? msg (str (fs/file-name "/r") " PR #370"))
          "the repo identifier must appear right before the PR number — a
           regression that dropped repo-root from the message would still
           satisfy every assertion above it"))))

(deftest findings-message-carries-parse-warnings
  (let [msg (trigger/findings-message
             {:repo-root "/r" :pr 1 :pass 1}
             {:verdict "NOT MERGEABLE" :body "BODY" :counts {}}
             ["PARSE WARNING TEXT"])]
    (is (str/includes? msg "PARSE WARNING TEXT")
        "a review whose findings carry no fingerprints cannot converge; agent
         A has to be told")))

;; --------------------------------------------------------------- review!

(deftest a-completed-review-records-one-pass-and-wakes-the-session
  (let [[r g] (tmp-repo)
        d {:repo-root r :git-dir g :pr 370 :pass 1 :sha "headsha"
           :base-ref "main" :draft? false :prior-fingerprints []}
        result (#'trigger/review! d (review-opts))]
    (is (= 2 (:exit result)))
    (is (= ["headsha"] (mapv :sha (ledger/read-passes g 370))))
    (is (= "MERGEABLE" (:verdict (first (ledger/read-passes g 370)))))
    (is (nil? (lock/read-lock g)) "the lock is released on the success path")))

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
             :base-ref "main" :draft? false :prior-fingerprints []}
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
           :base-ref "main" :draft? false :prior-fingerprints []}
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
          steal (fn [_ _ _]
                  (lock/acquire! g {:pr 370 :sha "new"} {:pid 9999})
                  {:exit 143 :out "" :err "terminated"})
          result (#'trigger/review! d (review-opts :spawn-fn steal))]
      (is (= 0 (:exit result)) "silence, not a wake")
      (is (nil? (:message result)))
      (is (empty? (ledger/read-passes g 370)))
      (is (= 9999 (:pid (lock/read-lock g)))
          "and the loser must not have deleted the winner's lock record"))))

(deftest a-duplicate-push-is-silent
  (let [[r g] (tmp-repo)
        self (.pid (java.lang.ProcessHandle/current))]
    (spit (lock/lock-path g)
          (str "{\"pid\":" self ",\"pr\":370,\"sha\":\"headsha\",\"started\":1}"))
    (let [d {:repo-root r :git-dir g :pr 370 :pass 1 :sha "headsha"
             :base-ref "main" :draft? false :prior-fingerprints []}
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
             :base-ref "main" :draft? false :prior-fingerprints []}
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
           :base-ref "main" :draft? false :prior-fingerprints []}
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
           :base-ref "main" :draft? false :prior-fingerprints []}
        result (with-redefs [lock/release! (fn [& _] (throw (ex-info "release exploded" {})))]
                 (#'trigger/review! d (review-opts)))]
    (is (= 2 (:exit result)))
    (is (str/includes? (:message result) "MERGEABLE")
        "a throw out of a `finally` replaces the value the body already
         computed, so an unwrapped release! discards a completed review's
         findings and reports a crash instead")
    (is (= 1 (count (ledger/read-passes g 370))))))

;; ------------------------------------------------------ worktree end to end

(deftest a-worktree-whose-dot-git-is-a-file-still-reviews
  (testing "the fixture no test had. <repo-root>/.git is a FILE in a linked
            worktree, so fs/create-dirs on it throws
            FileAlreadyExistsException — and lock/acquire! used to be
            evaluated outside review!'s try, so that throw escaped -main and
            babashka exited 1 on every push from a worktree. Before that,
            read-passes found no ledger there at all, so neither the cap nor
            the re-raise rule ever engaged"
    (let [tmp  (str (fs/create-temp-dir {:prefix "pr-review-wt"}))
          main (str tmp "/main")
          wt   (str tmp "/wt")
          git! (fn [dir & args]
                 (let [{:keys [exit err]} (p/sh (into ["git"] args) {:dir dir})]
                   (when-not (zero? exit)
                     (throw (ex-info (str "fixture git failed: " args " " err) {})))))]
      (fs/create-dirs main)
      (git! main "init" "-q")
      (git! main "config" "user.email" "t@t.t")
      (git! main "config" "user.name" "t")
      (spit (str main "/f") "hi")
      (git! main "add" "f")
      (git! main "commit" "-qm" "init")
      (git! main "worktree" "add" "-q" wt "-b" "feat")
      (is (fs/regular-file? (str wt "/.git"))
          "fixture precondition: a linked worktree's .git is a file")

      (let [d (trigger/decide
               {:cwd wt}
               {:repo-root-fn (constantly wt)
                :branch-fn (constantly "feat")
                :head-sha-fn (constantly "wtsha")
                :open-pr-fn (constantly (a-pr 42))})]
        (is (= :review (:action d)))
        (is (fs/directory? (:git-dir d))
            "the resolved git dir must be a real directory")
        (is (= (str (fs/real-path (str main "/.git")))
               (str (fs/real-path (:git-dir d))))
            "one ledger, one lock and one context dir per repository — a
             worktree-local pair would make the cap and the lock meaningless
             across worktrees")

        (let [result (#'trigger/review! d (review-opts))]
          (is (= 2 (:exit result))
              "and the whole pass must complete without a throw escaping to a
               non-2 exit")
          (is (= ["wtsha"] (mapv :sha (ledger/read-passes (:git-dir d) 42)))
              "the pass lands in the shared ledger, visible to every worktree")
          (is (fs/exists? (str (:git-dir d) "/pr-review-context/wtsha.diff"))))))))

;; ------------------------------------- the directory the command ran in

(deftest a-push-from-a-worktree-reviews-that-worktrees-pr-not-the-sessions
  (testing "the defect the first live push in a real repo found. The payload
            `cwd` is the SESSION's directory; the push ran in a worktree the
            command `cd`ed into. Measured on both sides: session cwd on
            branch docs/mydeck-design with open-pr nil, the worktree on
            fix/llm-logs-sse-deadline-and-abandoned-tabs with open PR #391.
            `decide` resolved the repository from `cwd`, correctly-by-its-own-
            logic found nothing, and exited 0 — so the loop was silently
            inert for the workflow agents actually use, and a real PR went
            unreviewed with nothing said about it"
    (let [tmp  (str (fs/create-temp-dir {:prefix "pr-review-cd"}))
          main (str tmp "/main")
          wt   (str tmp "/wt-sse-deadline-abandon")
          feat "fix/llm-logs-sse-deadline-and-abandoned-tabs"
          git! (fn [dir & args]
                 (let [{:keys [exit err]} (p/sh (into ["git"] args) {:dir dir})]
                   (when-not (zero? exit)
                     (throw (ex-info (str "fixture git failed: " args " " err) {})))))]
      (fs/create-dirs main)
      (git! main "init" "-q")
      (git! main "config" "user.email" "t@t.t")
      (git! main "config" "user.name" "t")
      (spit (str main "/f") "hi")
      (git! main "add" "f")
      (git! main "commit" "-qm" "init")
      (git! main "checkout" "-q" "-b" "docs/mydeck-design")
      (git! main "worktree" "add" "-q" wt "-b" feat)

      ;; Only the network call is stubbed. repo-root, branch, git-dir and
      ;; head-sha all run real git against whichever directory `decide`
      ;; resolved, which is the whole point: a stub that ignored the
      ;; directory could not tell the two checkouts apart.
      (let [pr-for   (fn [_root branch _opts] (when (= feat branch) (a-pr 391)))
            command  (str "SP=" tmp "; cd \"$SP/wt-sse-deadline-abandon\""
                          " && git push -u origin " feat " 2>&1 | tail -2"
                          "; grep -aE 'a;b' /dev/null")
            d        (trigger/decide {:cwd main :tool_input {:command command}}
                                     {:open-pr-fn pr-for})]
        (is (= :review (:action d)))
        (is (= 391 (:pr d)) "the worktree's PR, which is the one that was pushed")
        (is (= (str (fs/real-path wt)) (str (fs/real-path (:repo-root d))))
            "and the review runs against the worktree, not the session cwd")
        (testing "the same payload with a command that never leaves the
                  session directory is the measured control: no PR there"
          (let [c (trigger/decide {:cwd main :tool_input {:command "git push"}}
                                  {:open-pr-fn pr-for})]
            (is (not= :review (:action c))
                "if this were :review the fixture would be proving nothing")))))))

(deftest a-push-that-finds-no-open-pr-stays-silent
  (testing "fix wave 2 turned this into an exit-2 diagnostic naming the
            directory and branch checked, reasoning that silence and a
            broken plugin are indistinguishable. Deliberately reverted: `if`
            is documented best-effort and fails open (C7), so a single
            undeterminable command fired all four `hooks.json` entries and
            each one produced the diagnostic — four wakes for one push. The
            known cost is back: a push that reviews nothing is once again
            indistinguishable from a broken plugin"
    (let [[r g] (tmp-repo)
          d (trigger/decide {:cwd r :tool_input {:command "git push -u origin feat/x"}}
                            (opts r g :pr nil))]
      (is (= :silent (:action d)))
      (let [res (#'trigger/respond d {})]
        (is (= 0 (:exit res)))
        (is (nil? (:message res))
            "no message either — printing one here recreates the four-wakes
             defect this reversion exists to remove")))))

(deftest a-command-with-no-trigger-verb-is-still-silent
  (testing "the `if` rules are best-effort — C7 runs the hook anyway when it
            cannot determine the command — so a command that never pushed
            does reach here too, and must be exactly as silent as the
            trigger-verb case above: there is no diagnostic left to gate on
            the verb"
    (let [[r g] (tmp-repo)
          d (trigger/decide {:cwd r :tool_input {:command "git status --short"}}
                            (opts r g :pr nil))]
      (is (= :silent (:action d)))
      (is (= 0 (:exit (#'trigger/respond d {})))))))
