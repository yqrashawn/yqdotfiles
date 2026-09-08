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
