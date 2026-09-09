(ns pr-review.lock-test
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
            [pr-review.flock :as flock]
            [pr-review.lock :as lock]))

(defn- tmp-git-dir
  "A stand-in for the clone's shared git directory — what
   pr-review.gh/git-common-dir resolves. Never a repo root: `<root>/.git` is a
   file in a linked worktree, so a repo-root-relative lock is unwritable there
   and invisible to the clone's other worktrees."
  []
  (let [d (str (fs/create-temp-dir {:prefix "pr-review-lock"}) "/.git")]
    (fs/create-dirs d)
    d))

(defn- write-lock!
  ([git-dir m] (write-lock! git-dir 370 m))
  ([git-dir pr m]
   (spit (lock/lock-path git-dir pr) (json/generate-string m))))

(deftest lock-path-is-under-the-git-dir
  (is (= "/r/.git/pr-review.370.lock" (lock/lock-path "/r/.git" 370))))

(deftest alive-tracks-real-processes
  (is (true? (lock/alive? (.pid (java.lang.ProcessHandle/current)))))
  (is (false? (lock/alive? 999999)) "an absent PID must read as dead, not as held"))

(deftest acquire-on-free-repo-succeeds-and-records-identity
  (let [r (tmp-git-dir)
        res (lock/acquire! r {:pr 370 :sha "abc"} {:pid 4242})]
    (is (= :acquired (:status res)))
    (is (= {:pid 4242 :pr 370 :sha "abc"}
           (select-keys (lock/read-lock r 370) [:pid :pr :sha])))
    (is (pos? (:started (lock/read-lock r 370))))))

(deftest same-sha-in-flight-is-a-duplicate
  (let [r (tmp-git-dir)
        self (.pid (java.lang.ProcessHandle/current))]
    (write-lock! r {:pid self :pr 370 :sha "abc" :started 1})
    (is (= :duplicate (:status (lock/acquire! r {:pr 370 :sha "abc"} {:pid 1})))
        "two hooks for one push must not run two reviewers")
    (is (= self (:pid (lock/read-lock r 370))) "the incumbent keeps the lock")))

(deftest newer-sha-supersedes-and-kills-the-incumbent
  (let [r (tmp-git-dir)
        self (.pid (java.lang.ProcessHandle/current))
        killed (atom nil)]
    (write-lock! r {:pid self :pr 370 :sha "old" :started 1})
    (let [res (lock/acquire! r {:pr 370 :sha "new"}
                             {:pid 777 :kill-fn #(reset! killed %)})]
      (is (= :superseded (:status res)))
      (is (= self (:killed-pid res)))
      (is (= self @killed) "reviewing a stale SHA is waste; kill it")
      (is (= {:pid 777 :sha "new"} (select-keys (lock/read-lock r 370) [:pid :sha]))))))

(deftest dead-holder-lock-is-taken-without-killing
  (let [r (tmp-git-dir)
        killed (atom nil)]
    (write-lock! r {:pid 999999 :pr 370 :sha "old" :started 1})
    (let [res (lock/acquire! r {:pr 370 :sha "new"}
                             {:pid 5 :kill-fn #(reset! killed %)})]
      (is (= :acquired (:status res))
          "a crashed reviewer must not block every future review")
      (is (nil? @killed) "nothing to kill when the holder is already gone"))))

(deftest corrupt-lock-file-is-treated-as-free
  (let [r (tmp-git-dir)]
    (spit (lock/lock-path r 370) "{not json")
    (is (= :acquired (:status (lock/acquire! r {:pr 1 :sha "s"} {:pid 6}))))))

(deftest release-removes-the-lock
  (let [r (tmp-git-dir)]
    (lock/acquire! r {:pr 1 :sha "s"} {:pid 7})
    (lock/release! r 370 {:pid 7})
    (is (nil? (lock/read-lock r 370)))))

(deftest acquire-runs-under-the-shared-flock-on-the-guard-path
  (let [r (tmp-git-dir)
        seen (atom nil)]
    (with-redefs [flock/with-file-lock (fn [path f] (reset! seen path) (f))]
      (is (= :acquired (:status (lock/acquire! r {:pr 1 :sha "s"} {:pid 9})))))
    (is (some? @seen)
        "acquire! must run its read-check-write through pr-review.flock/with-file-lock")
    (is (= (flock/guard-path (lock/lock-path r 1)) @seen)
        "acquire! must flock the sibling guard path")
    (is (not= (lock/lock-path r 1) @seen)
        "acquire! must never flock the lock record path itself: acquire! rewrites
         that path, so a lock held on it would stop protecting anything the moment
         it's rewritten")))

(deftest acquire-on-lock-missing-pid-is-acquired-not-an-npe
  (let [r (tmp-git-dir)]
    (spit (lock/lock-path r 370) "{}")
    (is (= :acquired (:status (lock/acquire! r {:pr 1 :sha "s"} {:pid 10})))
        "a lock record that is valid JSON but missing :pid must read as free, not
         throw: read-lock returning {} truthy would send (alive? nil) into
         (long nil), an NPE")))

(deftest release-does-not-delete-a-record-owned-by-a-different-pid
  (let [r (tmp-git-dir)]
    (write-lock! r {:pid 4242 :pr 370 :sha "new" :started 1})
    (lock/release! r 370 {:pid 999})
    (is (= {:pid 4242 :pr 370 :sha "new"}
           (select-keys (lock/read-lock r 370) [:pid :pr :sha]))
        "release! must never delete a record that belongs to a different holder:
         a reviewer finishing normally must not be able to delete the record a
         concurrent acquire! just wrote for the process that superseded it")))

(deftest release-deletes-its-own-record
  (let [r (tmp-git-dir)]
    (write-lock! r {:pid 4242 :pr 370 :sha "new" :started 1})
    (lock/release! r 370 {:pid 4242})
    (is (nil? (lock/read-lock r 370))
        "the ownership check must not be so strict it turns release! into a
         no-op for its own record")))

(deftest release-runs-under-the-shared-flock-on-the-guard-path
  (let [r (tmp-git-dir)
        seen (atom nil)]
    (write-lock! r {:pid 4242 :pr 370 :sha "new" :started 1})
    (with-redefs [flock/with-file-lock (fn [path f] (reset! seen path) (f))]
      (lock/release! r 370 {:pid 4242}))
    (is (some? @seen)
        "release! must run under pr-review.flock/with-file-lock so it cannot
         interleave with an in-flight acquire!")
    (is (= (flock/guard-path (lock/lock-path r 370)) @seen)
        "release! must flock the same sibling guard path acquire! uses")
    (is (not= (lock/lock-path r 370) @seen)
        "release! must never flock the lock record path itself")))

(deftest kill-targets-the-reviewer-subtree-not-the-recorded-process
  (testing "the recorded pid is the bb trigger; the reviewer is its child.
            SIGTERMing the trigger — what this used to do — left the `claude
            -p` child running to completion, so a supersede gave two
            concurrent reviewers for the 2-6 minutes a review takes, and made
            the losing trigger exit 143"
    ;; A babashka parent that spawns a child and then sleeps in-process, not
    ;; a shell: the same shape as the real trigger, which spawns `claude -p`
    ;; and blocks on it. A `sh -c` parent would exit the moment its
    ;; foreground child died and prove nothing about who was killed.
    (let [proc (p/process ["bb" "-e" "(require '[babashka.process :as p]) (p/process [\"sleep\" \"30\"]) (Thread/sleep 20000)"]
                          {:out :string :err :string})
          pid  (.pid (:proc proc))]
      (try
        (Thread/sleep 2000)
        (let [h    (.get (java.lang.ProcessHandle/of (long pid)))
              kids (vec (iterator-seq (.iterator (.descendants h))))]
          (is (seq kids)
              "fixture precondition: the recorded process must actually have a
               child to stand in for the reviewer")
          (lock/kill-reviewers! pid)
          (Thread/sleep 800)
          (is (every? #(not (.isAlive %)) kids)
              "the reviewer child must be dead")
          (is (true? (lock/alive? pid))
              "the recorded trigger must survive, so it can reach its own
               System/exit 0 instead of dying with 143"))
        (finally (p/destroy-tree proc))))))

(deftest two-prs-in-one-clone-do-not-evict-each-other
  (testing "N1. The lock used to be one file per CLONE, and acquire!
            supersedes on any sha it does not recognise — so pushing PR #397
            killed PR #395's running reviewer and dropped that review with no
            ledger row and no message. A repository with five open PRs does
            that constantly"
    (let [g (tmp-git-dir)
          killed (atom [])
          kill (fn [pid] (swap! killed conj pid))]
      (is (= :acquired (:status (lock/acquire! g {:pr 395 :sha "sha-395"}
                                               {:pid 4242 :kill-fn kill}))))
      (is (= :acquired (:status (lock/acquire! g {:pr 397 :sha "sha-397"}
                                               {:pid 4343 :kill-fn kill})))
          "a second PR must find its own lock free, not take the first's")
      (is (empty? @killed) "no reviewer may be killed for another PR's push")
      (is (= 4242 (:pid (lock/read-lock g 395))))
      (is (= 4343 (:pid (lock/read-lock g 397))))
      (is (false? (lock/superseded? g 395 {:pid 4242}))
          "PR #395's reviewer must not learn it lost a race it was not in")
      (is (false? (lock/superseded? g 397 {:pid 4343}))))))

(deftest a-second-push-to-the-SAME-pr-still-supersedes
  (testing "R14 is unchanged and still scoped where it belongs: two rapid
            pushes to one PR must not run two reviewers on stale shas"
    (let [g (tmp-git-dir)
          killed (atom [])
          kill (fn [pid] (swap! killed conj pid))
          self (.pid (java.lang.ProcessHandle/current))]
      (is (= :acquired (:status (lock/acquire! g {:pr 395 :sha "old"}
                                               {:pid self :kill-fn kill}))))
      (let [r (lock/acquire! g {:pr 395 :sha "new"} {:pid 4343 :kill-fn kill})]
        (is (= :superseded (:status r)))
        (is (= [self] @killed) "the older reviewer for that PR is killed"))
      (is (= "new" (:sha (lock/read-lock g 395)))))))

(deftest releasing-one-pr-leaves-the-others-held
  (let [g (tmp-git-dir)]
    (lock/acquire! g {:pr 395 :sha "a"} {:pid 4242})
    (lock/acquire! g {:pr 397 :sha "b"} {:pid 4242})
    (lock/release! g 395 {:pid 4242})
    (is (nil? (lock/read-lock g 395)))
    (is (= 4242 (:pid (lock/read-lock g 397)))
        "release! must not reach another PR's record")))

(deftest abandoned-lists-dead-holders-and-never-live-ones
  (testing "the whole retry path keys off pid liveness: a record naming a LIVE
            pid is a review still running, and retrying it would run two
            reviewers on one sha and spend two ledger slots"
    (let [g (tmp-git-dir)
          self (.pid (java.lang.ProcessHandle/current))]
      ;; Written directly, with distinct :started values: three acquires land
      ;; in the same millisecond and the ordering assertion would be testing a
      ;; tie-break that is not part of the contract.
      (write-lock! g 395 {:pid 999999 :pr 395 :sha "s395" :branch "feat/a" :started 100})
      (write-lock! g 397 {:pid self    :pr 397 :sha "s397" :branch "feat/b" :started 200})
      (write-lock! g 400 {:pid 999998  :pr 400 :sha "s400" :branch "feat/c" :started 300})
      (let [ab (lock/abandoned g)]
        (is (= [400 395] (map :pr ab)) "newest-started first, live one absent")
        (is (not (some #(= 397 (:pr %)) ab)))
        (is (= ["feat/c" "feat/a"] (map :branch ab))
            "the record has to carry the branch, or a retry cannot name the PR")
        (is (= ["s400" "s395"] (map :sha ab))))
      (testing "and a live holder really was written, so the exclusion above
                is the liveness check and not a missing file"
        (is (= self (:pid (lock/read-lock g 397))))))))

(deftest abandoned-on-a-clone-with-no-locks-is-empty
  (is (empty? (lock/abandoned (tmp-git-dir))))
  (is (empty? (lock/abandoned "/nonexistent/path/.git"))
      "a clone that has since been deleted must not throw"))

(deftest kill-on-a-dead-pid-is-not-an-error
  (is (nil? (lock/kill-reviewers! 999999))
      "a supersede must never throw out of acquire!: the trigger's only legal
       exit codes are 0 and 2"))

(deftest superseded-is-false-while-this-process-still-holds-the-lock
  (let [g (tmp-git-dir)]
    (lock/acquire! g {:pr 1 :sha "s"} {:pid 4242})
    (is (false? (lock/superseded? g 1 {:pid 4242})))))

(deftest superseded-is-true-once-another-trigger-takes-the-slot
  (let [g (tmp-git-dir)]
    (lock/acquire! g {:pr 1 :sha "old"} {:pid 4242})
    (write-lock! g 1 {:pid 777 :pr 1 :sha "new" :started 1})
    (is (true? (lock/superseded? g 1 {:pid 4242}))
        "this is how a loser learns it lost: its reviewer was killed
         mid-answer, and recording that truncated output would spend a cap
         slot and wake agent A with findings for a stale SHA")))

(deftest a-missing-lock-record-reads-as-superseded
  (let [g (tmp-git-dir)]
    (is (true? (lock/superseded? g 370 {:pid 4242}))
        "no record can only mean this trigger was superseded and the winner
         has since released, or that something outside the loop deleted it;
         publishing a pass whose lock is gone is the riskier of the two")))
