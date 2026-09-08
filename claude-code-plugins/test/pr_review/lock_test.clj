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

(defn- write-lock! [git-dir m]
  (spit (lock/lock-path git-dir) (json/generate-string m)))

(deftest lock-path-is-under-the-git-dir
  (is (= "/r/.git/pr-review.lock" (lock/lock-path "/r/.git"))))

(deftest alive-tracks-real-processes
  (is (true? (lock/alive? (.pid (java.lang.ProcessHandle/current)))))
  (is (false? (lock/alive? 999999)) "an absent PID must read as dead, not as held"))

(deftest acquire-on-free-repo-succeeds-and-records-identity
  (let [r (tmp-git-dir)
        res (lock/acquire! r {:pr 370 :sha "abc"} {:pid 4242})]
    (is (= :acquired (:status res)))
    (is (= {:pid 4242 :pr 370 :sha "abc"}
           (select-keys (lock/read-lock r) [:pid :pr :sha])))
    (is (pos? (:started (lock/read-lock r))))))

(deftest same-sha-in-flight-is-a-duplicate
  (let [r (tmp-git-dir)
        self (.pid (java.lang.ProcessHandle/current))]
    (write-lock! r {:pid self :pr 370 :sha "abc" :started 1})
    (is (= :duplicate (:status (lock/acquire! r {:pr 370 :sha "abc"} {:pid 1})))
        "two hooks for one push must not run two reviewers")
    (is (= self (:pid (lock/read-lock r))) "the incumbent keeps the lock")))

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
      (is (= {:pid 777 :sha "new"} (select-keys (lock/read-lock r) [:pid :sha]))))))

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
    (spit (lock/lock-path r) "{not json")
    (is (= :acquired (:status (lock/acquire! r {:pr 1 :sha "s"} {:pid 6}))))))

(deftest release-removes-the-lock
  (let [r (tmp-git-dir)]
    (lock/acquire! r {:pr 1 :sha "s"} {:pid 7})
    (lock/release! r {:pid 7})
    (is (nil? (lock/read-lock r)))))

(deftest acquire-runs-under-the-shared-flock-on-the-guard-path
  (let [r (tmp-git-dir)
        seen (atom nil)]
    (with-redefs [flock/with-file-lock (fn [path f] (reset! seen path) (f))]
      (is (= :acquired (:status (lock/acquire! r {:pr 1 :sha "s"} {:pid 9})))))
    (is (some? @seen)
        "acquire! must run its read-check-write through pr-review.flock/with-file-lock")
    (is (= (flock/guard-path (lock/lock-path r)) @seen)
        "acquire! must flock the sibling guard path")
    (is (not= (lock/lock-path r) @seen)
        "acquire! must never flock the lock record path itself: acquire! rewrites
         that path, so a lock held on it would stop protecting anything the moment
         it's rewritten")))

(deftest acquire-on-lock-missing-pid-is-acquired-not-an-npe
  (let [r (tmp-git-dir)]
    (spit (lock/lock-path r) "{}")
    (is (= :acquired (:status (lock/acquire! r {:pr 1 :sha "s"} {:pid 10})))
        "a lock record that is valid JSON but missing :pid must read as free, not
         throw: read-lock returning {} truthy would send (alive? nil) into
         (long nil), an NPE")))

(deftest release-does-not-delete-a-record-owned-by-a-different-pid
  (let [r (tmp-git-dir)]
    (write-lock! r {:pid 4242 :pr 370 :sha "new" :started 1})
    (lock/release! r {:pid 999})
    (is (= {:pid 4242 :pr 370 :sha "new"}
           (select-keys (lock/read-lock r) [:pid :pr :sha]))
        "release! must never delete a record that belongs to a different holder:
         a reviewer finishing normally must not be able to delete the record a
         concurrent acquire! just wrote for the process that superseded it")))

(deftest release-deletes-its-own-record
  (let [r (tmp-git-dir)]
    (write-lock! r {:pid 4242 :pr 370 :sha "new" :started 1})
    (lock/release! r {:pid 4242})
    (is (nil? (lock/read-lock r))
        "the ownership check must not be so strict it turns release! into a
         no-op for its own record")))

(deftest release-runs-under-the-shared-flock-on-the-guard-path
  (let [r (tmp-git-dir)
        seen (atom nil)]
    (write-lock! r {:pid 4242 :pr 370 :sha "new" :started 1})
    (with-redefs [flock/with-file-lock (fn [path f] (reset! seen path) (f))]
      (lock/release! r {:pid 4242}))
    (is (some? @seen)
        "release! must run under pr-review.flock/with-file-lock so it cannot
         interleave with an in-flight acquire!")
    (is (= (flock/guard-path (lock/lock-path r)) @seen)
        "release! must flock the same sibling guard path acquire! uses")
    (is (not= (lock/lock-path r) @seen)
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

(deftest kill-on-a-dead-pid-is-not-an-error
  (is (nil? (lock/kill-reviewers! 999999))
      "a supersede must never throw out of acquire!: the trigger's only legal
       exit codes are 0 and 2"))

(deftest superseded-is-false-while-this-process-still-holds-the-lock
  (let [g (tmp-git-dir)]
    (lock/acquire! g {:pr 1 :sha "s"} {:pid 4242})
    (is (false? (lock/superseded? g {:pid 4242})))))

(deftest superseded-is-true-once-another-trigger-takes-the-slot
  (let [g (tmp-git-dir)]
    (lock/acquire! g {:pr 1 :sha "old"} {:pid 4242})
    (write-lock! g {:pid 777 :pr 1 :sha "new" :started 1})
    (is (true? (lock/superseded? g {:pid 4242}))
        "this is how a loser learns it lost: its reviewer was killed
         mid-answer, and recording that truncated output would spend a cap
         slot and wake agent A with findings for a stale SHA")))

(deftest a-missing-lock-record-reads-as-superseded
  (let [g (tmp-git-dir)]
    (is (true? (lock/superseded? g {:pid 4242}))
        "no record can only mean this trigger was superseded and the winner
         has since released, or that something outside the loop deleted it;
         publishing a pass whose lock is gone is the riskier of the two")))
