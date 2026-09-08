(ns pr-review.lock-test
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
            [pr-review.flock :as flock]
            [pr-review.lock :as lock]))

(defn- tmp-repo []
  (let [d (str (fs/create-temp-dir {:prefix "pr-review-lock"}))]
    (fs/create-dirs (str d "/.git"))
    d))

(defn- write-lock! [repo m]
  (spit (lock/lock-path repo) (json/generate-string m)))

(deftest lock-path-is-under-dot-git
  (is (= "/r/.git/pr-review.lock" (lock/lock-path "/r"))))

(deftest alive-tracks-real-processes
  (is (true? (lock/alive? (.pid (java.lang.ProcessHandle/current)))))
  (is (false? (lock/alive? 999999)) "an absent PID must read as dead, not as held"))

(deftest acquire-on-free-repo-succeeds-and-records-identity
  (let [r (tmp-repo)
        res (lock/acquire! r {:pr 370 :sha "abc"} {:pid 4242})]
    (is (= :acquired (:status res)))
    (is (= {:pid 4242 :pr 370 :sha "abc"}
           (select-keys (lock/read-lock r) [:pid :pr :sha])))
    (is (pos? (:started (lock/read-lock r))))))

(deftest same-sha-in-flight-is-a-duplicate
  (let [r (tmp-repo)
        self (.pid (java.lang.ProcessHandle/current))]
    (write-lock! r {:pid self :pr 370 :sha "abc" :started 1})
    (is (= :duplicate (:status (lock/acquire! r {:pr 370 :sha "abc"} {:pid 1})))
        "two hooks for one push must not run two reviewers")
    (is (= self (:pid (lock/read-lock r))) "the incumbent keeps the lock")))

(deftest newer-sha-supersedes-and-kills-the-incumbent
  (let [r (tmp-repo)
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
  (let [r (tmp-repo)
        killed (atom nil)]
    (write-lock! r {:pid 999999 :pr 370 :sha "old" :started 1})
    (let [res (lock/acquire! r {:pr 370 :sha "new"}
                             {:pid 5 :kill-fn #(reset! killed %)})]
      (is (= :acquired (:status res))
          "a crashed reviewer must not block every future review")
      (is (nil? @killed) "nothing to kill when the holder is already gone"))))

(deftest corrupt-lock-file-is-treated-as-free
  (let [r (tmp-repo)]
    (spit (lock/lock-path r) "{not json")
    (is (= :acquired (:status (lock/acquire! r {:pr 1 :sha "s"} {:pid 6}))))))

(deftest release-removes-the-lock
  (let [r (tmp-repo)]
    (lock/acquire! r {:pr 1 :sha "s"} {:pid 7})
    (lock/release! r {:pid 7})
    (is (nil? (lock/read-lock r)))))

(deftest acquire-runs-under-the-shared-flock-on-the-guard-path
  (let [r (tmp-repo)
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
  (let [r (tmp-repo)]
    (spit (lock/lock-path r) "{}")
    (is (= :acquired (:status (lock/acquire! r {:pr 1 :sha "s"} {:pid 10})))
        "a lock record that is valid JSON but missing :pid must read as free, not
         throw: read-lock returning {} truthy would send (alive? nil) into
         (long nil), an NPE")))

(deftest release-does-not-delete-a-record-owned-by-a-different-pid
  (let [r (tmp-repo)]
    (write-lock! r {:pid 4242 :pr 370 :sha "new" :started 1})
    (lock/release! r {:pid 999})
    (is (= {:pid 4242 :pr 370 :sha "new"}
           (select-keys (lock/read-lock r) [:pid :pr :sha]))
        "release! must never delete a record that belongs to a different holder:
         a reviewer finishing normally must not be able to delete the record a
         concurrent acquire! just wrote for the process that superseded it")))

(deftest release-deletes-its-own-record
  (let [r (tmp-repo)]
    (write-lock! r {:pid 4242 :pr 370 :sha "new" :started 1})
    (lock/release! r {:pid 4242})
    (is (nil? (lock/read-lock r))
        "the ownership check must not be so strict it turns release! into a
         no-op for its own record")))

(deftest release-runs-under-the-shared-flock-on-the-guard-path
  (let [r (tmp-repo)
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
