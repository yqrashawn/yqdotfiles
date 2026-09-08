(ns pr-review.lock-test
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
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
    (lock/release! r)
    (is (nil? (lock/read-lock r)))))
