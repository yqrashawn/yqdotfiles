(ns pr-review.ledger-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [pr-review.ledger :as ledger]))

(defn- tmp-repo
  "A directory with a .git subdir, standing in for a clone."
  []
  (let [d (str (fs/create-temp-dir {:prefix "pr-review-ledger"}))]
    (fs/create-dirs (str d "/.git"))
    d))

(deftest ledger-path-is-under-dot-git
  (is (= "/r/.git/pr-review-ledger.jsonl" (ledger/ledger-path "/r"))))

(deftest empty-ledger-starts-at-pass-one
  (let [r (tmp-repo)]
    (is (= [] (ledger/read-passes r 370)))
    (is (= 1 (ledger/next-pass-number r 370)))
    (is (false? (ledger/cap-reached? r 370)))))

(deftest passes-are-per-pr-and-monotonic
  (let [r (tmp-repo)]
    (ledger/append-pass! r {:pr 370 :sha "aaa" :pass 1 :verdict "NOT_MERGEABLE"
                            :blocking 2 :followup 0 :coverage 0 :fingerprints []})
    (ledger/append-pass! r {:pr 371 :sha "bbb" :pass 1 :verdict "MERGEABLE"
                            :blocking 0 :followup 1 :coverage 0 :fingerprints []})
    (ledger/append-pass! r {:pr 370 :sha "ccc" :pass 2 :verdict "MERGEABLE"
                            :blocking 0 :followup 1 :coverage 0 :fingerprints []})
    (testing "reads are filtered by PR"
      (is (= ["aaa" "ccc"] (mapv :sha (ledger/read-passes r 370))))
      (is (= ["bbb"] (mapv :sha (ledger/read-passes r 371)))))
    (testing "next pass number counts only that PR"
      (is (= 3 (ledger/next-pass-number r 370)))
      (is (= 2 (ledger/next-pass-number r 371))))))

(deftest append-stamps-a-timestamp
  (let [r (tmp-repo)
        e (ledger/append-pass! r {:pr 1 :sha "x" :pass 1 :verdict "MERGEABLE"
                                  :blocking 0 :followup 0 :coverage 0 :fingerprints []})]
    (is (pos? (:ts e)) "append-pass! must stamp :ts so the cap can be reasoned about over time")))

(deftest raise-count-counts-fingerprint-occurrences
  (let [r (tmp-repo)
        fp "src/a.clj:12:correctness/followup"]
    (ledger/append-pass! r {:pr 5 :sha "a" :pass 1 :verdict "NOT_MERGEABLE"
                            :blocking 1 :followup 0 :coverage 0 :fingerprints [fp]})
    (is (= 1 (ledger/raise-count r 5 fp)))
    (ledger/append-pass! r {:pr 5 :sha "b" :pass 2 :verdict "MERGEABLE"
                            :blocking 0 :followup 1 :coverage 0 :fingerprints [fp]})
    (is (= 2 (ledger/raise-count r 5 fp))
        "a second raise must be visible so the one-re-raise rule can fire")
    (is (= 0 (ledger/raise-count r 5 "other:1:style")))))

(deftest cap-blocks-at-max-passes
  (let [r (tmp-repo)]
    (doseq [n (range 1 (inc ledger/max-passes))]
      (ledger/append-pass! r {:pr 9 :sha (str n) :pass n :verdict "NOT_MERGEABLE"
                              :blocking 1 :followup 0 :coverage 0 :fingerprints []}))
    (is (true? (ledger/cap-reached? r 9))
        "at max-passes the trigger must refuse to spawn another reviewer")
    (is (false? (ledger/cap-reached? r 10)) "the cap is per PR, not global")))

(deftest corrupt-lines-are-skipped-not-fatal
  (let [r (tmp-repo)]
    (ledger/append-pass! r {:pr 2 :sha "ok" :pass 1 :verdict "MERGEABLE"
                            :blocking 0 :followup 0 :coverage 0 :fingerprints []})
    (spit (ledger/ledger-path r) "{not json\n" :append true)
    (is (= ["ok"] (mapv :sha (ledger/read-passes r 2)))
        "a truncated write from a killed reviewer must not break every later read")))
