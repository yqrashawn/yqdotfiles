(ns pr-review.ledger-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [pr-review.flock :as flock]
            [pr-review.ledger :as ledger]))

(defn- tmp-git-dir
  "A stand-in for the clone's shared git directory. Every ledger function
   takes this, never a repo root: `<repo-root>/.git` is a file in a linked
   worktree."
  []
  (let [d (str (fs/create-temp-dir {:prefix "pr-review-ledger"}) "/.git")]
    (fs/create-dirs d)
    d))

(defn- pass
  [pr sha n & {:keys [fingerprints verdict]}]
  {:pr pr :sha sha :pass n :verdict (or verdict "NOT_MERGEABLE")
   :blocking 1 :followup 0 :coverage 0 :fingerprints (vec fingerprints)})

(deftest ledger-path-is-under-the-git-dir
  (is (= "/r/.git/pr-review-ledger.jsonl" (ledger/ledger-path "/r/.git")))
  (is (= "/main/.git/pr-review-ledger.jsonl"
         (ledger/ledger-path "/main/.git"))
      "given the shared git dir of a worktree, the ledger lands in the main
       clone — one ledger per repository is what makes the cap mean anything
       across worktrees"))

(deftest empty-ledger-starts-at-pass-one
  (let [g (tmp-git-dir)]
    (is (= [] (ledger/read-passes g 370)))
    (is (= 1 (ledger/next-pass-number (ledger/read-passes g 370))))
    (is (false? (ledger/cap-reached? (ledger/read-passes g 370))))))

(deftest passes-are-per-pr-and-monotonic
  (let [g (tmp-git-dir)]
    (ledger/append-pass! g (pass 370 "aaa" 1))
    (ledger/append-pass! g (pass 371 "bbb" 1))
    (ledger/append-pass! g (pass 370 "ccc" 2))
    (testing "reads are filtered by PR"
      (is (= ["aaa" "ccc"] (mapv :sha (ledger/read-passes g 370))))
      (is (= ["bbb"] (mapv :sha (ledger/read-passes g 371)))))
    (testing "next pass number counts only that PR"
      (is (= 3 (ledger/next-pass-number (ledger/read-passes g 370))))
      (is (= 2 (ledger/next-pass-number (ledger/read-passes g 371)))))))

(deftest append-stamps-a-timestamp
  (let [g (tmp-git-dir)
        e (ledger/append-pass! g (pass 1 "x" 1))]
    (is (pos? (:ts e)) "append-pass! must stamp :ts so the cap can be reasoned about over time")))

(deftest cap-blocks-at-max-passes
  (let [g (tmp-git-dir)]
    (doseq [n (range 1 (inc ledger/max-passes))]
      (ledger/append-pass! g (pass 9 (str n) n)))
    (is (true? (ledger/cap-reached? (ledger/read-passes g 9)))
        "at max-passes the trigger must refuse to spawn another reviewer")
    (is (false? (ledger/cap-reached? (ledger/read-passes g 10)))
        "the cap is per PR, not global")))

(deftest reviewed-sha-is-recognised-per-pr
  (let [g (tmp-git-dir)]
    (ledger/append-pass! g (pass 370 "deadbeef" 1))
    (is (true? (ledger/reviewed-sha? (ledger/read-passes g 370) "deadbeef"))
        "a repeat push of one commit must be recognisable, or the same SHA is
         reviewed again on every `git push --tags` and spends a cap slot each time")
    (is (false? (ledger/reviewed-sha? (ledger/read-passes g 370) "newsha")))
    (is (false? (ledger/reviewed-sha? (ledger/read-passes g 371) "deadbeef"))
        "SHA identity is scoped to the PR")
    (is (false? (ledger/reviewed-sha? (ledger/read-passes g 370) nil))
        "an unresolvable HEAD must not match every recorded pass")))

(deftest fingerprint-category-reads-after-the-last-colon
  (is (= "style" (ledger/fingerprint-category "src/a.clj:1:style")))
  (is (= "correctness/blocking"
         (ledger/fingerprint-category "src/a.clj:1:correctness/blocking")))
  (is (= "style" (ledger/fingerprint-category "src/pool:v2/file.clj:34:style"))
      "a path may contain a colon; splitting on colons would read \"v2\" as the
       category and silently mis-classify the finding")
  (is (nil? (ledger/fingerprint-category "nocolons"))))

(deftest only-followup-grade-categories-are-suppressible
  (testing "the spec authorises the one-re-raise rule for follow-up grade only"
    (is (true? (ledger/suppressible? "src/a.clj:1:correctness/followup")))
    (is (true? (ledger/suppressible? "src/a.clj:1:docs-accuracy")))
    (is (true? (ledger/suppressible? "src/a.clj:1:style"))))
  (testing "a blocking or coverage finding that survived two passes is not
            fixed; suppressing it makes the next pass report MERGEABLE with
            the defect still in the tree"
    (is (false? (ledger/suppressible? "src/a.clj:1:correctness/blocking")))
    (is (false? (ledger/suppressible? "src/a.clj:1:coverage")))))

(deftest suppressed-fingerprints-needs-two-raises-and-a-suppressible-category
  (let [blocking "src/a.clj:1:correctness/blocking"
        coverage "src/a.clj:2:coverage"
        followup "src/a.clj:3:correctness/followup"
        once     "src/a.clj:4:style"
        passes [(pass 1 "a" 1 :fingerprints [blocking coverage followup])
                (pass 1 "b" 2 :fingerprints [blocking coverage followup once])]]
    (is (= [followup] (ledger/suppressed-fingerprints passes))
        "a blocking finding reported on two passes must still be reported on
         the third: filtering only on the raise count is a false-clean path,
         not a convergence aid")
    (is (= [] (ledger/suppressed-fingerprints [(first passes)]))
        "one sighting is not a re-raise")))

(deftest suppressed-fingerprints-counts-a-pass-once
  (let [fp "src/a.clj:9:style"
        passes [(pass 1 "a" 1 :fingerprints [fp fp])]]
    (is (= [] (ledger/suppressed-fingerprints passes))
        "one pass reporting the same finding twice is still one raise")))

(deftest suppressed-fingerprints-order-is-first-appearance
  (let [a "z/a.clj:1:style" b "a/b.clj:2:style" c "m/c.clj:3:docs-accuracy"
        passes [(pass 1 "1" 1 :fingerprints [a b c])
                (pass 1 "2" 2 :fingerprints [a b c])]]
    (is (= [a b c] (ledger/suppressed-fingerprints passes))
        "the do-not-re-raise list is rendered into the prompt; a hash-order
         list would churn the prompt between passes for no reason")))

(deftest suppressed-fingerprints-reads-the-ledger-once
  (testing "the filter used to call a per-fingerprint helper that re-slurped
            the whole ledger: 138 full file reads for one decision at nine
            passes and fifteen findings"
    (let [g (tmp-git-dir)
          fp "src/a.clj:1:style"]
      (doseq [n [1 2]]
        (ledger/append-pass! g (pass 370 (str n) n :fingerprints [fp])))
      (let [reads (atom 0)
            orig  slurp]
        (with-redefs [slurp (fn [& args] (swap! reads inc) (apply orig args))]
          (let [passes (ledger/read-passes g 370)]
            (is (= [fp] (ledger/suppressed-fingerprints passes)))))
        (is (= 1 @reads)
            "one decision must cost exactly one read of the ledger file")))))

(deftest corrupt-lines-are-skipped-not-fatal
  (let [g (tmp-git-dir)]
    (ledger/append-pass! g (pass 2 "ok" 1))
    (spit (ledger/ledger-path g) "{not json\n" :append true)
    (is (= ["ok"] (mapv :sha (ledger/read-passes g 2)))
        "a truncated write from a killed reviewer must not break every later read")))

(deftest interrupted-write-does-not-lose-prior-passes
  (let [g (tmp-git-dir)]
    (ledger/append-pass! g (pass 42 "first" 1))
    ;; Simulate a reviewer killed after the new pass is durably written to the
    ;; temp file but before it is published.
    (with-redefs [ledger/atomic-replace! (fn [_ _] (throw (ex-info "simulated crash before publish" {})))]
      (is (thrown? Exception (ledger/append-pass! g (pass 42 "second" 2)))))
    (is (= ["first"] (mapv :sha (ledger/read-passes g 42)))
        "a crash between the temp write and the atomic rename must leave every
         previously recorded pass intact, not zero the ledger")))

(deftest append-pass-flocks-a-guard-file-not-the-ledger-path
  (let [g (tmp-git-dir)
        seen (atom [])]
    (with-redefs [flock/with-file-lock (fn [path f] (swap! seen conj path) (f))]
      (ledger/append-pass! g (pass 77 "one" 1))
      (ledger/append-pass! g (pass 77 "two" 2)))
    (testing "both sequential appends still land"
      (is (= ["one" "two"] (mapv :sha (ledger/read-passes g 77)))))
    (testing "the flock target is a sibling guard file, never the ledger path append-pass! renames over"
      (is (= [(flock/guard-path (ledger/ledger-path g)) (flock/guard-path (ledger/ledger-path g))]
             @seen))
      (is (not-any? #{(ledger/ledger-path g)} @seen)
          "flocking the path that gets renamed over lets a second process later lock a
           different inode after the rename and run concurrently with this one — the
           exact defect this test guards against"))))
