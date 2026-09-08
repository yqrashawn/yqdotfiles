(ns pr-review.trigger-test
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [pr-review.ledger :as ledger]
            [pr-review.trigger :as trigger]))

(defn- tmp-repo []
  (let [d (str (fs/create-temp-dir {:prefix "pr-review-trigger"}))]
    (fs/create-dirs (str d "/.git"))
    d))

(defn- opts
  "Wire every collaborator to a stub so `decide` is exercised in isolation.

   :branch is read via `contains?`, not `(or branch \"feat/x\")`: the latter
   cannot distinguish an explicit `:branch nil` (simulating detached HEAD)
   from the key being omitted (the normal-branch default), so a caller
   passing `:branch nil` would silently get \"feat/x\" back instead of nil."
  [repo & {:keys [pr] :as kvs}]
  {:repo-root-fn (constantly repo)
   :branch-fn    (constantly (if (contains? kvs :branch) (:branch kvs) "feat/x"))
   :head-sha-fn  (constantly "headsha")
   :open-pr-fn   (constantly pr)})

(deftest no-repo-is-silent
  (let [d (trigger/decide {:cwd "/tmp"}
                          (assoc (opts nil) :repo-root-fn (constantly nil)))]
    (is (= :silent (:action d)))
    (is (str/includes? (:reason d) "not a git repo"))))

(deftest detached-head-is-silent
  (let [r (tmp-repo)
        d (trigger/decide {:cwd r} (opts r :branch nil))]
    (is (= :silent (:action d)))
    (is (str/includes? (:reason d) "detached HEAD")
        "must name detached HEAD specifically: this fixture's :open-pr-fn is
         already nil, so deleting the detached-HEAD branch of `decide` would
         still fall through to :silent via the no-open-PR branch, and this
         test would not catch it without a :reason assertion")))

(deftest no-open-pr-is-silent
  (let [r (tmp-repo)
        d (trigger/decide {:cwd r} (opts r :pr nil))]
    (is (= :silent (:action d)))
    (is (str/includes? (:reason d) "no open PR")
        "a push to a branch with no PR is the human's normal workflow, not an error")))

(deftest open-pr-yields-a-review-with-pass-one
  (let [r (tmp-repo)
        d (trigger/decide {:cwd r}
                          (opts r :pr {:number 370 :isDraft false :baseRefName "main"}))]
    (is (= :review (:action d)))
    (is (= 370 (:pr d)))
    (is (= 1 (:pass d)))
    (is (= "headsha" (:sha d)))
    (is (= "main" (:base-ref d)))
    (is (false? (:draft? d)))))

(deftest drafts-are-reviewed
  (let [r (tmp-repo)
        d (trigger/decide {:cwd r}
                          (opts r :pr {:number 9 :isDraft true :baseRefName "main"}))]
    (is (= :review (:action d)))
    (is (true? (:draft? d)))))

(deftest pass-number-comes-from-the-ledger
  (let [r (tmp-repo)]
    (doseq [n [1 2]]
      (ledger/append-pass! r {:pr 370 :sha (str n) :pass n :verdict "NOT_MERGEABLE"
                              :blocking 1 :followup 0 :coverage 0 :fingerprints []}))
    (is (= 3 (:pass (trigger/decide
                     {:cwd r}
                     (opts r :pr {:number 370 :isDraft false :baseRefName "main"})))))))

(deftest cap-stops-the-loop
  (let [r (tmp-repo)]
    (doseq [n (range 1 (inc ledger/max-passes))]
      (ledger/append-pass! r {:pr 370 :sha (str n) :pass n :verdict "NOT_MERGEABLE"
                              :blocking 1 :followup 0 :coverage 0 :fingerprints []}))
    (let [d (trigger/decide {:cwd r}
                            (opts r :pr {:number 370 :isDraft false :baseRefName "main"}))]
      (is (= :cap-reached (:action d)))
      (is (str/includes? (:reason d) "10")))))

(deftest twice-raised-fingerprints-are-carried-into-the-decision
  (let [r (tmp-repo)
        fp "src/a.clj:1:correctness/followup"]
    (doseq [n [1 2]]
      (ledger/append-pass! r {:pr 370 :sha (str n) :pass n :verdict "MERGEABLE"
                              :blocking 0 :followup 1 :coverage 0 :fingerprints [fp]}))
    (is (= [fp] (:prior-fingerprints
                 (trigger/decide {:cwd r}
                                 (opts r :pr {:number 370 :isDraft false
                                              :baseRefName "main"})))))))

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

(deftest unresolved-base-ref-skips-the-reviewer-and-the-ledger
  (let [r (tmp-repo)
        d {:repo-root r :pr 370 :pass 1 :sha "headsha" :base-ref "main"
           :draft? false :prior-fingerprints []}
        ;; :spawn-fn is a safety net, not the point of the test: if a
        ;; regression ever let review! reach the reviewer on this path, this
        ;; stub keeps the test from shelling out to a real `claude -p`.
        opts {:merge-base-fn (constantly nil)
              :diff-fn (constantly nil)
              :spawn-fn (fn [_ _ _] {:exit 0 :out "" :err ""})}
        result (#'trigger/review! d opts)]
    (is (= 2 (:exit result)))
    (is (str/includes? (:message result) "main")
        "the diagnostic must name the unresolved base ref")
    (is (str/includes? (:message result) "git fetch origin main")
        "the diagnostic must give the concrete command that fixes it")
    (is (empty? (ledger/read-passes r 370))
        "no findings were produced; spending one of the ten cap slots on a
         pass that never reviewed anything would let a PR reach \"cap
         reached\" without a single real review")))
