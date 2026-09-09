(ns pr-review.manual-test
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [pr-review.ledger :as ledger]
            [pr-review.manual :as manual]))

(defn- tmp-repo []
  (let [d (str (fs/create-temp-dir {:prefix "pr-review-manual"}))
        g (str d "/.git")]
    (fs/create-dirs g)
    [d g]))

(defn- row [pr sha n]
  {:pr pr :sha sha :pass n :verdict "MERGEABLE"
   :blocking 0 :followup 0 :coverage 0 :fingerprints []})

(defn- opts
  "`gh pr view` stubbed by number, and the git accessors by value."
  [& {:keys [repo git-dir branch prs]}]
  {:repo-root-fn (constantly repo)
   :git-dir-fn   (constantly git-dir)
   :branch-fn    (constantly branch)
   :sh (fn [args _dir]
         (let [n (some-> (nth args 3 nil) parse-long)]
           (if-let [pr (get prs n)]
             {:exit 0 :out (json/generate-string pr) :err ""}
             {:exit 1 :out "" :err "no pr"})))})

(defn- a-pr [n head & {:keys [state draft? branch]}]
  {:number n :headRefOid head :headRefName (or branch "feat/x")
   :baseRefName "main" :isDraft (boolean draft?) :state (or state "OPEN")})

(deftest a-named-pr-is-reviewed-without-any-push-evidence
  (testing "the reason to ask by hand is usually that the push evidence is
            gone, outside the window, or was never recorded — a killed review,
            a machine where the hook was not installed yet, a PR someone else
            opened. So this path must not consult it at all"
    (let [[r g] (tmp-repo)
          d (manual/decide {:dir r :pr 401}
                           (opts :repo r :git-dir g :branch "other-branch"
                                 :prs {401 (a-pr 401 "sha401" :branch "feat/a")}))]
      (is (= :review (:action d)))
      (is (= 401 (:pr d)))
      (is (= "sha401" (:sha d)))
      (is (= "feat/a" (:branch d)) "the branch comes from the PR, not the checkout")
      (is (= 1 (:pass d)))
      (is (= "main" (:base-ref d)))
      (is (= :manual (:trigger d)) "so a ledger row's provenance is legible"))))

(deftest with-no-number-the-current-branch-decides
  (let [[r g] (tmp-repo)
        d (manual/decide {:dir r}
                         (assoc (opts :repo r :git-dir g :branch "feat/x" :prs {})
                                :open-pr-fn (fn [_ b _]
                                              (when (= "feat/x" b)
                                                (a-pr 370 "shax")))))]
    (is (= :review (:action d)))
    (is (= 370 (:pr d)))))

(deftest an-already-reviewed-head-is-refused-with-the-reason
  (testing "a manual re-review of the same sha would spend a cap slot and tell
            the author nothing new, so it says so rather than doing it"
    (let [[r g] (tmp-repo)]
      (ledger/append-pass! g (row 401 "sha401" 1))
      (let [d (manual/decide {:dir r :pr 401}
                             (opts :repo r :git-dir g :branch "feat/a"
                                   :prs {401 (a-pr 401 "sha401")}))]
        (is (= :silent (:action d)))
        (is (str/includes? (:reason d) "already has a recorded pass"))
        (is (str/includes? (:reason d) "push a commit"))))))

(deftest the-cap-applies-to-a-manual-review-too
  (let [[r g] (tmp-repo)]
    (doseq [n (range 1 (inc ledger/max-passes))]
      (ledger/append-pass! g (row 401 (str "sha" n) n)))
    (is (= :cap-reached
           (:action (manual/decide {:dir r :pr 401}
                                   (opts :repo r :git-dir g :branch "feat/a"
                                         :prs {401 (a-pr 401 "shaN")})))))))

(deftest a-closed-or-unknown-pr-is-refused
  (let [[r g] (tmp-repo)
        o (opts :repo r :git-dir g :branch "feat/a"
                :prs {401 (a-pr 401 "sha401" :state "MERGED")})]
    (testing "merged"
      (let [d (manual/decide {:dir r :pr 401} o)]
        (is (= :silent (:action d)))
        (is (str/includes? (:reason d) "not open"))))
    (testing "no such PR"
      (is (str/includes? (:reason (manual/decide {:dir r :pr 999} o)) "not open")))))

(deftest a-detached-head-with-no-number-says-so
  (let [[r g] (tmp-repo)
        d (manual/decide {:dir r} (opts :repo r :git-dir g :branch nil :prs {}))]
    (is (= :silent (:action d)))
    (is (str/includes? (:reason d) "detached HEAD"))))

(deftest outside-a-repository-nothing-happens
  (let [d (manual/decide {:dir "/nowhere" :pr 1}
                         (assoc (opts :prs {}) :repo-root-fn (constantly nil)))]
    (is (= :silent (:action d)))
    (is (str/includes? (:reason d) "not a git repository"))))

(deftest prior-fingerprints-are-carried-so-a-manual-pass-still-converges
  (let [[r g] (tmp-repo)
        fp "src/a.clj:1:correctness/followup"]
    (doseq [n [1 2]]
      (ledger/append-pass! g (assoc (row 401 (str "s" n) n) :fingerprints [fp])))
    (is (= [fp] (:prior-fingerprints
                 (manual/decide {:dir r :pr 401}
                                (opts :repo r :git-dir g :branch "feat/a"
                                      :prs {401 (a-pr 401 "sha-new")})))))))
