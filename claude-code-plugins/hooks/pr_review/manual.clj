(ns pr-review.manual
  "Review a PR now, without a push.

   The trigger asks \"is there a recorded agent push whose sha is an open PR's
   head\". That is the right question for a hook and the wrong one for a human:
   the reason to ask for a review by hand is usually that the recorded push is
   gone, outside the window, or was never recorded — a review killed mid-run, a
   machine where the `pre-push` hook was not installed yet, a PR someone else
   opened.

   So this path skips the push evidence entirely and takes the PR as the
   subject. Everything downstream is unchanged: the same lock, the same pinned
   checkout, the same reviewer, the same ledger row and the same cap. It is
   synchronous — findings come back on stdout, because there is no turn to wake."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [pr-review.gh :as gh]
            [pr-review.ledger :as ledger]
            [pr-review.trigger :as trigger]))

(defn- open-pr-by-number
  "The PR as `gh/open-pr` would have returned it, looked up by number."
  [repo-root n opts]
  (let [{:keys [exit out]} ((or (:sh opts) gh/default-sh)
                            ["gh" "pr" "view" (str n) "--json"
                             "number,isDraft,baseRefName,headRefOid,headRefName,state"]
                            repo-root)]
    (when (zero? exit)
      (let [pr (try (json/parse-string out true) (catch Exception _ nil))]
        (when (= "OPEN" (:state pr)) pr)))))

(defn decide
  "The same decision shape `pr-review.trigger/decide` returns, for a PR named
   directly. `:action` is :silent with a reason when there is nothing to do."
  [{:keys [dir pr]} opts]
  (if-let [repo-root ((or (:repo-root-fn opts) gh/repo-root) dir opts)]
    (let [git-dir (or ((or (:git-dir-fn opts) gh/git-common-dir) repo-root opts)
                      (str repo-root "/.git"))
          branch ((or (:branch-fn opts) gh/current-branch) repo-root opts)
          info (if pr
                 ((or (:pr-by-number-fn opts) open-pr-by-number) repo-root pr opts)
                 (when branch
                   ((or (:open-pr-fn opts) gh/open-pr) repo-root branch opts)))]
      (cond
        (nil? info)
        {:action :silent
         :reason (if pr
                   (str "PR #" pr " is not open, or gh could not read it")
                   (str "no open PR for branch " (or branch "«detached HEAD»")))}

        :else
        (let [pr-num (:number info)
              sha (:headRefOid info)
              passes (ledger/read-passes git-dir pr-num)
              base {:trigger :manual :candidates 1
                    :git-dir git-dir :repo-root repo-root
                    :branch (or (:headRefName info) branch)
                    :pr pr-num :sha sha}]
          (cond
            (ledger/reviewed-sha? passes sha)
            (assoc base :action :silent
                   :reason (str "PR #" pr-num " already has a recorded pass at "
                                sha " — push a commit to get a new one"))

            (ledger/cap-reached? passes)
            (assoc base :action :cap-reached
                   :reason (str "review cap of " ledger/max-passes
                                " passes reached for PR #" pr-num))

            :else
            (assoc base :action :review
                   :pass (ledger/next-pass-number passes)
                   :base-ref (:baseRefName info)
                   :draft? (boolean (:isDraft info))
                   :prior-fingerprints (ledger/suppressed-fingerprints passes))))))
    {:action :silent :reason (str "not a git repository: " dir)}))

(defn -main
  "Usage: bb review-pr [<pr-number>]

   With no argument, reviews the open PR for the current branch. Exits 0
   whether or not a review ran; unlike the hook there is no wake to earn, and
   a non-zero exit here would only look like a crash to whoever invoked it."
  [& args]
  (let [n (some-> (first args) str/trim not-empty parse-long)
        d (decide {:dir (System/getProperty "user.dir") :pr n} {})
        {:keys [message]} (#'trigger/respond d {})]
    (println (or message (str "pr-review-loop: no review ran — " (:reason d))))
    (flush)
    (System/exit 0)))
