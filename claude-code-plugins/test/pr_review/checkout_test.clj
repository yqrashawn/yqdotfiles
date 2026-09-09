(ns pr-review.checkout-test
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [pr-review.checkout :as co]))

(defn- clone!
  "A clone with two commits, so a checkout can be pinned to a sha that is not
   HEAD — which is the only interesting case."
  []
  (let [d (str (fs/create-temp-dir {:prefix "pr-review-checkout"}))
        g (fn [& args] (p/sh (into ["git" "-c" "user.email=t@t" "-c" "user.name=t"] args) {:dir d}))]
    (p/sh ["git" "init" "-q" "--initial-branch=main" d])
    (spit (str (fs/path d "a.txt")) "first\n")
    (g "add" "a.txt") (g "commit" "-qm" "one")
    (let [first-sha (str/trim (:out (g "rev-parse" "HEAD")))]
      (spit (str (fs/path d "a.txt")) "second\n")
      (g "add" "a.txt") (g "commit" "-qm" "two")
      {:root d
       :git-dir (str (fs/path d ".git"))
       :first first-sha
       :head (str/trim (:out (g "rev-parse" "HEAD")))})))

(defn- parent [] (str (fs/create-temp-dir {:prefix "pr-review-co-parent"})))

(deftest the-checkout-is-pinned-to-the-reviewed-sha-not-head
  ;; The whole point: the reviewer must see the code the PR points at, not
  ;; whatever the agent's tree currently holds.
  (let [{:keys [git-dir first head]} (clone!)
        seen (atom nil)]
    (co/with-checkout git-dir first (parent) {}
      (fn [dir]
        (reset! seen {:dir dir
                      :content (slurp (str (fs/path dir "a.txt")))
                      :sha (str/trim (:out (p/sh ["git" "rev-parse" "HEAD"] {:dir dir})))})))
    (is (= "first\n" (:content @seen)) "checked out the wrong commit")
    (is (= first (:sha @seen)))
    (is (not= head (:sha @seen)))))

(deftest the-checkout-is-removed-afterwards
  (let [{:keys [git-dir first]} (clone!)
        pdir (parent)
        captured (atom nil)]
    (co/with-checkout git-dir first pdir {} (fn [dir] (reset! captured dir)))
    (is (some? @captured))
    (is (not (fs/exists? @captured)) "the checkout leaked")
    (is (empty? (fs/list-dir pdir)) "the parent must be left clean")))

(deftest a-throwing-review-still-removes-the-checkout
  ;; A reviewer that dies must not leave a tree behind; the path is derived
  ;; from the sha, so a leak would block every later review of that sha.
  (let [{:keys [git-dir first]} (clone!)
        pdir (parent)
        captured (atom nil)]
    (is (thrown? Exception
                 (co/with-checkout git-dir first pdir {}
                   (fn [dir] (reset! captured dir) (throw (ex-info "boom" {}))))))
    (is (not (fs/exists? @captured)))))

(deftest a-sha-git-refuses-yields-nil-rather-than-an-exception
  ;; The caller decides whether an unavailable sha is fatal. Throwing from
  ;; here would surface as the hook exiting non-zero on a condition that is
  ;; merely a missed review.
  (let [{:keys [git-dir]} (clone!)
        seen (atom :untouched)]
    (co/with-checkout git-dir (str/join (repeat 40 \e)) (parent) {}
      (fn [dir] (reset! seen dir)))
    (is (nil? @seen))))

(deftest a-killed-run-does-not-block-the-same-sha-forever
  ;; Reproduces the measured failure: a session restart killed a reviewer
  ;; mid-run. Here the directory is destroyed without unregistering it, which
  ;; is what that leaves behind — git then refuses the path until pruned.
  (let [{:keys [git-dir first]} (clone!)
        pdir (parent)
        leaked (atom nil)]
    (co/with-checkout git-dir first pdir {} (fn [dir] (reset! leaked dir)))
    ;; re-add, then simulate the kill: tree gone, admin entry still registered
    (let [dir (co/add! git-dir first pdir {})]
      (is (some? dir))
      (fs/delete-tree dir)
      (is (str/includes? (str (:out (p/sh ["git" (str "--git-dir=" git-dir)
                                           "worktree" "list"] {})))
                         "prunable")
          "the stale entry should still be registered"))
    (let [second-try (atom :untouched)]
      (co/with-checkout git-dir first pdir {} (fn [dir] (reset! second-try dir)))
      (is (some? @second-try) "a stale entry blocked a later review of that sha"))))

(deftest a-leaked-checkout-whose-directory-survives-is-reclaimed
  (testing "the case `worktree prune` cannot reach, and the one that actually
            happens: a reviewer killed while its temp directory survives.
            Measured — a review killed at the two-minute mark left a
            registered worktree and prune reported nothing prunable, so git
            kept refusing to reuse that path"
    (let [{:keys [git-dir first]} (clone!)
          pdir (parent)
          dir (co/add! git-dir first pdir {})]
      (is (some? dir))
      ;; the kill: nothing unregisters it, and the tree is still on disk
      (co/prune! git-dir {})                       ; finds nothing to do
      (is (fs/exists? dir))
      (testing "young enough to be a review in flight — must be left alone"
        (is (= 0 (co/prune-stale! git-dir {})))
        (is (fs/exists? dir)))
      (testing "older than the window — reclaimed, directory and registration"
        (fs/set-last-modified-time
         dir (- (System/currentTimeMillis) (+ co/stale-after-ms 60000)))
        (is (= 1 (co/prune-stale! git-dir {})))
        (is (not (fs/exists? dir)))
        (is (not (str/includes?
                  (str (:out (p/sh ["git" (str "--git-dir=" git-dir)
                                    "worktree" "list"] {})))
                  (str (fs/file-name dir))))
            "the registration must go too, or git still owns the path")))))

(deftest starting-a-review-reclaims-an-older-leak
  (testing "the wiring, not just the function: `with-checkout` is the only
            thing that ever runs on a real machine, so a leak is only
            reclaimed if IT prunes. Reviews are the only recurring event in
            this system — there is nothing else to hang periodic cleanup on"
    (let [{:keys [git-dir first head]} (clone!)
          pdir (parent)
          leaked (co/add! git-dir first pdir {})]
      (is (some? leaked))
      (fs/set-last-modified-time
       leaked (- (System/currentTimeMillis) (+ co/stale-after-ms 60000)))
      (co/with-checkout git-dir head pdir {}
        (fn [dir]
          (is (some? dir) "the new review still gets its own checkout")
          (is (not (fs/exists? leaked))
              "and the older leak was reclaimed on the way in"))))))

(deftest the-main-worktree-is-never-a-pruning-candidate
  ;; Found by a test whose temp clone was named `pr-review-checkout...`: a
  ;; bare `pr-review-` prefix test made the CLONE ITSELF a candidate.
  (let [{:keys [git-dir root]} (clone!)]
    (fs/set-last-modified-time
     root (- (System/currentTimeMillis) (* 30 24 60 60 1000)))
    (is (= 0 (co/prune-stale! git-dir {})))
    (is (fs/exists? root) "the clone must never be removed by its own pruner")))

(deftest a-leak-at-the-same-sha-does-not-block-a-retry
  (testing "the case the retry path always hits: a retry is by definition of
            the same sha the killed review had, the path is derived from the
            sha, and git refuses a path it still believes it owns even after
            the directory is deleted. Measured — the retry fired for PR #401,
            found the leaked checkout registered, and reported \"could not
            check out\". prune-stale! cannot help: it is age-based at twelve
            hours and the leak in the way is minutes old"
    (let [{:keys [git-dir first]} (clone!)
          pdir (parent)
          leaked (co/add! git-dir first pdir {})]
      (is (some? leaked))
      ;; the kill: directory gone, registration left behind, seconds old
      (fs/delete-tree leaked)
      (is (str/includes? (str (:out (p/sh ["git" (str "--git-dir=" git-dir)
                                           "worktree" "list"] {})))
                         "pr-review-")
          "the stale registration must still be there for this to test anything")
      (let [again (atom :never-ran)]
        (co/with-checkout git-dir first pdir {} (fn [d] (reset! again d)))
        (is (some? @again) "the retry must reclaim the path, not refuse")
        (is (= leaked @again) "and it is the same sha-derived path")))))

(deftest a-live-review-is-never-pruned-by-age
  ;; A review takes about seven minutes against a twelve-hour window; the
  ;; margin is what makes age alone safe, so no lock correlation is needed.
  (let [{:keys [git-dir first]} (clone!)
        pdir (parent)
        seen (atom nil)]
    (co/with-checkout git-dir first pdir {}
      (fn [dir]
        (reset! seen dir)
        (is (= 0 (co/prune-stale! git-dir {}))
            "with-checkout prunes on entry; it must not eat its own tree")
        (is (fs/exists? dir))))
    (is (not (fs/exists? @seen)) "and it is still removed normally at the end")))

(deftest only-this-namespaces-worktrees-are-pruned
  ;; The agent's own worktrees live in the same clone and are frequently
  ;; older than twelve hours.
  (let [{:keys [git-dir root first]} (clone!)
        theirs (str (fs/path (fs/parent root) "wt-someones-work"))]
    (p/sh ["git" (str "--git-dir=" git-dir) "worktree" "add" "-q" "--detach"
           theirs first] {:dir root})
    (fs/set-last-modified-time
     theirs (- (System/currentTimeMillis) (* 30 24 60 60 1000)))
    (is (= 0 (co/prune-stale! git-dir {})))
    (is (fs/exists? theirs) "a worktree this namespace did not create is not ours to remove")))

(deftest remove-deletes-the-directory-even-when-git-declines
  ;; `worktree remove` refuses on some dirty states. The directory must go
  ;; anyway, or the sha-derived path stays occupied.
  (let [{:keys [git-dir first]} (clone!)
        pdir (parent)
        dir (co/add! git-dir first pdir {})]
    (is (some? dir))
    (co/remove! git-dir dir {:sh (fn [_ _] {:exit 1 :out "" :err "refused"})})
    (is (not (fs/exists? dir)))))

(deftest concurrent-reviews-of-different-shas-do-not-collide
  ;; Two PRs in one clone review at once; the path is per-sha for exactly this.
  (let [{:keys [git-dir first head]} (clone!)
        pdir (parent)
        a (co/add! git-dir first pdir {})
        b (co/add! git-dir head pdir {})]
    (is (some? a)) (is (some? b))
    (is (not= a b))
    (is (= "first\n" (slurp (str (fs/path a "a.txt")))))
    (is (= "second\n" (slurp (str (fs/path b "a.txt")))))
    (co/remove! git-dir a {}) (co/remove! git-dir b {})))
