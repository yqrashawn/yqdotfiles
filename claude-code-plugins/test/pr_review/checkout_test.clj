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
