(ns pr-review.prepush-hook-test
  "Runs the installed `pre-push` hook as git runs it.

   A non-zero pre-push ABORTS the push, so the hook's exit status is not a
   detail — it is the difference between a missing review and a user who
   cannot push. Everything here executes the real script."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [pr-review.cloneindex :as ci]
            [pr-review.hookinstall :as hi]))

(def ^:private hook-source
  (str (fs/path (or (some-> (System/getProperty "babashka.config") fs/parent str)
                    (System/getProperty "user.dir"))
                "hooks" "pr-push-hook.sh")))

(def ^:private ref-lines
  "What git feeds a pre-push hook: '<local ref> <local sha> <remote ref>
   <remote sha>' per ref being pushed."
  (str "refs/heads/topic " (str/join (repeat 40 \a))
       " refs/heads/topic " (str/join (repeat 40 \0)) "\n"))

(defn- clone! []
  (let [d (str (fs/create-temp-dir {:prefix "pr-review-prepush"}))]
    (p/sh ["git" "init" "-q" "--initial-branch=main" d])
    d))

(defn- run-hook
  "Invokes the hook the way git does — argv is remote name and URL, ref lines
   on stdin, cwd the worktree top — with XDG_CACHE_HOME pointed at `cache`."
  [root cache]
  (let [hook (str (fs/path (hi/hooks-dir root {}) "pre-push"))]
    (p/sh ["sh" hook "origin" "git@github.com:o/r.git"]
          {:dir root :in ref-lines :extra-env {"XDG_CACHE_HOME" cache}})))

(deftest the-hook-records-the-clone-and-exits-zero
  (let [root (clone!)
        cache (str (fs/create-temp-dir {:prefix "pr-review-xdg"}))]
    (hi/install! root hook-source {})
    (let [{:keys [exit err]} (run-hook root cache)
          log (str (fs/path cache "pr-review-loop" "pushes.log"))]
      (is (zero? exit) (str "a non-zero pre-push aborts the push; stderr: " err))
      (is (fs/exists? log) "no clone pointer was written")
      (let [gd (first (ci/clones-since log 0))]
        (is (some? gd) "the pointer names no existing directory")
        (is (= (str (fs/canonicalize (fs/path root ".git")))
               (str (fs/canonicalize gd)))
            "the pointer must name the clone's git dir, which is what
             pr-review.pushlog reads the reflog from")))))

(deftest the-recorded-git-dir-is-the-clone-not-the-worktree
  ;; The whole point: a push from a linked worktree must be attributed to the
  ;; clone whose reflog holds the entry.
  (let [root (clone!)
        cache (str (fs/create-temp-dir {:prefix "pr-review-xdg"}))]
    (spit (str (fs/path root "f")) "x")
    (p/sh ["git" "add" "f"] {:dir root})
    (p/sh ["git" "-c" "user.email=t@t" "-c" "user.name=t" "commit" "-qm" "init"] {:dir root})
    (let [wt (str (fs/path (fs/parent root) (str (fs/file-name root) "-wt")))]
      (p/sh ["git" "worktree" "add" "-q" "--detach" wt] {:dir root})
      (hi/install! root hook-source {})
      (let [{:keys [exit]} (run-hook wt cache)
            log (str (fs/path cache "pr-review-loop" "pushes.log"))]
        (is (zero? exit))
        (is (= (str (fs/canonicalize (fs/path root ".git")))
               (str (fs/canonicalize (first (ci/clones-since log 0)))))
            "recorded the worktree's own gitdir instead of the clone's")))))

(deftest a-chained-hook-keeps-its-veto-and-its-stdin
  (let [root (clone!)
        cache (str (fs/create-temp-dir {:prefix "pr-review-xdg"}))
        dir (hi/hooks-dir root {})
        theirs (str (fs/path dir "pre-push"))
        seen (str (fs/path root "seen.txt"))]
    (fs/create-dirs dir)
    ;; Their hook records what it was handed, then rejects the push.
    (spit theirs (str "#!/bin/sh\ncat > " seen "\necho refused >&2\nexit 3\n"))
    (fs/set-posix-file-permissions theirs "rwxr-xr-x")
    (is (= :chained (:status (hi/install! root hook-source {}))))
    (let [{:keys [exit err]} (run-hook root cache)]
      (testing "the exit status is relayed, so their rejection still aborts"
        (is (= 3 exit)))
      (is (str/includes? (str err) "refused") "their stderr must reach the user")
      (testing "stdin was spooled and replayed, not consumed"
        (is (= ref-lines (slurp seen))))
      (testing "and the clone was still recorded before handing over"
        (is (seq (ci/clones-since (str (fs/path cache "pr-review-loop" "pushes.log")) 0)))))))

(deftest a-chained-hook-that-passes-lets-the-push-through
  (let [root (clone!)
        cache (str (fs/create-temp-dir {:prefix "pr-review-xdg"}))
        dir (hi/hooks-dir root {})
        theirs (str (fs/path dir "pre-push"))]
    (fs/create-dirs dir)
    (spit theirs "#!/bin/sh\ncat >/dev/null\nexit 0\n")
    (fs/set-posix-file-permissions theirs "rwxr-xr-x")
    (hi/install! root hook-source {})
    (is (zero? (:exit (run-hook root cache))))))

(deftest an-unwritable-cache-cannot-fail-the-push
  ;; The invariant that matters most. If the index cannot be written the loop
  ;; loses a review; if the hook exits non-zero the user cannot push at all.
  (let [root (clone!)
        cache (str (fs/create-temp-dir {:prefix "pr-review-xdg"}))]
    (hi/install! root hook-source {})
    (fs/set-posix-file-permissions cache "r-xr-xr-x")
    (try
      (let [{:keys [exit]} (run-hook root cache)]
        (is (zero? exit) "an unwritable index must not abort a push"))
      (finally (fs/set-posix-file-permissions cache "rwxr-xr-x")))))

(deftest a-broken-chained-hook-still-reports-its-own-status
  ;; A parked hook that is not executable: ours must not mask that as success
  ;; nor crash — it simply is not exec'd, and the push proceeds.
  (let [root (clone!)
        cache (str (fs/create-temp-dir {:prefix "pr-review-xdg"}))
        dir (hi/hooks-dir root {})]
    (fs/create-dirs dir)
    (spit (str (fs/path dir "pre-push")) "#!/bin/sh\nexit 0\n")
    (fs/set-posix-file-permissions (fs/path dir "pre-push") "rwxr-xr-x")
    (hi/install! root hook-source {})
    (fs/set-posix-file-permissions (fs/path dir hi/chained-name) "rw-r--r--")
    (is (zero? (:exit (run-hook root cache)))
        "a non-executable parked hook must be skipped, not fatal")))
