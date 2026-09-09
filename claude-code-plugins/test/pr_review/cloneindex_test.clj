(ns pr-review.cloneindex-test
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [pr-review.cloneindex :as ci]))

(defn- tmp [] (str (fs/create-temp-dir {:prefix "pr-review-cloneindex"})))

(defn- log!
  "A pushes.log holding [seconds git-dir] entries."
  [dir entries]
  (let [f (str (fs/path dir "pushes.log"))]
    (spit f (str/join "" (for [[ts gd] entries] (format "%d\t%s\n" ts gd))))
    f))

(defn- clone!
  "A directory that passes the `fs/directory?` admission test."
  [dir name]
  (let [d (str (fs/path dir name))] (fs/create-dirs d) d))

(deftest the-default-log-is-under-xdg-cache-not-tmpdir
  ;; TMPDIR measured at three distinct values on this machine, and the git
  ;; hook runs outside any session — a temp path is a place the writer and the
  ;; reader would never meet.
  (let [p (ci/default-log)]
    (is (str/ends-with? p "/pr-review-loop/pushes.log"))
    (is (not (str/includes? p "/var/folders/")) "a per-session temp path")
    (is (not (str/includes? p "/T/")) "a per-session temp path")))

(deftest entries-are-newest-first-and-distinct
  (let [d (tmp)
        a (clone! d "a") b (clone! d "b")
        f (log! d [[1000 a] [2000 b] [3000 a]])]
    (is (= [a b] (ci/clones-since f 0))
        "a clone pushed twice appears once, at its newest push")))

(deftest entries-older-than-the-window-are-excluded
  (let [d (tmp)
        a (clone! d "a") b (clone! d "b")
        f (log! d [[1000 a] [5000 b]])]
    (is (= [b a] (ci/clones-since f 0)))
    (is (= [b] (ci/clones-since f 2000000)))
    (is (empty? (ci/clones-since f 9000000)))))

(deftest a-clone-that-no-longer-exists-is-dropped
  ;; A deleted clone would only cost the caller a failed git invocation.
  (let [d (tmp)
        a (clone! d "a")
        f (log! d [[1000 a] [2000 (str (fs/path d "deleted"))]])]
    (is (= [a] (ci/clones-since f 0)))))

(deftest malformed-lines-are-skipped-not-fatal
  ;; The writer is a POSIX sh hook that must never fail a push, so it cannot
  ;; validate what it appends — a truncated line has to be survivable here.
  (let [d (tmp)
        a (clone! d "a")
        f (str (fs/path d "pushes.log"))]
    (spit f (str "not-a-line\n"
                 "\n"
                 "1000\n"
                 (format "1000\t%s\n" a)
                 "abc\t/nope\n"
                 "2000\t\n"))
    (is (= [a] (ci/clones-since f 0)))))

(deftest a-log-that-does-not-exist-is-empty-not-an-error
  ;; Before the first push on a machine there is no log at all.
  (is (empty? (ci/clones-since (str (fs/path (tmp) "absent.log")) 0))))

(deftest prune-drops-old-entries-and-keeps-recent-ones
  (let [d (tmp)
        a (clone! d "a") b (clone! d "b")
        now (quot (System/currentTimeMillis) 1000)
        f (log! d [[(- now 100000) a] [(- now 10) b]])]
    (is (= 1 (ci/prune! f (* 60 1000))))
    (is (= [b] (ci/clones-since f 0)))
    (testing "the surviving line keeps its trailing newline, so the hook's
              next append starts a line rather than joining onto this one"
      (is (str/ends-with? (slurp f) "\n")))))

(deftest pruning-everything-leaves-an-empty-file-the-hook-can-append-to
  (let [d (tmp)
        a (clone! d "a")
        f (log! d [[1000 a]])]
    (is (= 0 (ci/prune! f 1000)))
    (is (= "" (slurp f)))
    (is (empty? (ci/clones-since f 0)))))

(deftest prune-on-a-missing-log-is-a-no-op
  (is (nil? (ci/prune! (str (fs/path (tmp) "absent.log")) 1000))))
