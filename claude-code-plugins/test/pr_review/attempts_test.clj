(ns pr-review.attempts-test
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [pr-review.attempts :as attempts]))

(defn- tmp [] (str (fs/create-temp-dir {:prefix "pr-review-attempts"})))
(defn- sha [c] (str/join (repeat 40 c)))

(defn- log!
  "A pushes.log. A 3-element entry is a clone record, a 5-element one an
   attempt — the two shapes the hook writes."
  [dir entries]
  (let [f (str (fs/path dir "pushes.log"))]
    (spit f (str/join "" (for [e entries] (str (str/join "\t" e) "\n"))))
    f))

(defn- clone! [dir name]
  (let [d (str (fs/path dir name))] (fs/create-dirs d) d))

(deftest the-default-log-is-under-xdg-cache-not-tmpdir
  ;; TMPDIR measured at three distinct values on this machine, and the git
  ;; hook runs outside any session — a temp path is a place the writer and the
  ;; reader would never meet.
  (let [p (attempts/default-log)]
    (is (str/ends-with? p "/pr-review-loop/pushes.log"))
    (is (not (str/includes? p "/var/folders/")))
    (is (not (str/includes? p "/T/")))))

;; ------------------------------------------------------------- provenance

(deftest an-agent-push-carries-its-session-and-a-human-push-does-not
  (testing "R2, decided by what MADE the push rather than by how the command
            was spelled. Claude Code exports CLAUDE_CODE_SESSION_ID into a
            Bash tool call and every child inherits it; a terminal has none"
    (let [d (tmp)
          g (clone! d "g")
          f (log! d [[1000 g "sess-a" "refs/heads/feat/x" (sha \a)]
                     [2000 g "-" "refs/heads/feat/human" (sha \b)]])
          [human agent] (attempts/attempts-since f 0)]
      (is (= "-" (:session human)))
      (is (false? (attempts/by-agent? human)))
      (is (= "sess-a" (:session agent)))
      (is (true? (attempts/by-agent? agent))))))

(deftest an-attempt-carries-the-branch-not-just-the-ref
  ;; The reflog names branches; the hook is handed refs. They have to meet.
  (let [d (tmp)
        g (clone! d "g")
        f (log! d [[1000 g "s" "refs/heads/fix/llm-logs/deep" (sha \a)]])]
    (is (= "fix/llm-logs/deep" (:branch (first (attempts/attempts-since f 0)))))
    (is (= "refs/heads/fix/llm-logs/deep" (:ref (first (attempts/attempts-since f 0)))))))

(deftest a-ref-outside-refs-heads-keeps-its-full-name
  ;; `git push origin HEAD:refs/for/main` and tag pushes both reach here; only
  ;; the refs/heads prefix is stripped, so nothing else is silently mangled.
  (let [d (tmp)
        g (clone! d "g")
        f (log! d [[1000 g "s" "refs/tags/v1" (sha \a)]])]
    (is (= "refs/tags/v1" (:branch (first (attempts/attempts-since f 0)))))))

;; ------------------------------------------------------------ record kinds

(deftest clone-records-and-attempt-records-share-the-file
  (let [d (tmp)
        g1 (clone! d "g1") g2 (clone! d "g2")
        f (log! d [[1000 g1 "s"]
                   [1000 g1 "s" "refs/heads/a" (sha \a)]
                   [2000 g2 "-"]])]
    (is (= [g2 g1] (attempts/clones-since f 0))
        "clones come from either shape, newest first, distinct")
    (is (= [(sha \a)] (map :sha (attempts/attempts-since f 0)))
        "attempts come only from the shape that names a ref")))

(deftest a-three-field-line-from-an-older-hook-still-parses
  ;; The hook is a POSIX sh script installed per clone; a clone this session
  ;; has never started in may still hold a version that wrote fewer fields.
  (let [d (tmp)
        g (clone! d "g")
        f (log! d [[1000 g]])]
    (is (= [g] (attempts/clones-since f 0)))
    (is (empty? (attempts/attempts-since f 0)))))

(deftest malformed-lines-are-skipped-not-fatal
  ;; The writer must never fail a push, so it cannot validate what it appends.
  (let [d (tmp)
        g (clone! d "g")
        f (str (fs/path d "pushes.log"))]
    (spit f (str "not-a-line\n\n1000\n"
                 (format "1000\t%s\ts\n" g)
                 "abc\t/nope\ts\n"
                 "2000\t\ts\n"
                 (format "3000\t%s\ts\trefs/heads/a\tnot-a-sha\n" g)
                 (format "4000\t%s\ts\trefs/heads/b\t%s\n" g (sha \c))))
    (is (= [g] (attempts/clones-since f 0)))
    (is (= [(sha \c)] (map :sha (attempts/attempts-since f 0)))
        "a line whose sha is not 40 hex is a clone record, never an attempt")))

(deftest entries-older-than-the-window-are-excluded
  (let [d (tmp)
        g (clone! d "g")
        f (log! d [[1000 g "s" "refs/heads/a" (sha \a)]
                   [5000 g "s" "refs/heads/b" (sha \b)]])]
    (is (= 2 (count (attempts/attempts-since f 0))))
    (is (= [(sha \b)] (map :sha (attempts/attempts-since f 2000000))))
    (is (empty? (attempts/attempts-since f 9000000)))))

(deftest a-clone-that-no-longer-exists-is-dropped
  (let [d (tmp)
        g (clone! d "g")
        f (log! d [[1000 g "s"] [2000 (str (fs/path d "deleted")) "s"]])]
    (is (= [g] (attempts/clones-since f 0)))))

(deftest a-log-that-does-not-exist-is-empty-not-an-error
  (let [absent (str (fs/path (tmp) "absent.log"))]
    (is (empty? (attempts/clones-since absent 0)))
    (is (empty? (attempts/attempts-since absent 0)))))

;; ---------------------------------------------------- append-only, by design

(deftest a-concurrent-append-is-never-lost
  (testing "the log has NO truncating writer, and this is why. `prune!` used to
            rewrite it in place with `spit` under a flock the POSIX pre-push
            hook's bare `>>` never takes — measured, 156 of 400 concurrent
            appends destroyed, each one a push that can no longer match a
            reflog entry and is silently never reviewed. An atomic rename would
            not have fixed it either: an append between the read and the rename
            lands on the inode the rename replaces"
    (let [d (tmp)
          g (clone! d "g")
          f (str (fs/path d "pushes.log"))
          now (quot (System/currentTimeMillis) 1000)
          n 400]
      (spit f (apply str (for [i (range 2000)]
                           (format "%d\t%s\t-\n" (- now 999999) g))))
      (let [appender (p/process
                      ["sh" "-c" (format "i=0; while [ $i -lt %d ]; do printf '%%s\\t%s\\tsess\\n' %d >> %s; i=$((i+1)); done"
                                         n g now f)])]
        (Thread/sleep 5)
        (dotimes [_ 20] (attempts/clones-since f 0))
        @appender)
      (is (= n (count (filter #(str/includes? % "\tsess")
                              (str/split-lines (slurp f)))))
          "every appended record must survive a concurrent reader"))))

(deftest a-huge-log-is-read-from-the-tail-not-whole
  (testing "removing the truncating writer means the file grows forever, so the
            READER bounds its own work instead"
    (let [d (tmp)
          g (clone! d "g")
          f (str (fs/path d "pushes.log"))
          now (quot (System/currentTimeMillis) 1000)]
      (with-open [w (clojure.java.io/writer f)]
        (dotimes [_ 40000] (.write w (format "%d\t%s\t-\n" (- now 99999) g)))
        (.write w (format "%d\t%s\tsess\trefs/heads/x\t%s\n" now g (sha \a))))
      (is (> (fs/size f) 1000000) "the fixture must actually be large")
      (let [r (attempts/attempts-since f (* 1000 (- now 10)))]
        (is (= 1 (count r)))
        (is (= (sha \a) (:sha (first r))))))))

