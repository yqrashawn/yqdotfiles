(ns pr-review.prompt-test
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [pr-review.prompt :as prompt]))

(defn- tmp-repo []
  (let [d (str (fs/create-temp-dir {:prefix "pr-review-prompt"}))]
    (fs/create-dirs (str d "/.git"))
    d))

(def ^:private ctx
  {:diff-path "/r/.git/pr-review-context/abc.diff"
   :changed-files ["src/a.clj" "test/b_test.clj"]
   :base "basesha" :sha "abc" :diff-bytes 1234})

(defn- base-args [repo]
  {:core "CORE_TEXT" :repo-root repo :ctx ctx :pr 370 :pass 1
   :draft? false :prior-fingerprints []})

(deftest core-is-always-included
  (is (str/includes? (prompt/build (base-args (tmp-repo))) "CORE_TEXT")))

(deftest prompt-names-the-diff-file-and-repo-root
  (let [r (tmp-repo)
        out (prompt/build (base-args r))]
    (is (str/includes? out (:diff-path ctx)))
    (is (str/includes? out r))
    (is (str/includes? out "src/a.clj"))
    (is (str/includes? out "PR NUMBER: 370"))
    (is (str/includes? out "BASE: basesha"))
    (is (str/includes? out "HEAD: abc"))))

(deftest first-pass-is-labelled-FIRST
  (let [out (prompt/build (base-args (tmp-repo)))]
    (is (str/includes? out "PASS: FIRST"))
    (is (not (str/includes? out "RE-REVIEW")))))

(deftest later-passes-are-labelled-RE-REVIEW
  (let [out (prompt/build (assoc (base-args (tmp-repo)) :pass 3))]
    (is (str/includes? out "PASS: RE-REVIEW"))
    (is (str/includes? out "pass 3")
        "the reviewer must know which pass it is to apply the severity asymmetry")))

(deftest overlay-is-included-when-present
  (let [r (tmp-repo)]
    (fs/create-dirs (str r "/.claude"))
    (spit (prompt/overlay-path r) "OVERLAY_TEXT")
    (is (str/includes? (prompt/build (base-args r)) "OVERLAY_TEXT"))))

(deftest missing-overlay-degrades-silently
  (let [out (prompt/build (base-args (tmp-repo)))]
    (is (not (str/includes? out "OVERLAY")))
    (is (str/includes? out "CORE_TEXT")
        "a repo with no .claude/pr-review.md must still get a working review")))

(deftest hint-is-included-and-consumed
  (let [r (tmp-repo)]
    (spit (prompt/hint-path r) "watch the retry path")
    (let [out (prompt/build (base-args r))]
      (is (str/includes? out "watch the retry path")))
    (is (not (fs/exists? (prompt/hint-path r)))
        "a hint is for one review; leaving it would silently apply to every later pass")))

(deftest draft-status-is-stated
  (is (str/includes? (prompt/build (assoc (base-args (tmp-repo)) :draft? true))
                     "draft")))

(deftest twice-raised-fingerprints-are-listed-as-do-not-re-raise
  (let [out (prompt/build (assoc (base-args (tmp-repo)) :pass 2
                                 :prior-fingerprints ["src/a.clj:12:correctness/followup"]))]
    (is (str/includes? out "src/a.clj:12:correctness/followup"))
    (is (str/includes? out "do not re-raise"))))

(deftest empty-diff-is-called-out
  (let [out (prompt/build (assoc-in (base-args (tmp-repo)) [:ctx :diff-bytes] 0))]
    (is (str/includes? out "empty")
        "an empty diff must be stated, or the reviewer invents findings")))
