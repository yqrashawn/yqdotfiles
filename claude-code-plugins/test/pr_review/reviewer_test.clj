(ns pr-review.reviewer-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [pr-review.reviewer :as reviewer]))

(def ^:private good-output
  (str "VERDICT: NOT MERGEABLE — retry loop drops the last attempt\n"
       "\n"
       "  [correctness/blocking]  1 findings\n"
       "  [correctness/followup]  2 findings\n"
       "  [coverage]              none\n"
       "  [docs-accuracy]         none\n"
       "  [style]                 1 findings\n"
       "\n"
       "1. [correctness/blocking] src/retry.clj:42 — off-by-one drops attempt N\n"
       "2. [correctness/followup] src/retry.clj:88 — jitter unseeded\n"
       "3. [correctness/followup] src/pool.clj:12 — leak on 5xx\n"
       "4. [style] src/pool.clj:3 — naming\n"))

(deftest argv-pins-the-reviewer-to-read-only-tools
  (let [argv (reviewer/claude-argv)]
    (is (= "claude" (first argv)))
    (is (some #{"-p"} argv))
    (testing "no shell, no writes — this is what makes B provably non-mutating
              and immune to rtk's diff truncation"
      (let [i (.indexOf argv "--allowedTools")]
        (is (nat-int? i))
        (is (= "Read,Grep,Glob" (nth argv (inc i))))))
    (is (some #{"opus"} argv) "review quality is the point; do not downgrade the model")))

(deftest run-passes-the-prompt-and-cwd-to-the-spawner
  (let [seen (atom nil)
        spawn (fn [argv prompt dir] (reset! seen {:argv argv :prompt prompt :dir dir})
                {:exit 0 :out "VERDICT: MERGEABLE — 0 follow-ups to file" :err ""})
        res (reviewer/run! "PROMPT" "/repo" {:spawn-fn spawn})]
    (is (= 0 (:exit res)))
    (is (= "PROMPT" (:prompt @seen)))
    (is (= "/repo" (:dir @seen)) "the reviewer must run in the repo it is reviewing")))

(deftest parse-extracts-verdict-and-counts
  (let [p (reviewer/parse-output good-output)]
    (is (= "NOT MERGEABLE" (:verdict p)))
    (is (= 1 (get (:counts p) "correctness/blocking")))
    (is (= 2 (get (:counts p) "correctness/followup")))
    (is (= 0 (get (:counts p) "coverage")) "\"none\" must parse as 0, not nil")
    (is (= 1 (get (:counts p) "style")))))

(deftest parse-extracts-fingerprints-from-findings
  (let [p (reviewer/parse-output good-output)]
    (is (= ["src/retry.clj:42:correctness/blocking"
            "src/retry.clj:88:correctness/followup"
            "src/pool.clj:12:correctness/followup"
            "src/pool.clj:3:style"]
           (:fingerprints p))
        "fingerprints are file:line:category so the one-re-raise rule can key on them")))

(deftest mergeable-requires-no-blocking-and-no-coverage
  (is (true? (reviewer/mergeable?
              {:verdict "MERGEABLE" :counts {"correctness/blocking" 0 "coverage" 0}})))
  (is (false? (reviewer/mergeable?
               {:verdict "MERGEABLE" :counts {"correctness/blocking" 1 "coverage" 0}}))
      "the verdict line is the reviewer's claim; the counts are the evidence")
  (is (false? (reviewer/mergeable?
               {:verdict "MERGEABLE" :counts {"correctness/blocking" 0 "coverage" 2}}))))

(deftest output-with-no-verdict-is-MALFORMED-not-dropped
  (let [p (reviewer/parse-output "I could not find the diff file.")]
    (is (= "MALFORMED" (:verdict p)))
    (is (= "I could not find the diff file." (:body p))
        "a reviewer that fails must surface its own words, or the pass vanishes silently")
    (is (= [] (:fingerprints p)))))

(deftest body-is-preserved-verbatim
  (is (= good-output (:body (reviewer/parse-output good-output)))))

(deftest echoed-template-is-MALFORMED-not-a-clean-pass
  (let [echoed (->> (str/split-lines good-output)
                     (map #(str "    " %))
                     (str/join "\n"))
        p (reviewer/parse-output echoed)]
    (is (= "MALFORMED" (:verdict p))
        "an indented, echoed format example must never parse as a real verdict")
    (is (false? (reviewer/mergeable? p)))))

(def ^:private space-and-colon-output
  (str "VERDICT: NOT MERGEABLE — paths need care\n"
       "\n"
       "  [correctness/blocking]  none\n"
       "  [correctness/followup]  none\n"
       "  [coverage]              none\n"
       "  [docs-accuracy]         1 findings\n"
       "  [style]                 1 findings\n"
       "\n"
       "1. [docs-accuracy] docs/My Notes.md:12 — needs a heading\n"
       "2. [style] src/pool:v2/file.clj:34 — naming\n"))

(deftest parse-fingerprints-handles-paths-with-spaces-and-colons
  (let [p (reviewer/parse-output space-and-colon-output)]
    (is (= ["docs/My Notes.md:12:docs-accuracy"
            "src/pool:v2/file.clj:34:style"]
           (:fingerprints p))
        "a path with a space or an internal colon must still produce a whole
         file:line:category fingerprint — otherwise the one-re-raise rule can
         never match it and it is re-reported on every pass, forever")))

(deftest run-never-throws-even-if-the-spawner-does
  (let [spawn (fn [_ _ _] (throw (ex-info "boom" {})))
        res (reviewer/run! "PROMPT" "/repo" {:spawn-fn spawn})]
    (is (not (zero? (:exit res)))
        "a spawn failure must surface as a result, never propagate as an exception")
    (is (= "boom" (:err res)))))
