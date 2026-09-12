(ns pr-review.reviewer-test
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as str]
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

(defn- counted
  "A minimal well-formed reply with `n` blocking findings claimed in the count
   block, and `verdict` on the verdict line."
  [verdict n findings]
  (str "VERDICT: " verdict " — whatever\n\n"
       "  [correctness/blocking]  " n "\n"
       "  [correctness/followup]  none\n"
       "  [coverage]              none\n"
       "  [docs-accuracy]         none\n"
       "  [style]                 none\n\n"
       findings))

;; ---------------------------------------------------------------- sandbox

(deftest argv-sandboxes-the-reviewer-with-a-deny-list
  (let [argv (reviewer/claude-argv)]
    (is (= "claude" (first argv)))
    (is (some #{"-p"} argv))
    (is (some #{"opus"} argv) "review quality is the point; do not downgrade the model")
    (testing "--disallowedTools is the only mechanism that actually removes a
              tool. --allowedTools is a pre-approval allowlist, and with
              permissions.defaultMode bypassPermissions every tool is
              auto-approved regardless — measured: that invocation had Bash
              and wrote a file outside every repo"
      (let [i (.indexOf argv "--disallowedTools")]
        (is (nat-int? i))
        (let [denied (set (str/split (nth argv (inc i)) #","))]
          (testing "a writer — but NOT a shell. Bash is granted on purpose, so
                    the reviewer can inspect the change the way it wants
                    rather than only through a precomputed diff. That reverses
                    R7's original wording and costs real containment: with
                    Bash, `printf x > file` works, so denying Write and Edit
                    is no longer a write barrier. What bounds the damage is
                    the throwaway worktree the review runs in"
            (is (every? denied ["Write" "Edit" "NotebookEdit"]))
            (is (not (denied "Bash")) "Bash is deliberately granted"))
          (testing "the harms that are known and reachable are still closed,
                    as command shapes. Measured: a denied `rm` came back
                    \"Denied by user\" while `git log` and a `printf >` in the
                    same session both ran, so specifier denies are enforced
                    even under bypassPermissions"
            (is (every? denied ["Bash(rm:*)" "Bash(sudo:*)"])
                "irreversible or privileged, and never needed to read a change")
            (is (every? denied ["Bash(git push:*)" "Bash(git commit:*)"])
                "a push from the reviewer would carry the PARENT session's
                 CLAUDE_CODE_SESSION_ID, so the pre-push hook would record it
                 as an agent push and the loop would review the reviewer")
          (testing "gh READ is allowed and gh WRITE is not. The prompt points
                    the reviewer at `gh pr view` to read the PR it is
                    reviewing, so a blanket Bash(gh pr:*) would break that —
                    but the mutating subcommands would let it act ON the PR,
                    and commenting, merging and closing are agent A's job.
                    Before this, nothing gh-shaped was denied at all while
                    README and SKILL.md both told the author it was."
            (is (every? denied ["Bash(gh pr merge:*)" "Bash(gh pr close:*)"
                                "Bash(gh pr comment:*)" "Bash(gh pr review:*)"
                                "Bash(gh pr edit:*)" "Bash(gh pr reopen:*)"
                                "Bash(gh pr ready:*)"]))
            (is (denied "Bash(gh api:*)")
                "gh api reaches anything the token reaches, including other
                 repositories")
            (is (not (some #(str/includes? % "gh pr view") denied))
                "reading the PR is what the prompt asks for"))
            (is (every? denied ["Bash(curl:*)" "Bash(wget:*)" "Bash(nc:*)"])
                "WebFetch and WebSearch are denied for being a route off this
                 machine; leaving these open reopens it"))
          (testing "another agent to do it instead"
            (is (every? denied ["Agent" "Task" "SendMessage"])))
          (testing "a route off this machine — Artifact publishes to the web"
            (is (every? denied ["WebFetch" "WebSearch" "Artifact"
                                "PushNotification" "RemoteTrigger"
                                "ShareOnboardingGuide"])))
          (testing "a way to make work happen later"
            (is (every? denied ["CronCreate" "ScheduleWakeup" "Workflow" "Skill"])))
          (testing "a way to reach a tool that is not on this list at all"
            (is (denied "ToolSearch")))
          (testing "Read, Grep and Glob are the whole review"
            (is (not-any? denied ["Read" "Grep" "Glob"]))))))
    (testing "no MCP server the user happens to have configured: several write
              files and reach the network, and their names are
              per-installation so no deny list can enumerate them"
      (is (some #{"--strict-mcp-config"} argv)))
    (testing "--allowedTools is kept for intent; it restricts nothing"
      (let [i (.indexOf argv "--allowedTools")]
        (is (nat-int? i))
        (is (= "Read,Grep,Glob,Bash" (nth argv (inc i))))))))

(deftest run-passes-the-prompt-and-cwd-to-the-spawner
  (let [seen (atom nil)
        spawn (fn [argv prompt dir err-file]
                (reset! seen {:argv argv :prompt prompt :dir dir :err-file err-file})
                {:exit 0 :out "VERDICT: MERGEABLE — 0 follow-ups to file" :err ""})
        res (reviewer/run! "PROMPT" "/repo" {:spawn-fn spawn})]
    (is (= 0 (:exit res)))
    (is (= "PROMPT" (:prompt @seen)))
    (is (= "/repo" (:dir @seen)) "the reviewer must run in the repo it is reviewing")
    (is (nil? (:err-file @seen)) "no err-file unless the caller asks for one")))

(deftest default-spawn-streams-stderr-to-the-file-as-it-runs
  (testing "the point is the case where nothing is RETURNED: two reviews were
            killed mid-run after 5m48s and 7m33s and the loop had discarded
            everything the reviewer said. Streaming means a SIGKILL still
            leaves whatever reached the file, and parents are created because
            the path is under a git dir that may not have it yet"
    (let [f (str (fs/path (fs/create-temp-dir {:prefix "prl-err"}) "deep" "r.stderr"))
          spawn @#'reviewer/default-spawn
          res (spawn ["sh" "-c" "echo diagnosis >&2; echo reply; exit 7"] "" "." f)]
      (is (= 7 (:exit res)))
      (is (= "reply" (str/trim (:out res))))
      (is (= "diagnosis" (str/trim (:err res)))
          ":err must still behave as before, read back from the file")
      (is (= "diagnosis" (str/trim (slurp f)))
          "and the file must hold it, which is what survives a kill"))))

(deftest the-err-file-path-reaches-the-spawner
  (let [seen (atom nil)
        spawn (fn [_ _ _ err-file] (reset! seen err-file) {:exit 0 :out "" :err ""})]
    (reviewer/run! "P" "/repo" {:spawn-fn spawn :err-file "/tmp/x.stderr"})
    (is (= "/tmp/x.stderr" @seen)
        "or a killed reviewer leaves nothing behind to explain itself")))

(deftest run-never-throws-even-if-the-spawner-does
  (let [spawn (fn [_ _ _ _] (throw (ex-info "boom" {})))
        res (reviewer/run! "PROMPT" "/repo" {:spawn-fn spawn})]
    (is (not (zero? (:exit res)))
        "a spawn failure must surface as a result, never propagate as an exception")
    (is (= "boom" (:err res)))))

;; ---------------------------------------------------------------- parsing

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

(deftest output-with-no-verdict-is-MALFORMED-not-dropped
  (let [p (reviewer/parse-output "I could not find the diff file.")]
    (is (= "MALFORMED" (:verdict p)))
    (is (= "I could not find the diff file." (:body p))
        "a reviewer that fails must surface its own words, or the pass vanishes silently")
    (is (= [] (:fingerprints p)))))

(deftest body-is-preserved-verbatim
  (is (= good-output (:body (reviewer/parse-output good-output)))
      "emphasis is stripped for parsing only; agent A must read exactly what
       the reviewer wrote"))

(deftest echoed-template-is-MALFORMED-not-a-clean-pass
  (let [echoed (->> (str/split-lines good-output)
                    (map #(str "    " %))
                    (str/join "\n"))
        p (reviewer/parse-output echoed)]
    (is (= "MALFORMED" (:verdict p))
        "an indented, echoed format example must never parse as a real verdict")
    (is (false? (reviewer/mergeable? p)))))

(deftest the-last-column-zero-verdict-wins
  (let [out (str "VERDICT: MERGEABLE — this is me restating the required format\n"
                 "\n"
                 "Now the actual review.\n"
                 "\n"
                 (counted "NOT MERGEABLE" 1
                          "1. [correctness/blocking] src/a.clj:7 — boom\n"))]
    (is (= "NOT MERGEABLE" (:verdict (reviewer/parse-output out)))
        "re-find returns the FIRST match, so a reviewer that restated the
         format unindented before reviewing had that restatement parsed as its
         answer — a third false-clean path")))

(deftest a-bolded-verdict-at-column-zero-still-counts
  (is (= "MERGEABLE"
         (:verdict (reviewer/parse-output "**VERDICT: MERGEABLE — nothing to fix**\n")))
      "emphasis is stripped before the column-0 anchor is applied, so bold
       markup does not turn a real verdict into MALFORMED"))

(deftest counts-survive-emphasis-and-capitals
  (testing "mergeable? reads the count block as the evidence that overrides
            the verdict line, so a count block that fails to parse is a false
            clean — the same defect class as an unparseable finding line"
    (let [out (str "VERDICT: MERGEABLE — looks fine\n\n"
                   "  **[Correctness/Blocking]**  2\n"
                   "  `[correctness/followup]`    None\n"
                   "  [coverage]                  none\n"
                   "  [docs-accuracy]             none\n"
                   "  [style]                     none\n")
          p (reviewer/parse-output out)]
      (is (= 2 (get (:counts p) "correctness/blocking")))
      (is (= 0 (get (:counts p) "correctness/followup")))
      (is (false? (reviewer/mergeable? p))))))

(deftest fingerprints-are-parsed-from-every-realistic-line-shape
  (testing "each of these used to yield a counted finding with an EMPTY
            fingerprint, so the one-re-raise rule could never fire for it and
            it was re-reported on every pass straight into the 10-pass cap.
            The invariant is `[category] path:line`, not the canonical
            `N. [category] path:line — text`"
    (doseq [[label line expected]
            [["canonical"
              "1. [correctness/blocking] src/retry.clj:42 — off-by-one"
              "src/retry.clj:42:correctness/blocking"]
             ["line range collapses to its first line"
              "2. [correctness/blocking] src/retry.clj:42-45 — off-by-one"
              "src/retry.clj:42:correctness/blocking"]
             ["colon instead of the em-dash"
              "3. [correctness/blocking] src/retry.clj:42: off-by-one"
              "src/retry.clj:42:correctness/blocking"]
             ["backticked path — the likeliest LLM shape"
              "4. [correctness/blocking] `src/retry.clj:42` — off-by-one"
              "src/retry.clj:42:correctness/blocking"]
             ["bolded path"
              "5. [correctness/blocking] **src/retry.clj:42** — off-by-one"
              "src/retry.clj:42:correctness/blocking"]
             ["L-prefixed line number"
              "6. [correctness/blocking] src/retry.clj:L42 — off-by-one"
              "src/retry.clj:42:correctness/blocking"]
             ["capitalised category"
              "7. [Correctness/Blocking] src/retry.clj:42 — off-by-one"
              "src/retry.clj:42:correctness/blocking"]
             ["dash bullet instead of N."
              "- [correctness/blocking] src/retry.clj:42 — off-by-one"
              "src/retry.clj:42:correctness/blocking"]
             ["asterisk bullet"
              "* [correctness/blocking] src/retry.clj:42 — off-by-one"
              "src/retry.clj:42:correctness/blocking"]
             ["paren bullet"
              "8) [correctness/blocking] src/retry.clj:42 — off-by-one"
              "src/retry.clj:42:correctness/blocking"]
             ["no bullet at all"
              "[correctness/blocking] src/retry.clj:42 — off-by-one"
              "src/retry.clj:42:correctness/blocking"]
             ["path containing a space"
              "9. [docs-accuracy] docs/My Notes.md:12 — needs a heading"
              "docs/My Notes.md:12:docs-accuracy"]
             ["path containing a colon"
              "10. [style] src/pool:v2/file.clj:34 — naming"
              "src/pool:v2/file.clj:34:style"]
             ["everything at once"
              "- [Coverage] `src/pool:v2/my file.clj:L34-40`: certifies nothing"
              "src/pool:v2/my file.clj:34:coverage"]]]
      (let [p (reviewer/parse-output (counted "NOT MERGEABLE" 1 (str line "\n")))]
        (is (= [expected] (:fingerprints p)) label)))))

(deftest the-count-block-is-not-mistaken-for-a-finding
  (is (= [] (:fingerprints (reviewer/parse-output
                            (counted "MERGEABLE" "none" ""))))
      "the count block lines carry a category in brackets but no path:line;
       reading one as a finding would invent a fingerprint out of nothing"))

;; ------------------------------------------------- verdict vs the evidence

(deftest mergeable-requires-the-verdict-line-and-a-zero-blocking-count
  (is (true? (reviewer/mergeable?
              {:verdict "MERGEABLE" :counts {"correctness/blocking" 0 "coverage" 0}})))
  (is (false? (reviewer/mergeable?
               {:verdict "MERGEABLE" :counts {"correctness/blocking" 1 "coverage" 0}}))
      "the verdict line is the reviewer's claim; the counts are the evidence")
  (is (false? (reviewer/mergeable?
               {:verdict "NOT MERGEABLE" :counts {"correctness/blocking" 0}})))
  (testing "no coverage clause: the spec and the core prompt both define
            MERGEABLE as no blocking finding and no coverage finding \"in which
            a test certifies a safety property it does not check\" — a
            judgement about one finding's content that a bare count cannot
            express. Demanding zero coverage findings outright would turn
            every benign coverage nit into a false NOT-clean"
    (is (true? (reviewer/mergeable?
                {:verdict "MERGEABLE" :counts {"correctness/blocking" 0 "coverage" 2}})))))

(deftest reconcile-overrides-a-verdict-its-own-counts-contradict
  (let [p (reviewer/reconcile
           (reviewer/parse-output
            (counted "MERGEABLE" 1 "1. [correctness/blocking] src/a.clj:7 — boom\n")))]
    (is (= "NOT MERGEABLE" (:verdict p))
        "mergeable? had zero production call sites, so a count block that
         contradicted the verdict line produced a MERGEABLE headline for agent
         A and a self-contradictory ledger row (verdict MERGEABLE, blocking 1)")
    (is (str/includes? (:body p) "count block")
        "the contradiction must be stated, not silently rewritten")))

(deftest reconcile-leaves-a-consistent-verdict-alone
  (doseq [out [(counted "MERGEABLE" "none" "")
               (counted "NOT MERGEABLE" 2 "1. [correctness/blocking] a:1 — x\n")]]
    (let [parsed (reviewer/parse-output out)]
      (is (= parsed (reviewer/reconcile parsed))
          "reconciliation must be a no-op when claim and evidence agree"))))

(deftest reconcile-passes-MALFORMED-through
  (let [p (reviewer/parse-output "the diff file was empty")]
    (is (= p (reviewer/reconcile p))
        "there is no verdict to reconcile, and an unparsed review's counts are
         all zero by construction — rewriting it to NOT MERGEABLE would claim
         a review happened")))

(deftest counted-findings-with-no-fingerprints-are-surfaced
  (let [p (reviewer/parse-output
           (counted "NOT MERGEABLE" 2
                    "1. [correctness/blocking] the retry loop is wrong\n"))
        w (reviewer/parse-warnings p)]
    (is (= 1 (count w)))
    (is (str/includes? (first w) "2 finding"))
    (is (str/includes? (first w) "one-re-raise")
        "non-zero counts with no parseable path:line is a parse failure:
         nothing carries an identity, so every finding is re-reported until
         the cap. Nothing used to notice")))

(deftest a-genuinely-clean-pass-warns-about-nothing
  (is (= [] (reviewer/parse-warnings
             (reviewer/parse-output (counted "MERGEABLE" "none" ""))))))

;; ------------------------------------------------ prompt/parser round trip

(deftest the-shipped-core-prompt-is-not-mistaken-for-a-review
  (testing "the prompt and the parser are the same contract in two files that
            drift silently — which is how the first-match verdict bug and the
            fingerprint-shape gaps both shipped. Feed the SHIPPED
            review_core.md through the parser: every VERDICT line in it is an
            indented example, so the whole document must read as MALFORMED"
    (let [core (slurp (io/resource "review_core.md"))
          p (reviewer/parse-output core)]
      (is (= "MALFORMED" (:verdict p)))
      (is (false? (reviewer/mergeable? p)))
      (is (= [] (:fingerprints p))))))

(deftest the-shipped-core-prompts-own-example-finding-lines-parse
  (testing "the other half of the same contract: the example finding lines the
            prompt tells the reviewer to copy must produce fingerprints when
            they appear under a real verdict"
    (let [core (slurp (io/resource "review_core.md"))
          examples (->> (str/split-lines core)
                        (map str/trim)
                        (filter #(re-find #"^\d+\.\s*\[" %)))]
      (is (seq examples) "fixture precondition: review_core.md shows examples")
      (doseq [line examples]
        (is (= 1 (count (:fingerprints
                         (reviewer/parse-output
                          (counted "NOT MERGEABLE" 1 (str line "\n"))))))
            (str "the prompt's own example line must parse: " line))))))

(deftest counts-come-from-the-winning-verdicts-own-block
  (testing "N2. `parse-verdict` takes the LAST verdict at column 0, because
            reviewers restate the format or recap the previous pass before
            answering. `parse-counts` took the FIRST count block, so the two
            could describe different blocks — and a clean pass was reconciled
            back to NOT MERGEABLE off a recap's counts, so the loop ran
            another round on a PR that was finished"
    (let [reply (str "Recap of the previous pass:\n"
                     "VERDICT: NOT MERGEABLE — the old blocking finding\n"
                     "  [correctness/blocking]  1 findings\n"
                     "  [correctness/followup]  0\n"
                     "  [coverage]              0\n"
                     "  [docs-accuracy]         0\n"
                     "  [style]                 0\n"
                     "\nThat is now fixed. My verdict this pass:\n\n"
                     "VERDICT: MERGEABLE — 1 follow-up to file\n"
                     "  [correctness/blocking]  none\n"
                     "  [correctness/followup]  1 findings\n"
                     "  [coverage]              0\n"
                     "  [docs-accuracy]         0\n"
                     "  [style]                 0\n"
                     "\n1. [correctness/followup] src/a.clj:9 — bounded\n")
          p (reviewer/parse-output reply)]
      (is (= "MERGEABLE" (:verdict p)))
      (is (= 0 (get (:counts p) "correctness/blocking"))
          "the recap's blocking count must not be read as this pass's")
      (is (= 1 (get (:counts p) "correctness/followup")))
      (is (true? (reviewer/mergeable? p)))
      (is (= "MERGEABLE" (:verdict (reviewer/reconcile p)))
          "a finished PR must be allowed to finish"))))

(deftest a-blocking-count-in-the-real-block-still-overrides-a-clean-claim
  (testing "the guard the counts exist for must survive the fix: the verdict
            line is the reviewer's claim, its own counts are the evidence"
    (let [reply (str "VERDICT: MERGEABLE — nothing to fix\n"
                     "  [correctness/blocking]  1 findings\n"
                     "  [correctness/followup]  0\n"
                     "  [coverage]              0\n"
                     "  [docs-accuracy]         0\n"
                     "  [style]                 0\n"
                     "\n1. [correctness/blocking] src/a.clj:1 — real defect\n")
          p (reviewer/parse-output reply)]
      (is (= 1 (get (:counts p) "correctness/blocking")))
      (is (false? (reviewer/mergeable? p)))
      (is (= "NOT MERGEABLE" (:verdict (reviewer/reconcile p)))))))

(deftest findings-written-before-the-verdict-are-still-collected
  (testing "counts are read after the winning verdict; fingerprints are not,
            deliberately — a reviewer that lists findings then concludes would
            otherwise lose all of them, which is worse than a wrong count"
    (let [reply (str "1. [correctness/blocking] src/a.clj:42 — nil deref\n"
                     "2. [coverage] test/a_test.clj:7 — cannot fail\n"
                     "\nVERDICT: NOT MERGEABLE — nil deref\n"
                     "  [correctness/blocking]  1 findings\n"
                     "  [correctness/followup]  0\n"
                     "  [coverage]              1 findings\n"
                     "  [docs-accuracy]         0\n"
                     "  [style]                 0\n")
          p (reviewer/parse-output reply)]
      (is (= 2 (count (:fingerprints p))))
      (is (= 1 (get (:counts p) "correctness/blocking"))))))

(deftest the-reviewer-marks-its-own-environment
  (testing "so the trigger can recognise its own grandchildren. Without it,
            every Bash call the reviewer makes fires this plugin's PostToolUse
            hook — measured, a review-trigger process spawning inside the
            reviewer — and the abandoned path can start a review inside a
            review"
    (let [f (str (fs/path (fs/create-temp-dir {:prefix "prl-env"}) "e.stderr"))
          spawn @#'reviewer/default-spawn
          res (spawn ["sh" "-c" (str "printf %s \"$" reviewer/reviewer-env-var "\"")]
                     "" "." f)]
      (is (= "1" (:out res))
          (str reviewer/reviewer-env-var " must reach the reviewer's environment")))
    (testing "on the no-err-file path too"
      (let [spawn @#'reviewer/default-spawn
            res (spawn ["sh" "-c" (str "printf %s \"$" reviewer/reviewer-env-var "\"")]
                       "" "." nil)]
        (is (= "1" (:out res)))))))


;; ------------------------------------------------------------- credentials

(deftest the-token-is-read-and-trimmed
  (testing "the file ends with a newline — 109 bytes, 108 characters of token —
            and a token carrying one is not the token"
    (let [f (str (fs/path (fs/create-temp-dir {:prefix "prl-tok"}) "t"))]
      (spit f "sk-fake-token-value\n")
      (is (= "sk-fake-token-value" (reviewer/oauth-token f))))))

(deftest no-usable-token-is-nil-never-an-error
  (testing "losing a review is worse than running it as whoever the parent is,
            so nothing here may be the thing that stops a review starting"
    (let [d (str (fs/create-temp-dir {:prefix "prl-tok"}))]
      (is (nil? (reviewer/oauth-token (str (fs/path d "absent")))) "missing")
      (let [empty-f (str (fs/path d "empty"))]
        (spit empty-f "")
        (is (nil? (reviewer/oauth-token empty-f)) "empty"))
      (let [ws (str (fs/path d "ws"))]
        (spit ws "\n  \n")
        (is (nil? (reviewer/oauth-token ws)) "whitespace only"))
      (is (nil? (reviewer/oauth-token d)) "a directory, not a file")
      (is (nil? (reviewer/oauth-token nil)) "no path at all")
      ;; The only case the try/catch is for: the file passes
      ;; `fs/regular-file?` and then `slurp` throws. A Dropbox-synced
      ;; credential is exactly the kind of file that can be present and
      ;; momentarily unreadable.
      (let [locked (str (fs/path d "locked"))]
        (spit locked "sk-fake\n")
        (fs/set-posix-file-permissions locked "---------")
        (try
          (is (nil? (reviewer/oauth-token locked)) "present but unreadable")
          (finally (fs/set-posix-file-permissions locked "rw-------")))))))

(deftest the-reviewer-marker-survives-the-token
  (testing "PR_REVIEW_LOOP_REVIEWER is what stops a review triggering a review
            inside itself. Adding to :extra-env must never displace it"
    (let [f (str (fs/path (fs/create-temp-dir {:prefix "prl-tok"}) "t"))]
      (spit f "sk-fake\n")
      (with-redefs [reviewer/default-token-file f]
        (let [env (reviewer/spawn-env)]
          (is (= "1" (get env reviewer/reviewer-env-var)))
          (is (= "sk-fake" (get env "CLAUDE_CODE_OAUTH_TOKEN")))))
      (with-redefs [reviewer/default-token-file (str f ".absent")]
        (let [env (reviewer/spawn-env)]
          (is (= "1" (get env reviewer/reviewer-env-var)))
          (is (not (contains? env "CLAUDE_CODE_OAUTH_TOKEN"))
              "absent must mean ABSENT: :extra-env merges into the inherited
               environment, so an empty string would replace working
               credentials with a blank one"))))))

(deftest the-token-reaches-the-spawned-process
  (testing "the wiring, through the real spawner and a real child process"
    (let [f (str (fs/path (fs/create-temp-dir {:prefix "prl-tok"}) "t"))
          spawn @#'reviewer/default-spawn]
      (spit f "sk-fake-reaches-child\n")
      (with-redefs [reviewer/default-token-file f]
        (let [res (spawn ["sh" "-c" "echo \"$CLAUDE_CODE_OAUTH_TOKEN|$PR_REVIEW_LOOP_REVIEWER\""]
                         "" "." nil)]
          (is (= "sk-fake-reaches-child|1" (str/trim (:out res))))))
      (with-redefs [reviewer/default-token-file (str f ".absent")]
        (let [res (spawn ["sh" "-c" "echo \"[$CLAUDE_CODE_OAUTH_TOKEN]|$PR_REVIEW_LOOP_REVIEWER\""]
                         "" "." nil)]
          (is (= "[]|1" (str/trim (:out res)))
              "with no token the variable must be unset, not empty"))))))

(deftest the-token-never-appears-in-what-the-loop-records
  (testing "the previous version of this test could not fail: its stub child
            was `echo out; echo err >&2`, which had no reason to print the
            token, and there was no redaction anywhere in the tree. The child
            here PRINTS the credential — which is exactly what `env`, a stray
            `echo $CLAUDE_CODE_OAUTH_TOKEN` while debugging, or a tool dumping
            its environment on error would do, from a reviewer that has an
            unrestricted shell.

            All three sinks are checked because all three are republished:
            `:out` becomes the PR comment and the findings file, `:err` and the
            streamed file are read by the author."
    (let [d (str (fs/create-temp-dir {:prefix "prl-tok"}))
          errf (str (fs/path d "err"))
          tok "sk-ant-oat01-THIS-MUST-NOT-LEAK"
          res (reviewer/run!
               "P" "."
               {:token-fn (constantly tok)
                :err-file errf
                :spawn-fn (fn [_ _ _ ef]
                            (spit ef (str "debug: CLAUDE_CODE_OAUTH_TOKEN=" tok "\n"))
                            {:exit 0
                             :out (str "VERDICT: MERGEABLE — token is " tok)
                             :err (str "stderr had " tok)})})]
      (is (not (str/includes? (:out res) tok)) "the PR comment and findings file")
      (is (not (str/includes? (:err res) tok)) "the failure message")
      (is (not (str/includes? (slurp errf) tok))
          "and the streamed file, which is the copy that survives a kill")
      (is (str/includes? (:out res) "[redacted]")
          "redacted, not merely absent — or the test passes on an empty reply"))))

(deftest redaction-leaves-everything-else-alone
  (let [tok "sk-ant-oat01-abcdefgh"]
    (is (= "no secret here" (reviewer/redact "no secret here" tok)))
    (is (= "x" (reviewer/redact "x" nil)) "no token, nothing to do")
    (is (= "short" (reviewer/redact "short" "abc"))
        "a too-short token would match everywhere; refuse rather than mangle")))

(deftest a-quoted-prior-review-is-not-parsed-as-this-one
  (testing "the false clean this branch created. It posts every pass as a PR
            comment, tells the reviewer to read them, and tells a re-review to
            verify closure against the previous pass — so quoting the prior
            comment is the expected shape. A fence does not indent, and
            parse-verdict takes the LAST column-0 verdict, so the quote won.
            Measured before the fix: NOT MERGEABLE with 1 blocking parsed as
            MERGEABLE with 0, reconcile confirmed it, and agent A was told it
            could merge"
    (let [reply (str "VERDICT: NOT MERGEABLE — nil deref on every request\n\n"
                     "  [correctness/blocking]  1\n"
                     "  [correctness/followup]  0\n  [coverage]              0\n"
                     "  [docs-accuracy]         0\n  [style]                 0\n\n"
                     "1. [correctness/blocking] src/a.clj:42 — nil deref\n\n"
                     "For reference, pass 1 said:\n\n```\n"
                     "VERDICT: MERGEABLE — 2 follow-ups to file\n"
                     "  [correctness/blocking]  none\n"
                     "  [correctness/followup]  2\n  [coverage]              0\n"
                     "  [docs-accuracy]         0\n  [style]                 0\n"
                     "2. [correctness/followup] src/quoted.clj:9 — from the quote\n"
                     "```\n")
          p (reviewer/parse-output reply)]
      (is (= "NOT MERGEABLE" (:verdict p)))
      (is (= 1 (get (:counts p) "correctness/blocking")))
      (is (= "NOT MERGEABLE" (:verdict (reviewer/reconcile p))))
      (is (= ["src/a.clj:42:correctness/blocking"] (:fingerprints p))
          "a quoted review carries findings too; they are not this pass's"))))

(deftest an-unfenced-quote-of-a-prior-verdict-loses-to-the-pass-tag
  (testing "the fence was one spelling, not the class. review_core.md asks a
            re-review to verify closure against the previous pass, and nothing
            makes it fence what it quotes: plain, unindented, AFTER this pass's
            own verdict, the quote beat it under the last-at-column-0 rule.
            Reproduced: own NOT MERGEABLE with 1 blocking parsed as MERGEABLE
            with 0, reconcile confirmed it, and findings-message told the
            author `MERGEABLE means you may merge this PR now`.

            Position cannot fix it -- the working shape
            (`counts-come-from-the-winning-verdicts-own-block`) is this one's
            mirror image, recap first and own verdict last. The tag is the
            discriminator: minted per pass, so a quoted verdict carries an
            older one or none."
    (let [reply (str "VERDICT[a1b2c3d4]: NOT MERGEABLE — nil deref\n\n"
                     "  [correctness/blocking]  1\n"
                     "  [correctness/followup]  0\n  [coverage]              0\n"
                     "  [docs-accuracy]         0\n  [style]                 0\n\n"
                     "1. [correctness/blocking] src/a.clj:42 — nil deref\n\n"
                     "### Closure of the previous pass\n\n"
                     "Pass 2 ended with, verbatim:\n\n"
                     "VERDICT: MERGEABLE — 2 follow-ups to file\n"
                     "  [correctness/blocking]  none\n"
                     "  [correctness/followup]  2\n  [coverage]              0\n"
                     "  [docs-accuracy]         0\n  [style]                 0\n\n"
                     "which this pass overturns.\n")
          p (reviewer/parse-output reply "a1b2c3d4")]
      (is (= "NOT MERGEABLE" (:verdict p)))
      (is (= 1 (get (:counts p) "correctness/blocking"))
          "and the counts must come from the tagged verdict's block, not the quote")
      (is (false? (reviewer/mergeable? p)))
      (is (= "NOT MERGEABLE" (:verdict (reviewer/reconcile p)))))))

(deftest the-tag-beats-position-in-the-other-direction-too
  (testing "the mirror shape, which `counts-come-from-the-winning-verdicts-own-block`
            covers untagged: recap of the previous pass's NOT MERGEABLE first,
            this pass's MERGEABLE last. Measured in production. A tag that
            only ever pushed toward NOT MERGEABLE would re-open it — the loop
            would run another round on a PR that was finished."
    (let [reply (str "Recap of the previous pass:\n"
                     "VERDICT: NOT MERGEABLE — the old blocking finding\n"
                     "  [correctness/blocking]  1 findings\n"
                     "  [correctness/followup]  0\n  [coverage]              0\n"
                     "  [docs-accuracy]         0\n  [style]                 0\n\n"
                     "That is now fixed. My verdict this pass:\n\n"
                     "VERDICT[a1b2c3d4]: MERGEABLE — 1 follow-up to file\n"
                     "  [correctness/blocking]  none\n"
                     "  [correctness/followup]  1 findings\n  [coverage]              0\n"
                     "  [docs-accuracy]         0\n  [style]                 0\n")
          p (reviewer/parse-output reply "a1b2c3d4")]
      (is (= "MERGEABLE" (:verdict p)))
      (is (= 0 (get (:counts p) "correctness/blocking")))
      (is (true? (reviewer/mergeable? p)) "a finished PR must be allowed to finish"))))

(deftest a-reviewer-that-ignores-the-tag-is-still-parsed
  (testing "the tag is an override, not a requirement. A reply with no tagged
            verdict falls back to the last-at-column-0 rule every pass before
            the tag used — a reviewer that drops it must not become
            unreviewable, which would cost a whole pass for a formatting slip."
    (let [reply (str "VERDICT: NOT MERGEABLE — real\n\n"
                     "  [correctness/blocking]  1\n  [correctness/followup]  0\n"
                     "  [coverage]              0\n  [docs-accuracy]         0\n"
                     "  [style]                 0\n\n"
                     "1. [correctness/blocking] src/a.clj:42 — real\n")
          p (reviewer/parse-output reply "a1b2c3d4")]
      (is (= "NOT MERGEABLE" (:verdict p)))
      (is (= 1 (get (:counts p) "correctness/blocking"))))))

(deftest a-fenced-code-excerpt-inside-a-finding-keeps-the-finding
  ;; Blanking fenced regions must not eat the finding line that introduces one.
  (let [reply (str "VERDICT: NOT MERGEABLE — real\n\n"
                   "  [correctness/blocking]  1\n  [correctness/followup]  0\n"
                   "  [coverage]              0\n  [docs-accuracy]         0\n"
                   "  [style]                 0\n\n"
                   "1. [correctness/blocking] src/a.clj:42 — here is the code:\n"
                   "```clj\n(defn broken [] nil)\n```\n")
        p (reviewer/parse-output reply)]
    (is (= ["src/a.clj:42:correctness/blocking"] (:fingerprints p)))
    (is (= 1 (get (:counts p) "correctness/blocking")))))

(deftest fences-are-stripped-before-emphasis-not-after
  ;; strip-emphasis deletes every backtick, so an emphasis-first pipeline sees
  ;; a ``` line as "" and detects no fence at all. That ordering bug made the
  ;; first version of this fix inert.
  (let [reply (str "VERDICT: NOT MERGEABLE — real\n\n"
                   "  [correctness/blocking]  1\n  [correctness/followup]  0\n"
                   "  [coverage]              0\n  [docs-accuracy]         0\n"
                   "  [style]                 0\n\n"
                   "```\nVERDICT: MERGEABLE — quoted\n```\n")]
    (is (= "NOT MERGEABLE" (:verdict (reviewer/parse-output reply))))))

