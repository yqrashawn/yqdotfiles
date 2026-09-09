(ns pr-review.wrapper-test
  "Tests `hooks/rtk-rewrite-wrapper.sh`, the tracked PreToolUse wrapper that
   splices the push-directory recorder into a push or `gh pr create`.

   This is the riskiest code in the plugin: a misplaced splice does not lose a
   record, it corrupts a git command the user is actually running. So the
   suite asserts on both halves --

     * CLASSIFICATION: which commands get a recorder and which are left
       untouched. A command that cannot be placed exactly must be declined,
       because a missing record degrades to `pr-review.workdir` parsing while
       a broken command does not degrade at all.

     * EXECUTION: the rewritten command is then RUN, against stub `git`/`gh`,
       and checked for the properties the splice must preserve -- the `&&`
       conditionality of a preceding `cd`, the exit status of the push, the
       stdin of a following pipe, and the recorded directory itself.

   The wrapper delegates to rtk-rewrite.sh, so a fake rtk stands in: it echoes
   the command back as `updatedInput` verbatim. That keeps the suite
   independent of whatever the real rtk on this machine rewrites `git push`
   into, and lets the rtk-declined path be tested at all."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]))

;; `babashka.config` is the absolute bb.edn path, so the plugin root survives
;; `bb --config /abs/bb.edn test` from an unrelated cwd -- which is how the
;; suite is actually invoked.
(def ^:private plugin-root
  (if-let [cfg (System/getProperty "babashka.config")]
    (str (fs/parent cfg))
    (System/getProperty "user.dir")))

(def ^:private wrapper (str (fs/path plugin-root "hooks" "rtk-rewrite-wrapper.sh")))

(deftest wrapper-file-exists
  ;; Every other test here shells out to this path; without this, a rename
  ;; would turn the whole namespace into vacuous passes.
  (is (fs/exists? wrapper) (str "tracked wrapper missing: " wrapper)))

(defn- fake-rtk
  "An rtk stand-in echoing `updatedInput.command` back unchanged."
  [dir]
  (let [f (fs/path dir "fake-rtk.sh")]
    (spit (str f)
          (str "#!/usr/bin/env bash\n"
               "IN=$(cat)\n"
               "CMD=$(printf '%s' \"$IN\" | jq -r '.tool_input.command')\n"
               "jq -n --arg c \"$CMD\" "
               "'{hookSpecificOutput:{hookEventName:\"PreToolUse\","
               "updatedInput:{command:$c}}}'\n"))
    (fs/set-posix-file-permissions f "rwx------")
    (str f)))

(defn- silent-rtk
  "An rtk stand-in that emits NOTHING -- its 'no equivalent' / 'deny rule
   matched' / 'already in rtk form' outputs are indistinguishable, and the
   wrapper must rewrite nothing in that case."
  [dir]
  (let [f (fs/path dir "silent-rtk.sh")]
    (spit (str f) "#!/usr/bin/env bash\ncat >/dev/null\n")
    (fs/set-posix-file-permissions f "rwx------")
    (str f)))

(defn- run-wrapper
  "Feeds a Bash PreToolUse payload through the wrapper. Returns the emitted
   `updatedInput.command`, or nil when the wrapper declined to rewrite (either
   no JSON at all, or a command it handed back unchanged)."
  [{:keys [cmd id rtk]}]
  (let [payload (json/generate-string {:tool_name "Bash"
                                       :tool_use_id (or id "toolu_test")
                                       :tool_input {:command cmd}})
        {:keys [out]} (p/sh ["bash" wrapper]
                            {:in payload
                             :extra-env {"PRL_RTK_HOOK" rtk}})
        emitted (some-> (not-empty (str/trim out))
                        (json/parse-string true)
                        (get-in [:hookSpecificOutput :updatedInput :command]))]
    (when (and emitted (not= emitted cmd)) emitted)))

(defn- with-tmp [f]
  (let [d (fs/create-temp-dir {:prefix "prl-wrapper"})]
    (try (f (str d)) (finally (fs/delete-tree d)))))

;; --------------------------------------------------------------------------
;; classification
;; --------------------------------------------------------------------------

(def ^:private must-wrap
  {"bare git push"                 "git push"
   "rtk-prefixed push"             "rtk git push -u origin b"
   "cd through a variable"         "W=\"/tmp/x\"; cd \"$W\" && rtk git push -u origin b"
   "push mid-chain, piped"         "cd /tmp && git log -1 && git push 2>&1 | tail -2"
   "pipe with no space"            "cd /x && git push|cat"
   "brace group"                   "{ git push; }"
   "brace group, gh"               "{ rtk gh pr create; }"
   "subshell"                      "( cd /tmp && git push )"
   "after a background job"        "sleep 1 & git push"
   "newline separated"             "cd /tmp\ngit push"
   "after a comment line"          "# note\ngit push"
   ;; The whole point of scanning left-to-right and stopping at the head: all
   ;; three refused constructs sit to the RIGHT of it.
   "gh pr create, heredoc body"    "cd /tmp && gh pr create --body \"$(cat <<EOF\nhi\nEOF\n)\""
   "backtick to the right"         "cd /tmp && gh pr create --title `date`"})

(deftest wraps-what-it-can-place
  (with-tmp
    (fn [d]
      (let [rtk (fake-rtk d)]
        (doseq [[label cmd] must-wrap]
          (testing label
            (let [out (run-wrapper {:cmd cmd :id "toolu_W" :rtk rtk})]
              (is (some? out) "should have spliced a recorder")
              (when out
                (is (str/includes? out "__prl_d") "recorder marker absent")
                (is (str/includes? out "toolu_W") "tool_use_id not interpolated")))))))))

(def ^:private must-skip
  {;; Splicing `recorder && CMD` to the right of a pipe or `||` changes the
   ;; list's meaning: `(echo x | recorder) && git push` loses the push's stdin.
   "piped INTO the push"           "echo x | git push"
   "|| before the push"            "false || git push"
   ;; Refused constructs to the LEFT of the head: boundaries before it cannot
   ;; be placed without a real parser.
   "$() before the head"           "cd \"$(git rev-parse --show-toplevel)\" && git push"
   "heredoc before the head"       "cat <<EOF\nx\nEOF\ngit push"
   "process substitution"          "diff <(a) <(b) && git push"
   "unterminated quote"            "git push \"abc"
   ;; Not a placeable head.
   "git -C"                        "git -C /x push"
   "env assignment prefix"         "env A=1 git push"
   ;; Nothing to do.
   "no push at all"                "git status"
   "push is a prefix only"         "git pushx"
   "create is a prefix only"       "gh pr createx"
   ;; Idempotency: a command already carrying a recorder.
   "already recorded"              "x; ( __prl_d=1 ); git push"})

(deftest declines-what-it-cannot-place
  (with-tmp
    (fn [d]
      (let [rtk (fake-rtk d)]
        (doseq [[label cmd] must-skip]
          (testing label
            (is (nil? (run-wrapper {:cmd cmd :id "toolu_S" :rtk rtk}))
                "should have been left untouched")))))))

(deftest silent-rtk-means-rewrite-nothing
  ;; Rewriting while emitting no decision is the trap: Claude Code's native
  ;; deny rules would then be matched against a command the user never wrote.
  (with-tmp
    (fn [d]
      (is (nil? (run-wrapper {:cmd "cd /tmp && git push"
                              :id "toolu_Q"
                              :rtk (silent-rtk d)}))))))

(deftest injects-no-command-substitution
  ;; C40: Claude Code's permission analysis inspects the commands inside
  ;; `$(...)` and backticks. An injected substitution became an extra
  ;; subcommand to permission-check and failed the Bash call outright, so the
  ;; recorder must add exactly zero of them -- including on a command that
  ;; already contains one of the user's own.
  (with-tmp
    (fn [d]
      (let [rtk (fake-rtk d)
            n (fn [hay needle] (count (re-seq (re-pattern (java.util.regex.Pattern/quote needle)) hay)))]
        (doseq [cmd ["cd /tmp && git push"
                     "cd /tmp && gh pr create --body \"$(cat <<EOF\nhi\nEOF\n)\""]]
          (let [out (run-wrapper {:cmd cmd :id "toolu_C" :rtk rtk})]
            (is (some? out))
            (when out
              (is (= (n cmd "$(") (n out "$(")) "added a command substitution")
              (is (= (n cmd "`") (n out "`")) "added a backtick"))))))))

;; --------------------------------------------------------------------------
;; execution -- the rewritten command must behave like the original
;; --------------------------------------------------------------------------

(defn- stubs
  "Stub `git`/`gh` on PATH. Both announce their cwd; `git push` exits with
   `push-exit` so status propagation can be checked."
  [dir push-exit]
  (let [bin (fs/path dir "bin")]
    (fs/create-dirs bin)
    (doseq [[nm body] {"git" (str "#!/bin/sh\n"
                                  "[ \"$1\" = push ] && { echo \"PUSH-RAN pwd=$PWD\"; exit " push-exit "; }\n"
                                  "exit 0\n")
                       "gh"  "#!/bin/sh\necho \"GH-RAN pwd=$PWD\"\nexit 0\n"}]
      (let [f (fs/path bin nm)]
        (spit (str f) body)
        (fs/set-posix-file-permissions f "rwx------")))
    (str bin)))

(defn- exec-rewrite
  "Rewrites `cmd`, runs it, and returns {:exit :out :record}. `:record` is the
   recorder's file contents, or nil when no record was written."
  [{:keys [cmd id dir push-exit]}]
  (let [rtk (fake-rtk dir)
        bin (stubs dir (or push-exit 0))
        id (or id "toolu_E")
        new-cmd (run-wrapper {:cmd cmd :id id :rtk rtk})
        _ (assert new-cmd (str "wrapper declined to rewrite: " cmd))
        tmp (str (fs/path dir "rec"))
        _ (fs/create-dirs tmp)
        {:keys [exit out]} (p/sh ["bash" "-c" new-cmd]
                                 {:extra-env {"PATH" (str bin ":" (System/getenv "PATH"))
                                              "TMPDIR" tmp}})
        rec (fs/path tmp "pr-review-pushdir" id)]
    {:exit exit
     :out (str/trim (str out))
     :record (when (fs/exists? rec) (slurp (str rec)))}))

(deftest records-the-directory-the-push-runs-in
  ;; The defect this whole mechanism exists for: the payload's session cwd is
  ;; not where the agent pushed from when it pushed from a worktree.
  (with-tmp
    (fn [d]
      (let [wt (str (fs/path d "wt"))]
        (fs/create-dirs wt)
        (let [{:keys [exit out record]}
              (exec-rewrite {:cmd (str "cd " wt " && git push -u origin b")
                             :id "toolu_R" :dir d})]
          (is (zero? exit))
          (is (str/includes? out (str "PUSH-RAN pwd=" wt)))
          (is (some? record) "no record written")
          (when record
            (is (str/includes? record "tool_use_id=toolu_R"))
            ;; The recorded pwd is the POST-cd directory, not the shell's start.
            (is (str/includes? record (str "pwd=" wt)))))))))

(deftest a-failed-cd-still-stops-the-push
  ;; The splice is a new element of the enclosing list, so it is glued with
  ;; `&&`. Were it a `;`, a failed `cd` would no longer stop the push and the
  ;; push would run against whatever directory the shell started in.
  (with-tmp
    (fn [d]
      (let [{:keys [exit out record]}
            (exec-rewrite {:cmd (str "cd " (fs/path d "no-such-dir") " && git push")
                           :id "toolu_F" :dir d})]
        (is (pos? exit) "a failed cd must fail the command")
        (is (not (str/includes? out "PUSH-RAN")) "the push ran after a failed cd")
        (is (nil? record) "recorded a directory the push never used")))))

(deftest the-push-exit-status-propagates
  ;; The recorder ends in `|| :` precisely so it cannot mask this.
  (with-tmp
    (fn [d]
      (let [wt (str (fs/path d "wt"))]
        (fs/create-dirs wt)
        (is (= 7 (:exit (exec-rewrite {:cmd (str "cd " wt " && git push")
                                       :id "toolu_X" :dir d :push-exit 7}))))))))

(deftest a-following-pipe-still-gets-the-push-output
  ;; `|` binds tighter than `&&`, so `{ rec; } && git push | tail` pipes the
  ;; PUSH's output, not the group's. The recorder's own output is redirected to
  ;; /dev/null so it can never enter that pipe.
  (with-tmp
    (fn [d]
      (let [wt (str (fs/path d "wt"))]
        (fs/create-dirs wt)
        (let [{:keys [exit out]}
              (exec-rewrite {:cmd (str "cd " wt " && git push -u origin b 2>&1 | tail -1")
                             :id "toolu_P" :dir d})]
          (is (zero? exit))
          (is (= (str "PUSH-RAN pwd=" wt) out)
              "tail received something other than the push's last line"))))))

(deftest gh-pr-create-with-a-heredoc-body-is-recorded
  ;; The command that actually opens a PR. It carries `$(`, a backtick and a
  ;; heredoc, all to the right of the head; an earlier revision spliced the
  ;; recorder as `{ rec; CMD; }`, needed the segment's END, and so declined
  ;; the one command the loop depends on.
  (with-tmp
    (fn [d]
      (let [wt (str (fs/path d "wt"))]
        (fs/create-dirs wt)
        (let [{:keys [exit out record]}
              (exec-rewrite {:cmd (str "cd " wt " && gh pr create --title `date`"
                                       " --body \"$(cat <<'BODY'\nbody text\nBODY\n)\"")
                             :id "toolu_H" :dir d})]
          (is (zero? exit))
          (is (str/includes? out "GH-RAN"))
          (is (some? record) "gh pr create was not recorded")
          (when record
            (is (str/includes? record (str "pwd=" wt)))))))))
