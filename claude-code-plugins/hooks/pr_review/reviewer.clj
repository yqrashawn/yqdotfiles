(ns pr-review.reviewer
  "Spawn the independent reviewer and parse what it says.

   The reviewer is a separate `claude -p` process: fresh context window, none
   of agent A's conversation history, the same machine and working tree. It is
   granted Read, Grep, Glob and Bash, in a throwaway worktree — so what it runs cannot be
   rewritten by rtk and nothing it does can touch the tree."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [pr-review.atomicfile :as atomicfile]))

(def categories
  ["correctness/blocking" "correctness/followup" "coverage"
   "docs-accuracy" "style"])

(def reviewer-env-var
  "Set in the reviewer's environment so the trigger can recognise its own
   grandchildren and refuse to act.

   The reviewer is a plain `claude -p`, so it loads the user's settings AND
   every enabled plugin -- including this one. Measured: a `review-trigger`
   process spawns inside the reviewer on its Bash calls, and the reviewer uses
   Bash heavily now (it runs test suites). Most of those triggers go silent,
   but the abandoned-review path is deliberately neither session-scoped nor
   verb-gated, so one of them can find outstanding work and start a REVIEW
   INSIDE A REVIEW -- a grandchild that dies when the outer reviewer exits."
  "PR_REVIEW_LOOP_REVIEWER")

(def denied-tools
  "The reviewer's sandbox. THIS list is the mechanism; `--allowedTools` is
   not — see `claude-argv`.

   BASH IS GRANTED, on the author's instruction, so the reviewer can inspect
   the change the way it wants rather than only through a precomputed diff.
   That is a deliberate reversal of R7's original wording and it costs real
   containment, so state the position plainly rather than imply otherwise:

     * Write and Edit are still denied, but that is no longer a write
       barrier. `printf x > file` works — measured. What actually bounds the
       damage is that the reviewer runs in a THROWAWAY detached worktree
       pinned to the reviewed sha (`pr-review.checkout`), which is removed
       when the pass ends, so anything it does to its own tree is discarded.
     * Nothing bounds it outside that tree. The user's settings.json sets
       `permissions.defaultMode: \"bypassPermissions\"`, so a shell is a
       shell.

   The `Bash(...)` entries are therefore not a sandbox; they close the
   specific harms that are known and reachable. Measured to work: a denied
   `rm` came back \"Denied by user\" while `git log` and a `printf >` in the
   same session both ran, so specifier denies are enforced even under
   bypassPermissions.

     rm, sudo          irreversible or privileged, and never needed to read a
                       change
     git push, commit  the reviewer must not alter the PR it is reviewing —
                       and a push from here would carry the PARENT session's
                       CLAUDE_CODE_SESSION_ID, so the pre-push hook would
                       record it as an agent push and the loop would review
                       the reviewer's own commit
   `gh` IS NOT RESTRICTED AT ALL, on the author's instruction, so the reviewer
   can read the PR the prompt points it at. What that also allows, stated
   rather than left to be discovered:

     * `gh pr merge` -- it can merge the PR it is reviewing.
     * `gh pr comment`, `gh pr review` -- it can comment, which reverses R13:
       the ledger was its only channel and agent A posted the one summary.
     * `gh api` -- anything the token reaches, including other repositories.

   The prompt asks it to do none of these and says the ledger is its only
   output. That is guidance, not enforcement. `curl`/`wget`/`nc` stay denied:
   `gh` reaches GitHub with a known token, those reach anywhere
     curl, wget, nc    WebFetch and WebSearch are denied for being a route
                       off this machine; leaving these open reopens it

   NOT closed, and worth knowing: the production nREPL on port 8034 is
   reachable from a shell by any spelling `nc` does not cover, and the test
   suite can be run — which may touch a database. review_core.md permits
   running it and discourages it -- `allowed but rarely worth it` -- which is
   guidance, not enforcement.

   The tool entries are grouped by what each would buy an escaped reviewer:
   a writer (Write, Edit, MultiEdit, NotebookEdit), another agent to act for
   it (Agent, Task, TaskStop, SendMessage, SendUserMessage, ListAgents), a
   route off this machine (WebFetch, WebSearch, Artifact* — Artifact
   publishes to the web, PushNotification, RemoteTrigger, Monitor,
   DesignSync, ShareOnboardingGuide), a way to make work happen later (Cron*,
   ScheduleWakeup, Workflow, Skill), a way to reach a tool not in this list
   at all (ToolSearch, the MCP resource readers), or a place to put a finding
   where the parser will never see it (ReportFindings, Task*).

   Every name here must be a tool Claude Code actually has. An unknown one
   costs a warning line on stderr — `Permission deny rule \"X\" matches no
   known tool` — and stderr is now the channel a KILLED review leaves its
   evidence on. A file whose only content is a false positive makes a dead
   reviewer look like it said something, and it was 76 bytes of it on every
   review.

   `MultiEdit` was carried here after Claude Code dropped it, on the argument
   that a reintroduced editing tool would otherwise be granted silently. That
   argument died when Bash was granted: `printf x > file` writes, measured, so
   denying editing tools is not a write barrier and guarding a hypothetical
   one buys nothing Bash does not already allow."
  ["Agent" "Artifact" "ArtifactCheck" "ArtifactComments" "ArtifactData"
   "CronCreate" "CronDelete" "CronList" "DesignSync"
   "Edit" "EnterWorktree" "ExitWorktree" "ListAgents"
   "ListMcpResourcesTool" "Monitor" "NotebookEdit"
   "PushNotification" "ReadMcpResourceDirTool" "ReadMcpResourceTool"
   "RemoteTrigger" "ReportFindings" "ScheduleWakeup" "SendMessage"
   "SendUserMessage" "ShareOnboardingGuide" "Skill" "Task" "TaskCreate"
   "TaskGet" "TaskList" "TaskStop" "TaskUpdate" "ToolSearch" "WebFetch"
   "WebSearch" "Workflow" "Write"
   ;; Command shapes, not tools. See the docstring for why each is here.
   "Bash(rm:*)" "Bash(sudo:*)"
   ;; The MUTATING gh subcommands only. A blanket `Bash(gh pr:*)` is wrong now:
   ;; the prompt tells the reviewer to run `gh pr view` to read the PR it is
   ;; reviewing. These are the ones that would let it act on the PR instead —
   ;; commenting, merging and closing are agent A's job, and the ledger plus
   ;; the one posted review are this reviewer's only channels.
   "Bash(gh pr merge:*)" "Bash(gh pr close:*)" "Bash(gh pr edit:*)"
   "Bash(gh pr comment:*)" "Bash(gh pr review:*)" "Bash(gh pr ready:*)"
   "Bash(gh pr reopen:*)" "Bash(gh api:*)"
   "Bash(git push:*)" "Bash(git commit:*)"
   ;; Read-only `gh` is NOT restricted: the reviewer can read the PR, its
   ;; description and its comments. Only the mutating subcommands above are
   ;; refused, and `gh api` with them -- it is the spelling that reaches every
   ;; one of them again.
   "Bash(curl:*)" "Bash(wget:*)" "Bash(nc:*)"])

(defn claude-argv
  []
  ["claude" "-p"
   "--model" "opus"
   ;; `--disallowedTools` is the load-bearing mechanism and the only one.
   ;;
   ;; `--allowedTools` is a PRE-APPROVAL allowlist, not a tool restriction:
   ;; it says which calls skip the permission prompt, not which tools exist.
   ;; With `permissions.defaultMode: "bypassPermissions"` in the user's
   ;; ~/.claude/settings.json — and no allow/deny/ask rules at all — every
   ;; tool is auto-approved regardless. Measured: this exact invocation with
   ;; only `--allowedTools "Read,Grep,Glob"` still had Bash in its function
   ;; list and wrote a file outside every repo. `--permission-mode default`
   ;; does not close it either; the write still happened. The deny list does:
   ;; the tools vanish from the function list and the write does not happen.
   ;;
   ;; This is a deny list, so it is not airtight. A tool added to Claude Code
   ;; in a future version is granted to the reviewer by default, and only
   ;; appears here once someone notices. `denied-tools` needs re-checking
   ;; against a live probe on every Claude Code upgrade.
   "--disallowedTools" (str/join "," denied-tools)
   ;; No MCP server the user happens to have configured — several of them
   ;; write files and reach the network, and none of them are in the deny
   ;; list because their names are per-installation.
   "--strict-mcp-config"
   ;; Kept for intent, and harmless: it documents the three tools the review
   ;; is supposed to need. It restricts nothing.
   "--allowedTools" "Read,Grep,Glob,Bash"])

(def default-token-file
  "Where the reviewer's own OAuth token lives, if it has one.

   A separate credential on purpose: the reviewer is a `claude -p` the loop
   spawns with nobody watching, and pinning it to one token keeps its usage
   attributable and independent of however the session that triggered it
   happens to be authenticated."
  (str (fs/path (System/getProperty "user.home")
                "Library" "CloudStorage" "Dropbox" "sync" "default-cc-token")))

(defn oauth-token
  "The token to run the reviewer with, or nil to leave authentication alone.

   Trimmed: the file ends with a newline, and a token carrying one is not the
   token. Absent, empty or unreadable all mean nil rather than an error —
   losing a review is worse than running it as whoever the parent is, and this
   must never be the thing that stops a review starting."
  ([] (oauth-token default-token-file))
  ([path]
   (try
     (when (and path (fs/regular-file? path))
       (let [t (str/trim (slurp (str path)))]
         (when (seq t) t)))
     (catch Exception _ nil))))

(defn spawn-env
  "The environment the reviewer runs with.

   `reviewer-env-var` is not optional and must survive every future addition
   here: it is what stops a review triggering a review inside itself.

   The token is added only when there is one. `:extra-env` MERGES into the
   inherited environment, so setting CLAUDE_CODE_OAUTH_TOKEN to an empty
   string would replace working credentials with a blank one — absent has to
   mean absent."
  []
  (cond-> {reviewer-env-var "1"}
    (oauth-token) (assoc "CLAUDE_CODE_OAUTH_TOKEN" (oauth-token))))

(defn redact
  "Replace the reviewer's own credential with a marker.

   Everything the reviewer prints is recorded and republished: `:out` becomes
   the PR comment and the findings file, `:err` is streamed to a file the
   author reads. The reviewer runs with an OAuth token in its environment and
   has an unrestricted shell, so `env`, a stray `echo $CLAUDE_CODE_OAUTH_TOKEN`
   while debugging, or a tool dumping its environment on error all put the
   credential into that stream.

   Nothing else redacted: a value we do not know cannot be matched, and
   guessing at shapes would give false confidence. This covers the one secret
   this process is known to hold."
  [text token]
  (if (and (string? text) (string? token) (>= (count token) 8))
    (str/replace text token "[redacted]")
    text))

(defn- default-spawn
  "Runs the reviewer. `err-file`, when given, receives stderr AS IT IS WRITTEN
   and is read back afterwards, so `:err` behaves as before.

   Streaming rather than capturing, because the interesting case is the one
   where nothing is returned at all: two reviews were killed mid-run after 5m48s
   and 7m33s, and the loop had discarded everything the reviewer said, so there
   was no way to tell why. A SIGKILL leaves whatever reached the file."
  [argv prompt dir err-file]
  (let [base {:dir dir :in prompt :extra-env (spawn-env)}]
    (if err-file
      (let [f (io/file err-file)
            _ (io/make-parents f)
            {:keys [exit out]} (p/sh argv (assoc base :err :write :err-file f))]
        {:exit exit :out (or out "")
         :err (try (slurp f) (catch Exception _ ""))})
      (let [{:keys [exit out err]} (p/sh argv base)]
        {:exit exit :out (or out "") :err (or err "")}))))

(defn run!
  "Run the reviewer with `prompt` on stdin, in `repo-root`.
   Never throws: a spawn failure becomes a non-zero exit with the message in
   :err, so the caller can still tell the author what happened."
  [prompt repo-root opts]
  (let [spawn (or (:spawn-fn opts) default-spawn)
        token ((or (:token-fn opts) oauth-token))
        scrub #(redact % token)]
    (try
      (let [res (spawn (claude-argv) prompt repo-root (:err-file opts))]
        ;; Redacted here, at the one place every caller goes through, rather
        ;; than at each of the three places the text is republished — the PR
        ;; comment, the findings file and the streamed stderr. The stderr FILE
        ;; is rewritten too: it is on disk for the author to read, and it is
        ;; the copy that survives a kill.
        (when-let [f (:err-file opts)]
          (when (and token (fs/exists? f))
            (let [raw (slurp f) clean (scrub raw)]
              (when (not= raw clean) (atomicfile/spit! f clean)))))
        (-> res (update :out scrub) (update :err scrub)))
      (catch Exception e {:exit 127 :out "" :err (scrub (str (ex-message e)))}))))

(defn- strip-emphasis
  "Remove inline markdown emphasis so one parser handles every shape a model
   actually emits. A backticked path (`` `src/retry.clj:42` ``) is the single
   likeliest reviewer shape, and a bolded one is next; both used to yield a
   counted finding with no fingerprint at all, which made the one-re-raise
   rule permanently unable to fire for it."
  [line]
  (str/replace line #"[`*]" ""))

(defn- unfenced
  "Lines outside every ``` fenced block, with fenced lines blanked rather than
   dropped so line positions still line up with the reply.

   A fence does not indent, so a quoted prior review inside one carries its
   `VERDICT:` at column 0 — and `parse-verdict` takes the LAST such line. This
   branch made that reachable: it posts every pass as a PR comment, tells the
   reviewer to read them, and tells a re-review to verify closure against the
   previous pass. Measured: a reply whose own verdict is NOT MERGEABLE with one
   blocking finding, quoting pass 1's MERGEABLE comment in a fence, parsed as
   MERGEABLE with 0 blocking — `reconcile` confirmed it, the ledger recorded a
   clean pass, and agent A was told it could merge a PR with a blocking defect.

   Blanking the whole fenced region, not just verdict lines: a quoted review
   carries counts and finding lines too, and each of those is parsed."
  [lines]
  (first
   (reduce (fn [[acc in-fence?] line]
             (if (re-find #"^\s*```" line)
               [(conj acc "") (not in-fence?)]
               [(conj acc (if in-fence? "" line)) in-fence?]))
           [[] false]
           lines)))

(defn- normalized-lines
  "Fences are stripped BEFORE emphasis, not after: `strip-emphasis` deletes
   every backtick, so a ``` line reaches an emphasis-first pipeline as an empty
   string and no fence is ever detected."
  [out]
  (mapv strip-emphasis (unfenced (str/split-lines out))))

(defn- parse-verdict
  "The LAST line that starts a verdict at column 0.

   Two rules, each closing a different false-clean path.

   Column 0: leading whitespace means the line is quoted or indented — an
   echoed copy of the core prompt's own format example, say — not a real
   verdict. Without the anchor an echoed template parses as a clean pass,
   worse than the reviewer failing to run at all because it emits a positive
   signal for a review that never happened.

   Last, not first: a reviewer that restates the required format unindented
   before reviewing anything used to have that restatement parsed as its
   answer. The real verdict is the one it ends on."
  [lines]
  (->> lines
       (keep #(second (re-find #"^VERDICT:\s*(MERGEABLE|NOT MERGEABLE)" %)))
       last))

(defn- verdict-index
  "Index of the line `parse-verdict` chose, or nil. Same rule, same anchor —
   so the count block can be read from that verdict's own block rather than
   from whichever one came first."
  [lines]
  (->> (map-indexed vector lines)
       (keep (fn [[i l]]
               (when (re-find #"^VERDICT:\s*(MERGEABLE|NOT MERGEABLE)" l) i)))
       last))

(defn- parse-counts
  "Read the per-category count block. \"none\" means 0 — a missing key and a
   zero count must not be confusable, or a clean pass reads as an unparsed one.

   Case-insensitive over emphasis-stripped lines for the same reason
   `parse-fingerprints` is: a bolded or capitalised count block that parses as
   all-zero is a false clean, since `mergeable?` reads these counts as the
   evidence that overrides the verdict line."
  [lines]
  (into {}
        (map (fn [cat]
               (let [re (re-pattern (str "(?i)^\\s*\\[" cat "\\]\\s+(none|\\d+)"))
                     n  (->> lines (keep #(second (re-find re %))) first)]
                 [cat (cond (nil? n) 0
                            (= "none" (str/lower-case n)) 0
                            :else (parse-long n))])))
        categories))

(def ^:private finding-line-re
  "One finding line, in every shape a model plausibly writes it.

   The invariant is `[category] <path>:<line>` — not the canonical
   `N. [category] path:line — text` the prompt asks for. Anything narrower
   has to enumerate shapes, and each shape it misses is a counted finding
   with an empty fingerprint: invisible to the one-re-raise rule, so
   re-reported on every pass straight into the 10-pass cap.

   Deliberately permissive about everything that is not the invariant:
     - any bullet, or none: `1.`  `2)`  `-`  `*`  `+`  `•`
     - any category case: `[Correctness/Blocking]`
     - `L`-prefixed and ranged lines: `:L42`  `:42-45`
     - any separator after the line number: em-dash, colon, comma, EOL
   Emphasis is stripped before this runs, so backticked and bolded paths
   arrive bare.

   The path is non-greedy and anchors on the first `:<digits>` boundary that
   is actually followed by a separator, so a path holding a space or an
   internal colon (`src/pool:v2/file.clj:34`) is still captured whole.

   A range collapses to its first line: a reviewer that writes `42` one pass
   and `42-45` the next must produce the same fingerprint, or the rule cannot
   see the re-raise."
  #"(?i)^\s*(?:\d+[.)]|[-+•])?\s*\[([a-z][a-z/-]*)\]\s+(.+?):L?(\d+)(?:-\d+)?(?=[\s:,;)\]]|$)")

(defn- parse-fingerprints
  "Stable identity for a finding: `file:line:category`, lower-cased category."
  [lines]
  (->> lines
       (keep (fn [line]
               (when-let [[_ cat path ln] (re-find finding-line-re line)]
                 (str path ":" ln ":" (str/lower-case cat)))))
       distinct
       vec))

(defn parse-output
  [out]
  (let [out   (or out "")
        lines (normalized-lines out)]
    (if-let [v (parse-verdict lines)]
      {:verdict v
       ;; Counts come from AFTER the winning verdict line, not from the first
       ;; block in the reply. `parse-verdict` takes the LAST verdict at column
       ;; 0 because reviewers restate the format, or recap the previous pass,
       ;; before answering; `parse-counts` took the FIRST count block, so the
       ;; two could describe different blocks. Measured: a reviewer recapping
       ;; a previous NOT MERGEABLE pass and then reporting MERGEABLE had its
       ;; clean verdict reconciled back to NOT MERGEABLE off the recap's
       ;; counts, and the loop ran another round on a PR that was done.
       ;;
       ;; Fingerprints deliberately still read the whole reply: the prompt
       ;; asks for verdict, then counts, then findings, but a reviewer that
       ;; puts its findings before its final verdict line would lose all of
       ;; them, and losing findings is worse than the counts being off.
       :counts (parse-counts (drop (inc (verdict-index lines)) lines))
       :fingerprints (parse-fingerprints lines)
       :body out}
      {:verdict "MALFORMED"
       :counts (zipmap categories (repeat 0))
       :fingerprints []
       :body (str/trim out)})))

(defn mergeable?
  "MERGEABLE means exactly: the reviewer said so, and its own count block
   agrees there is no [correctness/blocking] finding. The verdict line is the
   reviewer's claim; the counts are the evidence, and the evidence wins.

   No coverage clause, deliberately. The spec and the core prompt both define
   MERGEABLE as no blocking finding and no coverage finding \"in which a test
   certifies a safety property it does not check\" — a judgement about one
   finding's content, which a bare `[coverage] N` count cannot express. This
   function used to demand zero coverage findings outright, which would have
   turned any benign coverage nit into a false NOT-clean the moment it was
   wired in. The narrow clause stays where it can be judged: in the
   reviewer's own verdict line, which this function still requires."
  [{:keys [verdict counts]}]
  (and (= "MERGEABLE" verdict)
       (zero? (get counts "correctness/blocking" 0))))

(defn reconcile
  "Replace the reviewer's claimed verdict with the one its own counts support,
   and say so in the body when they disagreed.

   This is `mergeable?` wired in. Before it was, `mergeable?` had zero
   production call sites: any output whose count block contradicted its
   verdict line — `VERDICT: MERGEABLE` over `[correctness/blocking] 1` —
   produced a MERGEABLE headline for agent A and a self-contradictory ledger
   row, and the skill merged on it.

   MALFORMED is passed through: there is no verdict to reconcile, and the
   count block of an unparsed review is all zeros by construction."
  [parsed]
  (if (= "MALFORMED" (:verdict parsed))
    parsed
    (let [effective (if (mergeable? parsed) "MERGEABLE" "NOT MERGEABLE")]
      (if (= effective (:verdict parsed))
        parsed
        (assoc parsed
               :verdict effective
               :body (str (:body parsed)
                          "\n\npr-review-loop: the reviewer's verdict line said "
                          (:verdict parsed) " while its own count block reported "
                          (get (:counts parsed) "correctness/blocking" 0)
                          " [correctness/blocking] finding(s). Recorded as "
                          effective " — the counts are the evidence."))))))

(defn parse-warnings
  "Diagnostics about the parse itself, for the wake message.

   Non-zero counts with no fingerprints at all is a parse failure, not a
   quiet review: the findings exist, but none of them carries an identity, so
   the one-re-raise rule can never fire for any of them and every one is
   re-reported until the cap. Nothing used to notice this."
  [{:keys [counts fingerprints]}]
  (let [total (reduce + 0 (vals counts))]
    (cond-> []
      (and (pos? total) (empty? fingerprints))
      (conj (str "pr-review-loop parse warning: the count block reports "
                 total " finding(s) but not one finding line carried a"
                 " parseable `path:line`, so none of them has a fingerprint"
                 " and the one-re-raise rule cannot track any of them."
                 " Check the reviewer's finding-line format against"
                 " review_core.md.")))))
