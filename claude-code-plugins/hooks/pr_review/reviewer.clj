(ns pr-review.reviewer
  "Spawn the independent reviewer and parse what it says.

   The reviewer is a separate `claude -p` process: fresh context window, none
   of agent A's conversation history, the same machine and working tree. It is
   confined to Read, Grep and Glob — no shell, so nothing it runs can be
   rewritten by rtk and nothing it does can touch the tree."
  (:require [babashka.process :as p]
            [clojure.string :as str]))

(def categories
  ["correctness/blocking" "correctness/followup" "coverage"
   "docs-accuracy" "style"])

(def denied-tools
  "The reviewer's sandbox. THIS list is the mechanism; `--allowedTools` is
   not — see `claude-argv`.

   Grouped by what each entry would buy an escaped reviewer: a shell or a
   writer (Bash, Write, Edit, MultiEdit, NotebookEdit, Notebook*, Bash*),
   another agent to do it for it (Agent, Task, TaskStop, SendMessage,
   SendUserMessage, ListAgents), a route off this machine (WebFetch,
   WebSearch, Artifact* — Artifact publishes to the web, PushNotification,
   RemoteTrigger, Monitor, DesignSync, ShareOnboardingGuide), a way to make
   work happen later (Cron*, ScheduleWakeup, Workflow, Skill), a way to
   reach a tool not in this list at all (ToolSearch, the MCP resource
   readers), or a place to put a finding where the parser will never see it
   (ReportFindings, Task*).

   MultiEdit no longer exists in Claude Code 2.1.263 — it is kept because an
   unknown name costs one warning line on stderr, and a reintroduced editing
   tool would otherwise be granted silently."
  ["Agent" "Artifact" "ArtifactCheck" "ArtifactComments" "ArtifactData"
   "Bash" "BashOutput" "CronCreate" "CronDelete" "CronList" "DesignSync"
   "Edit" "EnterWorktree" "ExitWorktree" "KillShell" "ListAgents"
   "ListMcpResourcesTool" "Monitor" "MultiEdit" "NotebookEdit"
   "PushNotification" "ReadMcpResourceDirTool" "ReadMcpResourceTool"
   "RemoteTrigger" "ReportFindings" "ScheduleWakeup" "SendMessage"
   "SendUserMessage" "ShareOnboardingGuide" "Skill" "Task" "TaskCreate"
   "TaskGet" "TaskList" "TaskStop" "TaskUpdate" "ToolSearch" "WebFetch"
   "WebSearch" "Workflow" "Write"])

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
   "--allowedTools" "Read,Grep,Glob"])

(defn- default-spawn
  [argv prompt dir]
  (let [{:keys [exit out err]} (p/sh argv {:dir dir :in prompt})]
    {:exit exit :out (or out "") :err (or err "")}))

(defn run!
  "Run the reviewer with `prompt` on stdin, in `repo-root`.
   Never throws: a spawn failure becomes a non-zero exit with the message in
   :err, so the caller can still tell the author what happened."
  [prompt repo-root opts]
  (let [spawn (or (:spawn-fn opts) default-spawn)]
    (try (spawn (claude-argv) prompt repo-root)
         (catch Exception e {:exit 127 :out "" :err (str (ex-message e))}))))

(defn- parse-verdict
  "The verdict line must start at column 0. Leading whitespace means the line
   is quoted or indented — e.g. an echoed copy of the core prompt's own format
   example — not the reviewer's real, final verdict. Without this anchor an
   echoed template parses as a clean pass, which is worse than the reviewer
   failing to run at all: it emits a positive signal for a review that never
   happened."
  [out]
  (when-let [[_ v] (re-find #"(?m)^VERDICT:\s*(MERGEABLE|NOT MERGEABLE)" out)]
    v))

(defn- parse-counts
  "Read the per-category count block. \"none\" means 0 — a missing key and a
   zero count must not be confusable, or a clean pass reads as an unparsed one."
  [out]
  (into {}
        (map (fn [cat]
               (let [re (re-pattern (str "(?m)^\\s*\\[" cat "\\]\\s+(none|\\d+)"))
                     [_ n] (re-find re out)]
                 [cat (cond (nil? n) 0
                            (= "none" n) 0
                            :else (parse-long n))])))
        categories))

(defn- parse-fingerprints
  "Stable identity for a finding: file:line:category, taken from numbered
   finding lines of the form `N. [category] path:line — text`. The path
   segment is non-greedy and anchors on the final `:<digits>` boundary (the
   number is followed by whitespace or end of line), so a path containing a
   space or an internal colon is still captured whole instead of truncating
   at the first space or colon inside it."
  [out]
  (->> (str/split-lines out)
       (keep (fn [line]
               (when-let [[_ cat path ln]
                          (re-find #"^\s*\d+\.\s*\[([a-z/-]+)\]\s+(.+?):(\d+)(?=\s|$)" line)]
                 (str path ":" ln ":" cat))))
       distinct
       vec))

(defn parse-output
  [out]
  (let [out (or out "")]
    (if-let [v (parse-verdict out)]
      {:verdict v
       :counts (parse-counts out)
       :fingerprints (parse-fingerprints out)
       :body out}
      {:verdict "MALFORMED"
       :counts (zipmap categories (repeat 0))
       :fingerprints []
       :body (str/trim out)})))

(defn mergeable?
  "MERGEABLE means exactly: no correctness/blocking finding, and no coverage
   finding. The verdict line is the reviewer's claim; the counts are the
   evidence, and the evidence wins."
  [{:keys [verdict counts]}]
  (and (= "MERGEABLE" verdict)
       (zero? (get counts "correctness/blocking" 0))
       (zero? (get counts "coverage" 0))))
