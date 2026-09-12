# pr-review-loop — Design Spec

**Date:** 2026-09-08
**Status:** implemented 2026-09-08
**Plan:** [../plans/2026-09-08-pr-review-loop.md](../plans/2026-09-08-pr-review-loop.md)

## Problem

Today a Claude reviewer (agent B) runs in GitHub Actions
(`.github/workflows/claude-code-review.yml` in `claude-code-http-proxy`) on
`pull_request: [opened, synchronize, ready_for_review, reopened]`. A local
hand-driven Claude Code session (agent A) writes code, pushes, reads B's
findings, verifies blocking ones, fixes and re-pushes, then merges and handles
follow-ups on a new PR.

Two reasons to move B off Actions:

1. The repo is **private**, so runner minutes are billed and are wanted
   elsewhere. Claude spend does not move — the Action already burns
   `secrets.ANTHROPIC_API_KEY` / the OAuth token. Only minutes are saved.
2. A local B can see more: the real working tree, full history, the dev server,
   and (for Clojure) a live nREPL.

## Goal

When agent A creates a PR or pushes to a PR branch, in **any** repo, on
whichever machine A is running on: automatically run an independent reviewer
with fresh context, deliver its findings back to A without A having to ask,
preserve the FIRST/RE-REVIEW convergence discipline, and consume zero Actions
minutes. A manual push by the human triggers nothing.

## Constraints

Measured against Claude Code 2.1.263, babashka 1.12.209, rtk 0.34.3, gh 2.83.2
on darwin 24.3.0. Confidence noted per item.

### Harness

| # | Constraint | Confidence |
|---|---|---|
| C1 | `asyncRewake: true` runs a hook in the background and wakes the model on **exit code 2**. Implies `async`. | confirmed — binary schema + live test |
| C2 | An `asyncRewake` hook wakes an **idle interactive** session. Measured: turn ended t+5s, session woke t+90s and acted on the payload. | confirmed — tmux |
| C3 | In `-p` (non-interactive) mode Claude Code **kills** any async hook still running at teardown, outcome `cancelled`. | confirmed — official docs; reproduced twice |
| C4 | The rewake payload is the hook's **stderr**. stdout is discarded even when stderr is empty — verified with explicit flush and with a second (bash) implementation. Docs claim a stdout fallback; it did not occur. | confirmed — 4 variants, 2 implementations |
| C5 | `rewakeMessage` / `rewakeSummary` are **ignored** for a third-party plugin. The terminal shows the default `Stop hook feedback`; the model sees `PostToolUse:Bash hook blocking error from command: "<cmd>": […]: <stderr>`. Both fields are `@internal`. | confirmed — 6 reminders, incl. a clone of `security-guidance`'s exact hook shape |
| C6 | `timeout` defaults to **600 s** for `command` hooks and is **not enforced** for `async: true`. A sync hook ran 140 s uninterrupted. | confirmed — docs + measurement |
| C7 | `if` uses permission-rule syntax, is evaluated **only** on `PreToolUse`, `PostToolUse`, `PostToolUseFailure`, `PermissionRequest`, `PermissionDenied`, checks each subcommand of a compound command, strips leading env assignments, inspects `$()`/backticks, and **runs the hook anyway** when it cannot determine the command. It is explicitly best-effort. | confirmed — docs + measurement |
| C8 | `Tool(prefix:*)` requires whitespace or end-of-string after `prefix`. `Bash(echo PROBE:*)` does **not** match `echo PROBE: zzz`; it does match `echo PROBE zzz`. | confirmed — controlled pair |
| C9 | A failed Bash call routes to `PostToolUseFailure`, not `PostToolUse`. A failed push therefore does not trigger. | probable — one negative observation + separate event export |
| C10 | Plugin hooks do **not** hot-load into a running session; a new session is required. Settings-file hooks do hot-load. | confirmed |
| C11 | A hook with no `if` fires in **every** session on the machine, not just the repo it was installed for. | confirmed — logged an unrelated concurrent session |
| C12 | `${CLAUDE_PLUGIN_ROOT}` is available only in a plugin's `hooks/hooks.json`, not in `settings.json`. `${CLAUDE_PLUGIN_DATA}`, `${CLAUDE_PROJECT_DIR}`, `${user_config.*}` and `$CLAUDE_PLUGIN_OPTION_<KEY>` also exist. | confirmed |
| C13 | The `args` exec form spawns `command` directly with no shell; placeholders are substituted per element as plain strings. | confirmed — docs + live |
| C14 | A **subagent's** Bash calls go through `PreToolUse` hooks exactly as the main session's do. | confirmed — measured `rtk git status --short` from a subagent |
| C15 | `--output-format stream-json` reports the model's **pre-rewrite** tool input. Only the `PostToolUse` hook sees the rewritten command. | confirmed |

### rtk

| # | Constraint | Confidence |
|---|---|---|
| C16 | `~/.claude/hooks/rtk-rewrite.sh` is a `PreToolUse` hook that rewrites commands before `PostToolUse` sees them. `git push …` → `rtk git push …`; `gh pr create …` → `rtk gh pr create …`. | confirmed |
| C17 | Registry coverage: **12 of 38** git subcommands rewritten (`push pull fetch commit status diff log add show branch stash worktree`); **22 of 28** gh probed rewritten, including every `pr` subcommand plus `api`. `git rev-parse` and `git checkout` pass through. Also rewrites `rg`→`rtk grep`, `cat`→`rtk read`, `ls`, `find`, `make`, `cargo`. | confirmed — direct `rtk rewrite` probe |
| C18 | Compound commands are rewritten per subcommand; leading env assignments are preserved in place. `git add -A && git commit -m x && git push` → `rtk git add -A && rtk git commit -m x && rtk git push`. | confirmed |
| C19 | **`rtk git diff HEAD~1` returns 195 bytes where plain `git diff HEAD~1` is 80 162** — a stat summary, not a diff. 411× reduction. | confirmed — measured on a real commit |
| C20 | `rtk gh pr view --json …` is **byte-identical** passthrough (58 679 = 58 679, valid JSON, 9 comments intact). Only human-facing output is compacted. | confirmed |
| C21 | rtk is installed by `modules/yqrashawn/common.nix:327`; `modules/yqrashawn/home-manager/activations/default.nix:126-129` runs `rtk init --global --auto-patch` on **every** activation, which registers the hook in `~/Dropbox/sync/claude-settings.json` → propagates to every machine. | confirmed |
| C22 | `rtk init --global --auto-patch` **splices**: it appends one matcher group to `hooks.PreToolUse` and preserves existing hooks, `enabledPlugins`, `extraKnownMarketplaces` and `permissions`. Idempotent — second run reports `hook already present` and adds nothing. Writes a `.bak` first. | confirmed — sandboxed `HOME`, two runs |
| C23 | The hook passes the command through **unchanged** on rtk exit 2 (a Claude Code deny rule matched), and exits 0 without rewriting if `rtk` or `jq` is missing or rtk < 0.23.0. So both rewritten and bare command shapes can reach `PostToolUse`. Exits 2/3 are currently unreachable: there are zero `allow`/`deny`/`ask` rules mentioning git or gh in `~/.claude/settings.json`, and `claude-code-http-proxy` has no project `settings.json`. | confirmed |
| C24 | rtk reads deny/ask rules from **both** `settings.json` and `settings.local.json`, so a repo-local rule can flip the shape for one repo. | probable — colocated binary strings |
| C25 | `RTK_HOOK_AUDIT=1` with `RTK_AUDIT_DIR` writes `hook-audit.log` with rewrite/skip counts. | confirmed — binary strings + `rtk init --show` |

### Plugin and tooling

| # | Constraint | Confidence |
|---|---|---|
| C26 | `claude plugin marketplace add <local dir>` works and registers as `Source: Directory (<path>)`. A git repo is **not** required — verified by adding and installing from a plain, non-git directory. The `gitCommitSha` in `installed_plugins.json` is recorded when one is available, not demanded. | confirmed — earlier draft of this spec claimed a git repo was required; that was inferred from the `gitCommitSha` field and is wrong |
| C27 | Install **copies** to `~/.claude/plugins/cache/<marketplace>/<plugin>/<version>`. Source edits are inert until reinstall, and `claude plugin update` is a **no-op if `version` is unchanged**. | confirmed |
| C28 | Uninstall leaves the cache directory orphaned; it needs manual removal. | confirmed |
| C29 | `bb --config <abs>/bb.edn <task>` runs a task with `:paths` on the classpath, passes stdin through, and propagates `System/exit 2`. Startup 30–40 ms over 5 runs. | confirmed |
| C30 | `gh pr list --head "$(git rev-parse --abbrev-ref HEAD)" --state open --json number,isDraft` returns `[]` or the PR in **1.307 s**. | confirmed — measured |
| C31 | `gh` is authenticated as `yqrashawn` (`gist project read:org repo workflow`, ssh). A second account `Holybasil` carries an invalid token; non-interactive account resolution is untested. | confirmed / untested |
| C32 | Across 100 PRs in `claude-code-http-proxy`: 268 comments (`claude` 223, `yqrashawn` 45), 595 reviews. 44/100 PRs have >1 comment; 39/100 have >1 `claude` review. `track_progress` posts one new comment per pass. | confirmed |
| C33 | Observed GH-Action review durations: 1 m 52 s – **5 m 43 s** (343 s), median ≈ 3 m 12 s, n=11. | confirmed |
| C34 | `~/.nixpkgs/claude-code-plugins` exists, is empty, untracked, and wired to nothing. `install.sh` symlinks only `.doom.d`. | confirmed |
| C35 | Every other Claude asset (`~/.claude/{agents,skills,hooks,commands,settings.json,CLAUDE.md}`) is a symlink into Dropbox. A plugin in `~/.nixpkgs` propagates on `git pull` + rebuild instead. | confirmed |
| C36 | `--allowedTools` is a **pre-approval allowlist, not a tool restriction**. With `permissions.defaultMode: "bypassPermissions"` every tool is auto-approved regardless, so a reviewer launched with `--allowedTools "Read,Grep,Glob"` still had Bash and wrote outside the repo. Only `--disallowedTools` actually removes tools. | confirmed — reviewer wrote a file outside every repo; the spec's original R7 rationale was wrong |
| C37 | Neither the PreToolUse nor the PostToolUse payload carries the tool call's real working directory. Both carry only the **session** `cwd`; the hook process's own `PWD` is the session cwd too. Payload keys: `cwd`, `duration_ms`, `hook_event_name`, `permission_mode`, `prompt_id`, `scratchpad_dir`, `session_id`, `tool_input{command,description}`, `tool_name`, `tool_response`, `tool_use_id`, `transcript_path`. | confirmed — probed a real payload |
| C38 | `tool_use_id` appears in **both** the Pre and Post payloads and correlates, so a PreToolUse-written record can be read exactly by the matching PostToolUse. | confirmed |
| C39 | Two PreToolUse hooks both returning `updatedInput` contend: last writer wins. A plugin hook runs **before** settings hooks, so rtk always clobbers a plugin's rewrite. Only a settings-level wrapper that delegates to rtk can add to the command. | confirmed — measured the suffix vanishing |
| C40 | Introducing a `$(…)` command substitution into `updatedInput.command` makes Claude Code permission-check the new subcommands. In a session with `--permission-prompt-tool`, that invoked the tool with a payload it rejected and every push-shaped Bash call failed. A substitution-free recorder does not. | confirmed — reproduced and fixed |
| C41 | `vcs_state_changed` carries the true `cwd` but is a **stream-json system event**, not a hook event — no hook receives it. It is persisted only for sessions whose stream a supervisor consumes (130 rows over 62 sessions in cchp's DB), so it is not a universal source. | confirmed |
| C42 | On this workflow a PR is opened by **two** Bash calls: `git push` creates the branch (no PR exists yet, so the trigger is correctly silent), then a separate `gh pr create` opens it. `gh pr create` is therefore the critical trigger, and its `--body "$(cat <<'EOF' … EOF)"` carries a command substitution, a backtick and a heredoc. | confirmed — the two real commands, 19.9 s apart |
| C43 | A recorder spliced as `{ recorder; CMD; }` needs the push segment's **end**, which requires modelling everything to its right — so `$()`, backticks and heredocs must be refused, and C42's command with them. Splicing it **ahead** of the head as `{ recorder; } && CMD` needs only where the head begins, so the scan returns at the head and never reads what follows. | confirmed — 25 classification + 5 execution tests |
| C44 | The glue must be `&&`, not `;`. The real command is `cd "$WT" && gh pr create …`; with `;` a failed `cd` would no longer stop it and the PR would be opened from the session directory. The recorder ends in `\|\| :` so it can never itself stop a push. Splicing to the right of `\|` or `\|\|` changes the list instead (`(a \| recorder) && git push` loses the push's stdin), so those segments stay declined. | confirmed — negative control: `;` fails the cd test |
| C45 | rtk bails on any command containing a HEREDOC -- exit 0, zero bytes on stdout and stderr. Neither size, backticks nor `$(` change that. Since `--body "$(cat <<'EOF' ... EOF)"` is how a PR body is written, rtk is silent on essentially every PR-creation command. | confirmed — measured against the real 5808-byte command |
| C46 | The wrapper relays rtk's non-zero exits before its empty-stdout check, so only exit 0 reaches it, and a hook cannot deny silently (a deny is a non-zero exit or a `permissionDecision` in JSON). Empty stdout there therefore means rtk DECLINED, not that its meaning is ambiguous — the earlier "silence in, silence out" rule rested on an ambiguity that does not exist, and cost C45's trigger. | confirmed — corrected in 0.7.0 |
| C47 | R9's hint is written as `> "$(git rev-parse --git-common-dir)/pr-review-hint"`, and A naturally puts it in the SAME command as the push it hints about. A blanket `$(...)` refusal in the scanner therefore drops the record on precisely the hinted pushes. Skipping the substitution whole (depth-counted close) is required, not optional. | confirmed — observed on the PR #397 push |
| C48 | The whole command-reading approach is **withdrawn**. Git records every push in the remote-tracking reflog — old sha, new sha, branch, timestamp — written only on success, and `update by push` excludes every fetch (72 push against 297 fetch entries across 439 files in cchp). C42–C47 describe a mechanism that no longer exists; they stand as the record of why it was abandoned. | confirmed — both pushes the parser missed are plainly in the reflog |
| C49 | The one fact the reflog cannot supply is which clone to read, because the PostToolUse hook has no working directory of its own (C37). A `pre-push` hook records that and nothing else. It cannot describe a push that failed — the reflog gains no entry — and it is built so it cannot fail one: stdin spooled not consumed, every step guarded, status 0 unless a chained hook objects. | confirmed — 4 negative controls |
| C50 | `git rev-parse --git-path hooks` asked from a linked worktree answers the MAIN clone's hooks directory, so one install covers every worktree of a clone, and it honours `core.hooksPath` — which cchp sets explicitly, so assuming `<root>/.git/hooks` would have installed somewhere git never looks, silently. | confirmed — measured from wt-388-followup |
| C51 | A remote-tracking reflog lives exactly as long as its ref. Merging a PR deletes the branch, git prunes ref and reflog together, and the record disappears — precisely when there is nothing left to review. Measured on PR #396: merged, branch gone, reflog gone, while both open PRs kept theirs. | confirmed |
| C52 | `--disallowedTools` accepts COMMAND SHAPES, not only tool names, and they are enforced under `bypassPermissions`: a denied `rm` returned "Denied by user" while `git log` and a `printf >` ran in the same session. So Bash can be granted while specific harms stay closed — unlike `--allowedTools`, which restricts nothing (C36). | confirmed — measured both directions |
| C53 | A push made by the reviewer would carry the PARENT session's `CLAUDE_CODE_SESSION_ID`, because the reviewer is a grandchild of agent A's Bash call. The `pre-push` hook would record it as an agent push and the loop would review the reviewer's own commit. `Bash(git push:*)` and `Bash(git commit:*)` are denied for that reason, not for tidiness. | confirmed by construction — the env inheritance is measured (C52 session) |
| C54 | Granting Bash leaves two hazards open that no deny list closes: the production nREPL on port 8034 is reachable by any spelling `nc` does not cover, and the test suite can now be run, which may touch a shared database. The prompt asks the reviewer not to; that is guidance, not enforcement. | acknowledged, not mitigated |
| C55 | The review is posted by the TRIGGER, never by the reviewer. The reviewer generates its review once and the MUTATING `gh pr` subcommands stay denied to it, so it cannot comment, merge or close. Posting happens last — after the ledger row and the findings file are both on disk — so a GitHub failure costs the convenience of reading the review there and nothing else, and the wake says so rather than letting A assume the comment exists. | confirmed — 4 negative controls |
| C56 | The body travels on stdin via `gh pr comment --body-file -`: a review runs to tens of kilobytes and argv has a hard limit. Over GitHub's 65536-character body limit it is truncated with a pointer to the findings file, because a 422 would lose the comment entirely. | confirmed |
| C55b | The reviewer could not see the PR description by ANY route: not in the diff, not in the prompt, and `gh pr view` was refused because `Bash(gh pr:*)` is a PREFIX match that blocks reads as well as writes. So every review judged the change with no statement of intent to check it against — most of what `[docs-accuracy]` exists for. | confirmed — measured, "Refused." |
| C56b | `gh` READ is unrestricted and gh WRITE is not. A blanket `Bash(gh pr:*)` is a PREFIX match, so it blocked `gh pr view` — the read the prompt asks for — as well as the writes; removing it left nothing gh-shaped denied at all, while README and SKILL.md both told the author `gh pr` was refused. The mutating subcommands are denied by name now (merge, close, comment, review, edit, reopen, ready) plus `gh api`, which reaches anything the token reaches. | confirmed — the docs said one thing and `denied-tools` another |
| C57 | GitHub's PR head LAGS the push. Measured on PR #403: push recorded 20:25:31, PostToolUse trigger fired 20:25:32, and the PR's `updated_at` shows the head moved at 20:25:34 — with `gh pr list` taking ~1.3s on top, the query lands inside the propagation window. The sha-equality test (C48) then read a stale head, treated the mismatch as an answer, and dropped the push in silence; no lock was written, so the retry path had nothing to find either. | confirmed — measured against the GitHub API |
| C58 | That mismatch has two causes needing opposite handling: GitHub has not caught up (wait) or this push was superseded (drop). Only time separates them, so the PR head is re-asked with exponential backoff — 1s doubling to a cap of 8s, plus jitter, ~24s of total patience — then dropped. Unbounded retry hangs the hook: a negative control without the bound had to be killed. | confirmed — 6 negative controls |
| C59 | For a push to an EXISTING PR the first ask races **every time**, by construction: GitHub has to move the head. So the mismatch is the normal path, which rules out the obvious alternative of a fixed delay BEFORE asking — it pays its full cost on every trigger including those with nothing to review, and still drops the push whenever GitHub takes longer than the guess. Backing off costs nothing when the head is already right (`gh pr create`, or a late trigger) and returns as soon as GitHub catches up. Jitter is for concurrent pushes, which this workflow does constantly. | reasoned from C57's measurement |
| C60 | The reviewer is a plain `claude -p`, so it loads the user's settings AND every enabled plugin — including this one. Measured: a `review-trigger` process spawns inside the reviewer on its Bash calls, and since Bash was granted the reviewer uses them heavily (it runs test suites). Most such triggers go silent, but the abandoned-review path is deliberately neither session-scoped nor verb-gated (C-retry), so one can start a REVIEW INSIDE A REVIEW — a grandchild that dies when the outer reviewer exits, leaving a dead lock and a leaked worktree. | confirmed — a trigger process caught running inside a reviewer |
| C61 | Every name in the reviewer's deny list must be a tool Claude Code actually has. An unknown one prints `Permission deny rule "X" matches no known tool` to stderr — and stderr is the channel a KILLED review leaves its evidence on, so a stale name makes a dead reviewer look like it spoke. `MultiEdit` was 76 bytes of exactly that in every file, and guarded nothing once Bash was granted. A clean run now writes 0 bytes. | confirmed — measured against the real argv |
| C62 | Claude Code **enforces `timeout` on an `asyncRewake` hook**, unlike a plain `async` one where it explicitly does not, and the default for a `command` hook is **600 s**. `hooks.json` set none, so every review ran against a 600 s budget nobody chose — measured at 390–497 s from context diff to ledger row, plus setup, and growing because the reviewer runs test suites. | confirmed — hooks.md, "Run hooks in the background" |
| C63 | The documented contract is that an `asyncRewake` hook exiting 2 **wakes Claude immediately even when the session is idle** — the stated exception to async output waiting for the next turn. So a dropped wake is a violated precondition or a harness bug, NOT a property of the design. Three observed drops (#409, #412, and one more) remain unexplained; duration does not separate them, since a 495 s review was delivered and a 497 s one was not. | confirmed — hooks.md; drops unexplained |
| C64 | The hook payload's `cwd` **follows Claude** into a worktree and after a `cd`, per hooks.md — which contradicts C37, measured earlier as session-cwd-only. Not re-litigated: the reflog design does not read `cwd` at all and covers pushes made by any tool, not only a Bash `cd`. Worth knowing if `cwd` is ever wanted again. | doc says so; C37 was measured — unresolved conflict |

## Requirements

| # | Requirement | Done when |
|---|---|---|
| R1 | Fires on agent-A push to a PR branch and on agent-A PR creation | A pushes to a branch with an open PR; the ledger gains a pass entry. Both commands reduce to one rule — is there a push whose new sha is the head of an open PR with no ledger row (C48–C51) — so `gh pr create`, which pushes nothing, needs no special case |
| R2 | Never fires on a human push from a terminal with no Claude session | `git push` from a bare shell leaves the ledger unchanged |
| R3 | Works in any repo with no per-repo installation | The loop runs in a repo that has no `.claude/` directory at all |
| R4 | A is never blocked by the review | A's turn ends before the review does, measurable in the transcript |
| R5 | A cannot silently skip the review | The wake arrives without A choosing to act |
| R6 | Findings reach A even if A's turn already ended | Wake arrives while the session is idle (C2) |
| R7 | Reviewer has fresh context, cannot mutate the tree **that matters** | B is granted Bash on the author's instruction (C52), so this is no longer a tool restriction: containment is the throwaway detached worktree pinned to the reviewed sha, removed when the pass ends. Edit/Write stay denied but a shell writes files, so what actually holds is that B's tree is discarded and `git push`, `git commit` and the mutating `gh pr` subcommands are refused (C56b — `gh pr view` is not) |
| R8 | Reviewer sees the true, untruncated diff | The diff B reads is byte-identical to `git diff <base>...<head>` |
| R9 | A can pass a hint to the reviewer | A writes `.git/pr-review-hint`; that text appears in B's prompt |
| R10 | Drafts are reviewed | A PR with `isDraft: true` gets a pass entry |
| R11 | Pass number and re-raise ledger survive session restart and compaction | Ledger entries persist across `claude` restarts |
| R12 | Loop terminates | Pass 11 on one PR does not spawn a reviewer |
| R13 | Every review is readable on the PR, and one summary lands before merge | The TRIGGER posts each pass's findings as a PR comment (C55); agent A still posts the single pre-merge summary. Reverses the original "B never comments" rule, on the author's request: the ledger stays authoritative, but a review that can only be read in a wake or a file on one machine is not reviewable by a person |
| R14 | Two rapid pushes do not run two reviewers on stale SHAs | The older reviewer is killed; only the newer SHA is reviewed |
| R15 | Survives a nix rebuild, and a NEW machine | Activation installs the plugin cache when it does not already hold the source's version, and warns loudly when `bb`, `claude` or the manifest is missing. Everything else already travels: source by git, marketplace and `enabledPlugins` by the Dropbox symlinks, the `pre-push` hook by SessionStart |

## Options considered

### Where B runs

| Option | Verdict |
|---|---|
| Keep GitHub Actions | Rejected — the stated problem. |
| Buildkite agent (`machine-label: studio`/`mbpi` already exist) | Rejected — cannot attribute a push to agent A vs the human; GitHub events carry no such field. Same defect as any poller. |
| Symphony (`cchp`'s Clara rules engine, which already has `DispatchReviewRun`, `feedback-cycle`, `max-feedback-cycles`) | Rejected by the user — per-project registered, DB-backed, built for autonomous runs that own the PR; the requirement is a hand-driven A on arbitrary repos. Its convergence *semantics* are reused; its engine is not. |
| **Hook on A's machine** | **Chosen.** A's harness is the only place the "agent A did this" distinction exists. |

### What launches B

Initially specified as a subagent that A spawns. Reversed after C1/C2/C6 were measured.

| | Subagent | **Hook-spawned (chosen)** |
|---|---|---|
| A blocked per push | 2–6 min (C33) | 0 (C1) |
| A can skip it | yes — a nudge is advisory | no |
| Wakes A when idle | n/a | yes (C2) |
| A can hint B | direct prompt | via `.git/pr-review-hint` |
| B's own Bash | rtk-rewritten (C14) | irrelevant — B gets no Bash |

The one advantage of the subagent — a hint channel — costs one file. Everything
else favours the hook.

Steel-manning the subagent: it gives a typed return value, keeps everything in
one process tree, needs no lock file, and makes "A must not miss it" trivially
true because A is waiting. That version is genuinely simpler. It loses on R4
alone: 2–6 minutes of dead time on *every* push, drafts included, is a worse
tax than the whole locking mechanism it avoids.

### How B reads the code

| Option | Verdict |
|---|---|
| B runs `git diff` itself | **Rejected — C19.** Under rtk, B would see 195 bytes instead of 80 KB. Silent, catastrophic quality loss. |
| B runs `git diff`, spawned with `disableAllHooks: true` | Rejected — also disables the user's `block-suspicious-bash` guard for B, to buy back something option 3 gets for free. |
| **Hook precomputes context; B gets `Read`, `Grep`, `Glob` only** | **Chosen**, but the stated reason was wrong. The hook is a plain babashka process, so it is not rtk-rewritten (C16 applies to tool calls, not hook subprocesses), and it writes the true diff to a file. The claim that "B has no Bash" did **not** follow from `--allowedTools`: see C36. R7 is satisfied only by the deny list added later. |

Dropping Bash from B deletes two problems at once: the rtk truncation and the
write risk. It is strictly more capable than the GitHub Action reviewer, which
had only `gh pr diff` / `gh pr view` plus file reads.

### Pass-state store

| Option | Verdict |
|---|---|
| Derive from PR comments | Feasible (C32) but requires a body marker to separate B's comments from the human's 45, and a network round trip per read. |
| **`.git/pr-review-ledger.jsonl`** | **Chosen** by the user. No network, no identity problem, works before a PR exists, survives compaction and restarts, per clone. Precedent: Claude Code's own `.git/claude-trailers`, and `security-guidance`'s `.git/sg-reviewed-shas` (flocked, capped, append-only). Loses cross-machine visibility — accepted, since the loop is per-clone anyway. |

## Architecture

```
A (interactive Claude Code session, any repo, machine M)
  │
  │  Bash: git push  /  gh pr create
  ├──▶ PreToolUse  rtk-rewrite.sh          → `rtk git push …`   (C16)
  │
  └──▶ PostToolUse  if: 4 rules covering both shapes            (C7, C8, C23)
         asyncRewake: true, no timeout                          (C1, C6)
         command: bb --config ${CLAUDE_PLUGIN_ROOT}/bb.edn review-trigger   (C13, C29)
           │
           │ 1. parse hook stdin  → tool_use_id, cwd, tool_input.command
           │    effective dir: push record → command parse → session cwd  (C37, C38)
           │ 2. repo root, branch → gh pr list --head --state open   (C30)
           │      no open PR                       → exit 0, silent
           │ 3. ledger: passes for this PR ≥ 10    → exit 2 "cap reached"
           │ 4. lock .git/pr-review.<pr>.lock
           │      same SHA in flight               → exit 0, duplicate
           │      older SHA in flight              → kill it, take lock  (R14)
           │ 5. context: base SHA, full diff → .git/pr-review-context/<pr>-<sha>.diff
           │      changed-file list, PR metadata
           │ 6. prompt: review_core.md + <repo>/.claude/pr-review.md
           │            + .git/pr-review-hint + pass number + context paths
           │ 7. spawn B:  claude -p --model opus
           │              --disallowedTools <deny list>  --strict-mcp-config
           │              --allowedTools "Read,Grep,Glob"   (C36)
           │      capture stdout
           │ 8. ledger append: {pr, sha, pass, verdict, counts, fingerprints}
           │ 9. release lock
           │
           └─ findings → STDERR, exit 2                          (C4)
                │
                ▼
      A wakes — idle or mid-turn, minutes later                  (C2)
      skills/pr-review-loop drives: verify blocking → fix → push (loop)
                                  → merge + post summary comment (R13)
                                  → follow-ups on a new PR
```

## Contracts

**Trigger hook** (`pr-review.trigger/-main`)
- pre: `PostToolUse` fired for a `Bash` call that exited 0 (C9); stdin is hook JSON
- post: exit 0 and silence, or exit 2 with self-describing findings on stderr
- invariant: writes only under `<repo>/.git/pr-review*`; never mutates the work tree, the index, or any ref
- violation: a non-2, non-0 exit → Claude Code shows `Failed with non-blocking status code:` and the first stderr line, and the pass is lost. Blame: hook implementation.

**Findings payload**
- pre: exit code 2
- post: text on stderr, self-describing — the wrapper prefix is fixed and useless (C5), so the first line must name the repo, PR and pass
- invariant: nothing on stdout is ever seen (C4)

**Ledger** (`.git/pr-review-ledger.jsonl`)
- pre: `<repo>/.git` exists and is writable
- post: exactly one appended line per completed pass, under `flock`
- invariant: append-only; monotonic `pass` per `pr`; capped at 500 lines with locked read-GC-write
- violation: two concurrent appends interleaving → the cap and re-raise logic misread. Blame: missing lock.

**Lock** (`.git/pr-review.<pr>.lock`)
- pre: none
- post: held for the reviewer's lifetime, released on every exit path including error
- invariant: at most one live reviewer PER PR; the holder's SHA is always the newest pushed SHA for that PR. Per PR, not per repo, since C55b — two open PRs in one clone are independent work and one must not block the other
- violation: a stale lock from a killed process blocks all future reviews. Mitigation: the lock records a PID and is treated as free when that PID is dead.

**Reviewer** (B)
- pre: prompt names an existing diff file and an existing repo root; tools are `Read,Grep,Glob`
- post: stdout contains a `VERDICT:` line and categorised findings
- invariant: B cannot write, cannot run commands, cannot reach the network beyond the model API
- violation: no `VERDICT:` line → the trigger records `verdict: "MALFORMED"` and emits B's raw output, rather than dropping the pass silently.

## Termination

Ported from the current workflow prompt, which encodes a real incident: a
sibling repo produced twelve passes and ~40 `[correctness]` findings on one PR
without converging, roughly two thirds of them defects in fixes written to
satisfy the previous pass.

- Pass 1 = `FIRST`. When torn between blocking and follow-up, choose **blocking**.
- Pass ≥ 2 = `RE-REVIEW`. When torn, choose **follow-up**. Verify closure of the
  previous pass's blocking findings first, then sweep the new commits.
- A follow-up-grade finding may be re-raised **once**, then it stays on the list.
  Enforced by fingerprint (`<file>:<line>:<category>`) in the ledger.
- Hard cap: **10 passes per PR**. At the cap the trigger does not spawn B; it
  exits 2 with the unresolved count and hands control to the human.
- `MERGEABLE` means exactly: no `[correctness/blocking]`, and no `[coverage]`
  finding in which a test certifies a safety property it does not check.

## Prompt split

The current workflow prompt is roughly 90 % `cchp`-specific. The split line:

- **Generic core** — `hooks/review_core.md`, shipped with the plugin. The five
  categories, inline-vs-batch routing, `VERDICT:` format, evidence rules
  (traced vs inferred), the sibling-sweep *principle*, the FIRST/RE-REVIEW
  asymmetry and its rationale.
- **Per-repo overlay** — `<repo>/.claude/pr-review.md`, optional. The
  enumeration *recipe*. For `cchp`: `ig/init-key` keyword wiring, 168
  defmethods whose only caller is a keyword in `resources/system.edn`, "search
  the keyword, not the function name", Buildkite as the test gate.

The core says *sweep for siblings*; the overlay says *how to enumerate callers
in this codebase*. That is exactly the part that was never generic. Absent
overlay → core alone, degraded but functional (R3).

## Falsification conditions

The design is wrong if any of these turn out true:

| # | Would falsify | Test |
|---|---|---|
| F1 | An `asyncRewake` hook does **not** wake A when A is mid-turn on unrelated work (only idle was measured) | Trigger a 90 s async hook, then keep A busy for 3 min; check whether the wake arrives or is dropped |
| F2 | `if` rules miss a real agent-A push shape in practice | `RTK_HOOK_AUDIT=1` for a week; compare rewrite/skip counts against ledger passes |
| F3 | A reviewer with only `Read,Grep,Glob` cannot perform the sibling sweep | Run B against a known past PR whose review found a sibling defect; check it is still found |
| F4 | 10 passes is too low and real PRs hit the cap | Ledger shows cap-reached entries on PRs a human then merged without further findings |
| F5 | A repo-local `settings.local.json` rule flips the command shape and the four `if` rules miss it (C24) | Add a project `ask` rule for `Bash(git push:*)` and confirm a pass is still recorded |

## Risks

1. **Interactive sessions only.** C3 — a `claude -p` wrapper around A would silently review nothing. There is no warning for this.
2. **`if` is fail-open** (C7). The in-hook `gh pr list --head` gate is load-bearing, not redundant.
3. **rtk registry drift.** C17 is rtk 0.34.3, pinned in `common.nix:327`. A `nix flake update` can change which subcommands are rewritten, silently changing which `if` rule matches. C25 gives a way to detect it.
4. **Session restart** needed to pick up the plugin, and again after every `version` bump (C10, C27).
5. **B runs with A's full environment** — `.env.local` symlinks into Dropbox, fly tokens, prod credentials. Bounded by B having no Bash and no Write, but B can still `Read` any file under the repo root.
6. **F1 open.** If a mid-turn rewake interrupts rather than queues, it will derail A mid-edit. Not measured.
7. **Cross-machine invisibility.** The ledger is per clone (C35 + the `.git/` choice). A PR reviewed on the studio has no ledger on the laptop; a pass there restarts at 1.

## Out of scope

- Changing the finding taxonomy — ported verbatim.
- Replacing Buildkite as the test gate.
- `copilot-review.yml` — explicitly ignored.
- Making reviews faster.
- Removing `claude-code-review.yml`. It stays until the local loop is trusted;
  deleting it is a separate, reversible decision.
