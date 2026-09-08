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

## Requirements

| # | Requirement | Done when |
|---|---|---|
| R1 | Fires on agent-A push to a PR branch and on agent-A PR creation | A pushes to a branch with an open PR; the ledger gains a pass entry |
| R2 | Never fires on a human push from a terminal with no Claude session | `git push` from a bare shell leaves the ledger unchanged |
| R3 | Works in any repo with no per-repo installation | The loop runs in a repo that has no `.claude/` directory at all |
| R4 | A is never blocked by the review | A's turn ends before the review does, measurable in the transcript |
| R5 | A cannot silently skip the review | The wake arrives without A choosing to act |
| R6 | Findings reach A even if A's turn already ended | Wake arrives while the session is idle (C2) |
| R7 | Reviewer has fresh context, cannot mutate the tree | B's tool list contains no Edit/Write/Bash |
| R8 | Reviewer sees the true, untruncated diff | The diff B reads is byte-identical to `git diff <base>...<head>` |
| R9 | A can pass a hint to the reviewer | A writes `.git/pr-review-hint`; that text appears in B's prompt |
| R10 | Drafts are reviewed | A PR with `isDraft: true` gets a pass entry |
| R11 | Pass number and re-raise ledger survive session restart and compaction | Ledger entries persist across `claude` restarts |
| R12 | Loop terminates | Pass 11 on one PR does not spawn a reviewer |
| R13 | One summary comment lands on the PR before merge | The merged PR has exactly one `pr-review-loop` summary comment |
| R14 | Two rapid pushes do not run two reviewers on stale SHAs | The older reviewer is killed; only the newer SHA is reviewed |
| R15 | Survives a nix rebuild | `rtk init --auto-patch` runs; the plugin still triggers (guaranteed by C22) |

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
| **Hook precomputes context; B gets `Read`, `Grep`, `Glob` only** | **Chosen.** The hook is a plain babashka process, so it is not rtk-rewritten (C16 applies to tool calls, not hook subprocesses). It writes the true diff to a file. B has no Bash, so there is nothing to rewrite and nothing that can mutate the tree. Satisfies R7 and R8 with one decision. |

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
           │ 1. parse hook stdin  → cwd, tool_input.command
           │ 2. repo root, branch → gh pr list --head --state open   (C30)
           │      no open PR                       → exit 0, silent
           │ 3. ledger: passes for this PR ≥ 10    → exit 2 "cap reached"
           │ 4. lock .git/pr-review.lock
           │      same SHA in flight               → exit 0, duplicate
           │      older SHA in flight              → kill it, take lock  (R14)
           │ 5. context: base SHA, full diff → .git/pr-review-context/<sha>.diff
           │      changed-file list, PR metadata
           │ 6. prompt: review_core.md + <repo>/.claude/pr-review.md
           │            + .git/pr-review-hint + pass number + context paths
           │ 7. spawn B:  claude -p --model opus
           │              --allowedTools "Read,Grep,Glob"
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

**Lock** (`.git/pr-review.lock`)
- pre: none
- post: held for the reviewer's lifetime, released on every exit path including error
- invariant: at most one live reviewer per repo; the holder's SHA is always the newest pushed SHA
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
