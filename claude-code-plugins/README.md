# pr-review-loop

An independent reviewer runs on every agent push to a PR, in a worktree pinned
to that commit, and wakes the pushing session with its findings.

## If it did not review something — check in this order

Each step's command answers one question. Stop at the first that is wrong.

```bash
R=/path/to/the/clone            # the clone that was PUSHED FROM
G=$(git -C "$R" rev-parse --path-format=absolute --git-common-dir)

# 1. Was the push recorded, and by an agent? (`-` in the 3rd field = a human)
tail -5 "${XDG_CACHE_HOME:-$HOME/.cache}/pr-review-loop/pushes.log"

# 2. Did git see it land? (no entry = the push was rejected)
tail -3 "$G/logs/refs/remotes/origin/<branch>"

# 3. Is the pushed sha the PR's head? (must match, or nothing fires)
gh pr view <N> --repo <owner/repo> --json headRefOid

# 4. Was it already reviewed? (a row for that sha means: correctly silent)
grep '<sha>' "$G/pr-review-ledger.jsonl"

# 5. Is a review running, or did one die? (a lock whose pid is dead = killed)
ls "$G"/pr-review.*.lock && cat "$G"/pr-review.*.lock
cat "$G/pr-review.<N>.stderr"          # what the reviewer said before dying
```

A killed review is retried by the next trigger in that clone. If nothing will
push again, review it by hand:

```bash
bb --config ~/.nixpkgs/claude-code-plugins/bb.edn review-pr <N>
```

Every pass is also posted to the PR as a comment, and left at
`$G/pr-review.<N>.findings.md` — so a review can be read on GitHub, or a
session that did not receive the wake can be pointed at that path. The
*trigger* posts it, never the reviewer: the reviewer generates its review once
and has `Bash(gh pr:*)` denied.

## How it decides

One question, for both `git push` and `gh pr create`:

> is there a push, **made by an agent** and **landed**, whose sha is the head
> of an open PR with **no ledger row**?

- **made by an agent** — the `pre-push` hook records `CLAUDE_CODE_SESSION_ID`;
  a human's terminal push records `-`. This is R2, and it is why `git -C /x
  push` and aliases work where matching the command text did not.
- **landed** — git's own remote-tracking reflog, written only on success.
- The command text is *not* consulted for anything but choosing the lookback
  window. Reading it to find the working directory produced five defects and
  still missed 25 of 260 real push commands; that whole approach is withdrawn.

## Where things are

| what | where |
|---|---|
| binding design, constraints C1–C54 | `docs/superpowers/specs/2026-09-08-pr-review-loop.md` |
| what agent A does with findings | `skills/pr-review-loop/SKILL.md` |
| reviewing by hand | `commands/review.md` |
| the reviewer's own prompt | `hooks/review_core.md`, plus `<repo>/.claude/pr-review.md` per repo |
| per-clone state | `<git-common-dir>/pr-review*` — ledger, locks, context diffs, findings, stderr |
| push provenance | `${XDG_CACHE_HOME:-~/.cache}/pr-review-loop/pushes.log` |
| the review, on GitHub | one comment per pass, posted by the trigger |

Namespaces, one line each — read the docstring, they carry the reasoning:

```
attempts    who pushed which clone, from the pre-push hook   (R2)
pushlog     what landed, from git's remote-tracking reflog
trigger     the decision, and the PostToolUse entrypoint
manual      the same review, for a PR named directly
checkout    the throwaway worktree pinned to the reviewed sha
reviewer    spawns claude -p, and parses its reply
ledger      passes, the 10-pass cap, and the re-raise rules
lock        one per PR; a dead pid means a killed review to retry
context     the unified diff the reviewer reads
hookinstall installs the pre-push hook, honouring core.hooksPath
```

## Invariants worth not breaking

- The `pre-push` hook must never fail a push. A non-zero exit aborts it.
- The trigger must exit **0 or 2**, never anything else — any other code makes
  Claude Code report a failed hook and the pass is lost.
- `--disallowedTools` is the reviewer's only real sandbox. `--allowedTools` is
  a pre-approval list and restricts nothing under `bypassPermissions`.
- Bash is granted to the reviewer; containment is the throwaway worktree, not
  the tool list. `git push`, `git commit`, `gh pr`, `rm`, `sudo`, `curl` are
  denied as command shapes.

`bb test` from this directory runs everything.
