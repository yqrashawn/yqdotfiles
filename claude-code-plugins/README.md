# claude-code-plugins

Claude Code plugins maintained in `~/.nixpkgs`. This directory doubles as a
local plugin marketplace. It is **not** a separate git repo — it is tracked
content of `~/.nixpkgs`, and a local-directory marketplace does not require a
repo of its own.

## Install

```bash
claude plugin marketplace add ~/.nixpkgs/claude-code-plugins
claude plugin install pr-review-loop@nixpkgs-plugins
```

Then start a **new** `claude` session — plugin hooks do not hot-load.

## Updating

`claude plugin update` is a no-op unless the version string changes. Bump
`version` in `.claude-plugin/plugin.json` **and** `plugin-version` in
`hooks/pr_review/version.clj` (a test enforces they agree), commit, then:

```bash
claude plugin uninstall pr-review-loop@nixpkgs-plugins
claude plugin install pr-review-loop@nixpkgs-plugins
```

Uninstall leaves `~/.claude/plugins/cache/nixpkgs-plugins` behind; remove it
by hand if you want it gone.

## Tests

```bash
cd ~/.nixpkgs/claude-code-plugins && bb test
```

## pr-review-loop

Reviews a PR in the background whenever this machine's Claude session pushes to
it, and wakes the session with the findings. See
`~/.nixpkgs/docs/superpowers/specs/2026-09-08-pr-review-loop.md`.

Per-repo state, all under the clone's *shared* git directory — what
`git rev-parse --git-common-dir` prints, which is `<repo>/.git` in an ordinary
clone and the main clone's `.git` from every linked worktree, so all worktrees
of one repository share one ledger, one lock and one cap. Safe to delete:

| Path | Purpose |
|---|---|
| `<git-common-dir>/pr-review-ledger.jsonl` | pass history, drives the 10-pass cap and the one-re-raise rule |
| `<git-common-dir>/pr-review.lock` | at most one live reviewer per clone |
| `<git-common-dir>/pr-review-context/<sha>.diff` | the untruncated diff the reviewer reads |
| `<git-common-dir>/pr-review-hint` | one-shot note to the next review; consumed on read |

Optional per-repo prompt overlay: `<repo>/.claude/pr-review.md`.

It only runs in **interactive** sessions. In `-p` mode Claude Code kills async
hooks at teardown, so nothing is reviewed and nothing warns you.
