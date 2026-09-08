---
name: pr-review-loop
description: Use when a pr-review-loop finding arrives, or when the user asks to work the PR review loop. Drives verification of blocking findings, fixes, merge, and follow-up PRs.
---

# PR review loop

A background reviewer wakes you with findings. Your job is the loop, not the review.

## On waking with findings

1. **Verify before fixing.** Reproduce each `[correctness/blocking]` finding
   against the source — read the cited `file:line`. A finding you cannot
   reproduce gets a note in your reply, not a fix. Reviewers over- and
   under-grade; re-derive severity yourself.
2. **Fix the class, not the instances.** A finding listing N bad inputs is
   evidence of a class. Name the invariant, derive the full violating input
   class, whitelist the valid class. Then grep for the *shape* of the defect,
   not the symbol name.
3. **Push the fixes to the same PR.** The push re-triggers the reviewer;
   pass N+1 verifies closure. Never open a new PR for review fixes — a new PR
   resets the pass counter and the loop never converges.
4. Ignore `[docs-accuracy]` and `[style]` until the PR is otherwise mergeable.

## When the verdict is MERGEABLE

1. Check CI. If Buildkite or another gate is configured and red, fix that first.
2. Merge.
3. Post one summary comment on the PR before or immediately after merging:
   passes run, findings by category, what was fixed, what was deferred. Read
   the pass history from `$(git rev-parse --git-common-dir)/pr-review-ledger.jsonl`.
4. If there are `[correctness/followup]` findings, open a **new** PR for them
   and let the loop run there. Verify each one before fixing it, same as above.
5. If there are none, the job is done. Say so.

## When the cap is reached

Ten passes have run on this PR. Do not push again expecting another review.
Summarise what is unresolved and hand the decision to the user.

## Hinting the reviewer

To tell the reviewer something before it runs, write it to
`$(git rev-parse --git-common-dir)/pr-review-hint` before pushing. It is
included in the next review's prompt and consumed — it applies to exactly one
pass. Use the command, not a literal `.git/`: in a linked worktree `.git` is a
file and nothing can live under it.

## What the reviewer cannot do

It has `Read`, `Grep`, `Glob` and the full diff on disk. No shell, no tests,
no REPL. If a finding depends on runtime behaviour it will say so, and
verifying that is your job.
