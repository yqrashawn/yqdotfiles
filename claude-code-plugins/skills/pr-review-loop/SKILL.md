---
name: pr-review-loop
description: Use when a pr-review-loop finding arrives, or when the user asks to work the PR review loop. Drives verification of blocking findings, fixes, merge, and follow-up PRs.
---

# PR review loop

A background reviewer wakes you with findings. Your job is the loop, not the review.

## What each category obliges you to do

Only **correctness** findings are work you owe. The rest are offered, not
assigned — and treating them as assigned is what stops the loop converging: a
PR whose diff is mostly tests earns a fresh `[coverage]` nit every pass, and
coverage findings are never suppressed by the re-raise rules, so fixing them
one at a time never ends.

| category | what you owe |
|---|---|
| `[correctness/blocking]` | verify, then fix on this PR. The verdict is NOT MERGEABLE until it is closed |
| `[correctness/followup]` | real but bounded. Open a follow-up PR — do not fix it here |
| `[coverage]` | your judgment. See the carve-out below |
| `[docs-accuracy]`, `[style]` | your judgment. Fix it only if you independently agree STRONGLY that it is worth a commit; otherwise say you are dropping it and move on. No follow-up PR is owed |

**The one coverage carve-out.** A `[coverage]` finding saying a test certifies
a property it does not actually check is worth acting on even under a
MERGEABLE verdict. A test that cannot fail is worse than no test: it emits a
false safety signal, and the reviewer's own MERGEABLE definition is supposed
to exclude it. Every other coverage finding — a missing case, an untested
branch — is yours to weigh.

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

## When the verdict is MERGEABLE

1. Check CI. If Buildkite or another gate is configured and red, fix that first.
2. Merge.
3. Post one summary comment on the PR before or immediately after merging:
   passes run, findings by category, what was fixed, what was deferred. Read
   the pass history from `$(git rev-parse --git-common-dir)/pr-review-ledger.jsonl`.
4. If there are `[correctness/followup]` findings, open a **new** PR for them
   and let the loop run there. Verify each one before fixing it, same as above.
   `[coverage]`, `[docs-accuracy]` and `[style]` findings do not oblige a
   follow-up PR — carry forward only the ones you judged worth carrying.
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

## What the reviewer can and cannot do

It has `Read`, `Grep`, `Glob` **and `Bash`**, in a throwaway worktree checked
out at the reviewed commit and deleted afterwards, plus the full diff on disk.
So it can and does run test suites, `clj-kondo`, `git log`, and mutation
checks. Its output says which findings it verified by RUNNING something and
which are a reading of the source — trust that distinction and re-verify the
readings, not the runs.

It cannot alter the PR: `git push`, `git commit` and `gh pr` are refused, and
it has no network. Nothing it did to its own tree survives, so no finding is
ever accompanied by a fix.
