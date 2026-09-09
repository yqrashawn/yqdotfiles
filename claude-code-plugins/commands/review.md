---
description: Review a PR now, without waiting for a push. Optional PR number; defaults to the current branch's PR.
---

Run the pr-review-loop reviewer against a pull request immediately.

Use this when the automatic loop did not, or could not, review something: a
review that was killed mid-run, a machine where the `pre-push` hook was not yet
installed, a push made before the plugin existed, or a PR opened by someone
else. It does not look for push evidence at all — it takes the PR as the
subject.

1. If the user gave a note for the reviewer, write it to the hint file first.
   Not a literal `.git/`, which is a *file* in a linked worktree:

   ```bash
   printf '%s\n' 'THE NOTE' > "$(git rev-parse --git-common-dir)/pr-review-hint"
   ```

2. Run it from inside the clone. `$CLAUDE_PLUGIN_ROOT` is set only for hooks
   declared in `hooks/hooks.json`, so it is empty here and the path is literal:

   ```bash
   bb --config ~/.nixpkgs/claude-code-plugins/bb.edn review-pr           # current branch
   bb --config ~/.nixpkgs/claude-code-plugins/bb.edn review-pr 401       # a named PR
   ```

   It is synchronous and takes several minutes — the reviewer runs tests. The
   findings come back on **stdout**, not through a wake, because there is no
   turn to wake. It always exits 0; read the output to see what happened.

   It writes a real ledger row and spends a real cap slot, exactly as the
   automatic path does, so the loop's convergence rules keep working. It
   refuses when the PR's head already has a row — push a commit to earn a new
   pass — and when the 10-pass cap is reached.

3. Then follow the pr-review-loop skill from step 1 of "On waking with
   findings": verify each `[correctness/blocking]` finding against the source
   before changing anything, push fixes onto **this** PR rather than a new one,
   and post the one summary comment before merging.
