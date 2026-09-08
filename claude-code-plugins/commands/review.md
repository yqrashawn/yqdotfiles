---
description: Run the PR reviewer on the current branch now, without waiting for a push.
---

Run the pr-review-loop reviewer against the current branch immediately.

1. Confirm the branch has an open PR:
   `gh pr list --head "$(git rev-parse --abbrev-ref HEAD)" --state open --json number,isDraft`
   If there is none, stop and say so.

2. If the user gave a note for the reviewer in their request, write it to
   `.git/pr-review-hint` first.

3. Invoke the trigger directly, feeding it the same JSON shape the hook feeds it.
   Note the literal path: `$CLAUDE_PLUGIN_ROOT` is set only for hooks declared in
   a plugin's `hooks/hooks.json`, so it is empty here.

   ```bash
   echo "{\"cwd\":\"$PWD\",\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"git push\"}}" \
     | bb --config ~/.nixpkgs/claude-code-plugins/bb.edn review-trigger
   ```

   It writes findings to stderr and exits 2. Read them from the command output
   rather than waiting for a wake — this path is synchronous.

4. Then follow the pr-review-loop skill from step 1 of "On waking with findings".
