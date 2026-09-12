#!/usr/bin/env sh
# PR_REVIEW_LOOP_PRE_PUSH v1
#
# Records which clone was pushed, who pushed it, and which refs were attempted.
#
# These are ATTEMPTS, not outcomes: a pre-push hook runs BEFORE the push and
# cannot know it succeeded. What actually LANDED is read afterwards from git's
# own remote-tracking reflog (see pr-review.pushlog), which git writes only on
# success, and a candidate must appear in both at the same sha. So a rejected
# push leaves a line here and no reflog entry, and is correctly never reviewed.
#
# The two facts the reflog cannot supply are which clone's .git to read — the
# PostToolUse hook that later asks has no working directory of its own — and
# who pushed, which is what keeps a human's terminal push out of the loop.
#
# IT MUST NEVER FAIL A PUSH. A non-zero pre-push aborts the push, so every
# step is guarded and the status is 0 unless a chained hook says otherwise.

set -u

# git writes '<local ref> <local sha> <remote ref> <remote sha>' lines here.
# This hook has no use for them, but a chained hook will, and stdin can only
# be read once — so it is spooled rather than consumed.
__prl_spool=$(mktemp 2>/dev/null) || __prl_spool="${TMPDIR:-/tmp}/prl-prepush.$$"
cat > "$__prl_spool" 2>/dev/null || :

{
  # --path-format=absolute needs git 2.31; fall back for older ones, since the
  # loop is meant to run on whichever machine the agent is on.
  __prl_gcd=$(git rev-parse --path-format=absolute --git-common-dir 2>/dev/null) || __prl_gcd=''
  if [ -z "$__prl_gcd" ]; then
    __prl_rel=$(git rev-parse --git-common-dir 2>/dev/null) || __prl_rel=''
    [ -n "$__prl_rel" ] && __prl_gcd=$(cd "$__prl_rel" 2>/dev/null && pwd)
  fi
  if [ -n "$__prl_gcd" ]; then
    # PROVENANCE. Claude Code exports CLAUDE_CODE_SESSION_ID into a Bash tool
    # call's environment and every child inherits it, so a push made by an
    # agent carries the session id and a push made by a human in a terminal
    # carries none. That is R2 -- "never fires on a human push" -- decided by
    # what actually made the push rather than by how the command was spelled,
    # which also catches `git -C /x push`, aliases and scripts that no
    # command-text gate can see. `-` keeps the field count fixed.
    __prl_sid="${CLAUDE_CODE_SESSION_ID:--}"
    __prl_dir="${XDG_CACHE_HOME:-$HOME/.cache}/pr-review-loop"
    __prl_now=$(date +%s)
    if mkdir -p "$__prl_dir"; then
      printf '%s\t%s\t%s\n' "$__prl_now" "$__prl_gcd" "$__prl_sid" \
        >> "$__prl_dir/pushes.log"
      # One line per ref being pushed: '<local ref> <local sha> <remote ref>
      # <remote sha>'. This is the ATTEMPT, not the outcome -- a pre-push hook
      # runs before the push and cannot know it succeeded -- so the reflog
      # stays the proof that it landed, and these lines only say who tried and
      # at which sha. A deletion pushes the all-zero sha and is skipped.
      while read -r __prl_lref __prl_lsha __prl_rref __prl_rsha; do
        [ -n "${__prl_rref:-}" ] || continue
        case "${__prl_lsha:-}" in
          '' | *[!0-9a-f]* | 0000000000000000000000000000000000000000 ) continue ;;
        esac
        printf '%s\t%s\t%s\t%s\t%s\n' \
          "$__prl_now" "$__prl_gcd" "$__prl_sid" "$__prl_rref" "$__prl_lsha" \
          >> "$__prl_dir/pushes.log"
      done < "$__prl_spool"
    fi
  fi
} >/dev/null 2>&1 || :

# A hook that was already here was moved aside at install time, and keeps its
# veto: its stdin is replayed and its exit status relayed untouched.
__prl_chained="$(dirname "$0")/pre-push.pr-review-chained"
if [ -x "$__prl_chained" ]; then
  "$__prl_chained" "$@" < "$__prl_spool"
  __prl_rc=$?
  rm -f "$__prl_spool" 2>/dev/null || :
  exit "$__prl_rc"
fi

rm -f "$__prl_spool" 2>/dev/null || :
exit 0
