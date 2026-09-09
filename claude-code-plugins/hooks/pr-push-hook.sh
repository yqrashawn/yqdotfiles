#!/usr/bin/env sh
# PR_REVIEW_LOOP_PRE_PUSH v1
#
# Records WHICH CLONE was pushed, and deliberately nothing else.
#
# What was pushed is read afterwards from git's own remote-tracking reflog
# (see pr-review.pushlog), which git writes only when a push SUCCEEDS. A
# pre-push hook runs BEFORE the push and cannot know that, so recording refs
# or shas here would mean recording pushes that were then rejected. The one
# fact the reflog cannot supply is which clone's .git to read, because the
# PostToolUse hook that later asks has no working directory of its own — so
# that is the one fact this hook writes.
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
    __prl_dir="${XDG_CACHE_HOME:-$HOME/.cache}/pr-review-loop"
    mkdir -p "$__prl_dir" \
      && printf '%s\t%s\n' "$(date +%s)" "$__prl_gcd" >> "$__prl_dir/pushes.log"
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
