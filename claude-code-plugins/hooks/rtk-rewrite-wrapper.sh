#!/usr/bin/env bash
# pr-review-loop PreToolUse wrapper around rtk-rewrite.sh
# ---------------------------------------------------------------------------
# WHY THIS EXISTS
#   The PostToolUse payload carries only the SESSION cwd, never the directory a
#   `git push` actually ran in (agents routinely do `cd "$SP/wt" && git push`).
#   The only universal way to learn the real directory is to make the shell
#   itself report it -- i.e. edit the command via `updatedInput`.
#   `updatedInput` is single-owner in practice: rtk-rewrite.sh already emits it
#   and rewrites from the ORIGINAL command, so a second hook's edit is
#   clobbered. Hence: wrap rtk, delegate to it, then edit whatever it produced.
#
# HOW (v2 -- "wrap the push", not "append to the command")
#   The push subcommand is located and replaced IN PLACE by a brace group:
#
#       cd /x && git push -u origin b
#       cd /x && { recorder; git push -u origin b; }
#
#   The recorder therefore runs AT the push, inside whatever subshell or
#   background job the push is in, so `(cd /x && git push)` and
#   `cd /x && git push &` both record /x. A `{ a; b; }` group's exit status is
#   already `b`'s, so no $? capture/restore is needed, and because nothing is
#   appended after the command there is no heredoc-vs-newline problem either.
#
# CONTRACT OF THE WRAPPED HOOK (~/.claude/hooks/rtk-rewrite.sh)
#   Always exits 0. stdout is either empty (passthrough / deny / no jq / no rtk)
#   or JSON with .hookSpecificOutput.updatedInput.command, plus
#   permissionDecision:"allow" (rtk exit 0) or nothing (rtk exit 3 = "ask").
#   That file is REGENERATED AND HASH-VERIFIED by `rtk init --global
#   --auto-patch` on every nix rebuild -- never patch it. This wrapper is a
#   sibling file; siblings are not touched by rtk init.
#
# SCOPE
#   Only a `git push` / `gh pr create` simple command (bare or `rtk`-prefixed)
#   is wrapped. Every other Bash command is passed through with rtk's own
#   output byte-identical, so nothing else on the machine changes shape.
#
# FAIL-OPEN INVARIANT
#   Every failure path degrades to "behave exactly like rtk alone": echo rtk's
#   stdout verbatim (possibly empty) and exit 0. This hook runs on EVERY Bash
#   tool call in EVERY session on this machine; it must never block a command,
#   never change a command's meaning, and never exit non-zero.
#
#   A MISSING RECORD IS HARMLESS -- the consumer (pr-review.pushrecord) falls
#   back to command parsing and then to the session cwd. A BROKEN COMMAND IS
#   NOT. So the scanner below models a deliberately small subset of shell
#   grammar exactly and REFUSES everything else (see `scan_segments`).
#
# TRACKED SOURCE
#   ~/.nixpkgs/claude-code-plugins/hooks/rtk-rewrite-wrapper.sh, installed to
#   ~/.claude/hooks/ by modules/yqrashawn/home-manager/activations/default.nix,
#   which also re-points the settings.json PreToolUse entry at this file after
#   `rtk init` re-registers its own.
# ---------------------------------------------------------------------------

set -u

# `${HOME:-}`, not `$HOME`: under `set -u` an environment without HOME would
# otherwise abort the hook with a non-zero exit and a stderr splat -- the one
# thing the fail-open invariant forbids. An unreadable RTK_HOOK is handled
# below, so a wrong path here degrades instead of exploding.
RTK_HOOK=${PRL_RTK_HOOK:-${HOME:-}/.claude/hooks/rtk-rewrite.sh}

# The recorder, as shell source. Single-quoted: ${TMPDIR} and $PWD must be
# resolved by the shell that runs the push, not by this hook process. @ID@ is
# replaced with the tool_use_id, which is whitelisted to [A-Za-z0-9_-] before
# it is ever interpolated.
#   * a subshell, so __prl_d does not leak into the caller's environment
#   * </dev/null so it can never eat stdin the push was going to read
#   * >/dev/null 2>&1 so it can never write into a pipe the push feeds
#   * || : so a failing recorder cannot abort the group under `set -e`
# `__prl_d` doubles as the idempotency marker (see the guard below).
#
# NO COMMAND SUBSTITUTION, AND NO BACKTICKS -- this is a hard constraint, not a
# style choice. Claude Code's permission analysis inspects the commands inside
# `$(...)` and backticks, so a substitution here becomes an extra subcommand to
# permission-check on a command the user never wrote; in a session configured
# with `--permission-prompt-tool` that was observed to fail the Bash call
# outright. `${TMPDIR:-/tmp}` and `"$PWD"` are parameter expansions, not
# commands, and are fine. Earlier revisions also recorded `branch=` (via
# `$(git rev-parse ...)`) and `ts=` (via `$(date ...)`); both were dead weight
# -- `pr-review.trigger` derives the branch itself with `gh/current-branch`,
# and `prune-records!` ages records off their file mtime -- so the record is
# now the one field the consumer actually reads, plus the key it is filed
# under. `pr-review.pushrecord` drops absent keys, so this stays compatible.
# shellcheck disable=SC2016
RECORDER_TMPL='( __prl_d="${TMPDIR:-/tmp}/pr-review-pushdir"; mkdir -p "$__prl_d" && printf '\''tool_use_id=%s\npwd=%s\n'\'' '\''@ID@'\'' "$PWD" >"$__prl_d/@ID@" ) </dev/null >/dev/null 2>&1 || :'

# --- 1. read the payload; keep it byte-for-byte for the delegate ------------
INPUT=$(cat 2>/dev/null) || exit 0

# --- 2. delegate to rtk-rewrite.sh, payload unchanged on its stdin ----------
# stderr is deliberately NOT captured, so rtk's warnings surface exactly as
# they do today.
RTK_OUT=""
RTK_RC=0
if [ -r "$RTK_HOOK" ]; then
  RTK_OUT=$(printf '%s' "$INPUT" | "${BASH:-bash}" "$RTK_HOOK") || RTK_RC=$?
fi

# The delegate's exit code is RELAYED, never swallowed. rtk-rewrite.sh always
# exits 0 today, but PreToolUse exit 2 means "block this tool call": if a future
# rtk hook ever blocks, eating its status here would silently UN-block the
# command. Relaying is what "behave exactly like rtk alone" means, and it is
# the only path on which this wrapper exits non-zero.
if [ "$RTK_RC" -ne 0 ]; then
  [ -n "$RTK_OUT" ] && printf '%s\n' "$RTK_OUT"
  exit "$RTK_RC"
fi

# Any early return from here on must reproduce rtk-alone behaviour exactly.
passthrough() { [ -n "$RTK_OUT" ] && printf '%s\n' "$RTK_OUT"; exit 0; }

command -v jq >/dev/null 2>&1 || passthrough

# --- 3. parse payload + rtk output; decide the base command ----------------
printf '%s' "$INPUT" | jq -e . >/dev/null 2>&1 || passthrough

TOOL_NAME=$(printf '%s' "$INPUT" | jq -r '.tool_name // empty' 2>/dev/null)
[ "$TOOL_NAME" = "Bash" ] || [ -z "$TOOL_NAME" ] || passthrough

TOOL_USE_ID=$(printf '%s' "$INPUT" | jq -r '.tool_use_id // empty' 2>/dev/null)
# Interpolated into a shell command *and* a filename: hard-whitelist it.
case $TOOL_USE_ID in
  '' | *[!A-Za-z0-9_-]* ) passthrough ;;
esac
[ ${#TOOL_USE_ID} -le 128 ] || passthrough

RTK_JSON=null
if [ -n "$RTK_OUT" ]; then
  # Non-empty but non-JSON stdout from rtk: unexpected -> hands off entirely.
  printf '%s' "$RTK_OUT" | jq -e . >/dev/null 2>&1 || passthrough
  RTK_JSON=$RTK_OUT
fi

BASE=$(printf '%s' "$RTK_JSON" | jq -r '.hookSpecificOutput.updatedInput.command // empty' 2>/dev/null)
[ -n "$BASE" ] || BASE=$(printf '%s' "$INPUT" | jq -r '.tool_input.command // empty' 2>/dev/null)
[ -n "$BASE" ] || passthrough

# --- 3a. cheap pre-filter --------------------------------------------------
# The scanner below is precise but forks `bash -n`; this keeps it off the
# ~all-commands path. Two bare words, so `git   push` (any run of blanks, which
# a `"git push"` substring test would miss) still reaches the scanner. Quoting
# is not analysed and over-matching costs nothing -- the scanner re-decides
# properly and refuses everything it cannot place exactly.
case $BASE in
  *push* | *create* ) ;;
  * ) passthrough ;;
esac

# Idempotency: never wrap a command that already carries a recorder.
case $BASE in *__prl_d* ) passthrough ;; esac

# Cheap sanity bounds. A trailing ODD run of backslashes means the command ends
# mid-escape; nothing sensible can be spliced into it.
[ ${#BASE} -le 65536 ] || passthrough
TRAIL=${BASE##*[!\\]}
if [ -n "$TRAIL" ] && [ $(( ${#TRAIL} % 2 )) -eq 1 ]; then passthrough; fi

# The base must parse, and parse SILENTLY (a bare warning from `bash -n` means
# an unterminated heredoc, which the scanner also refuses).
parses_clean() {
  local err
  err=$(printf '%s\n' "$1" | bash -n 2>&1) || return 1
  [ -z "$err" ]
}
parses_clean "$BASE" || passthrough

# --- 4. boundary scanner ---------------------------------------------------
# Splits the command into simple-command SEGMENTS at unquoted control
# operators, and REFUSES (returns 1) on any construct it does not model
# exactly. Segment boundaries are: ; & | newline ( ) `{ ` and an unquoted
# comment, i.e. exactly the places a simple command can end.
#
# Modelled: '...', "..." (with \ escapes and ${...} inside), \ escapes,
#   ${...}, <<< herestrings, >& <& &> >| redirections, # comments.
# REFUSED outright, because getting them wrong corrupts a command:
#   $(...)  `...`   -- command substitution: needs a nesting stack
#   <<EOF   <<-EOF  -- heredocs: the body is arbitrary text that would be
#                      scanned as if it were code
#   >(...)  <(...)  -- process substitution
#   an unterminated quote or ${
# Each refusal costs at most one missing record.
SEG_START=(); SEG_END=()
scan_segments() {
  # Separate `local` statements: bash expands ALL arguments of the builtin
  # before assigning any of them, so `local s=$1 n=${#s}` would read the
  # OUTER s (and, under `set -u`, kill the hook).
  local s=$1
  local n=${#s}
  local i=0 c nx pv wordstart=1 start=0
  SEG_START=(); SEG_END=()
  while [ "$i" -lt "$n" ]; do
    c=${s:i:1}
    nx=${s:i+1:1}
    pv=''; [ "$i" -gt 0 ] && pv=${s:i-1:1}
    case $c in
      '\')
        i=$((i+2)); wordstart=0 ;;
      "'")
        i=$((i+1))
        while [ "$i" -lt "$n" ] && [ "${s:i:1}" != "'" ]; do i=$((i+1)); done
        [ "$i" -lt "$n" ] || return 1
        i=$((i+1)); wordstart=0 ;;
      '"')
        i=$((i+1))
        while [ "$i" -lt "$n" ]; do
          case ${s:i:1} in
            '\') i=$((i+2)) ;;
            '`') return 1 ;;
            '$') [ "${s:i+1:1}" = '(' ] && return 1
                 i=$((i+1)) ;;
            '"') break ;;
            *)   i=$((i+1)) ;;
          esac
        done
        [ "$i" -lt "$n" ] || return 1
        i=$((i+1)); wordstart=0 ;;
      '`')
        return 1 ;;
      '$')
        case $nx in
          '(') return 1 ;;
          '{') i=$((i+2))
               while [ "$i" -lt "$n" ] && [ "${s:i:1}" != '}' ]; do i=$((i+1)); done
               [ "$i" -lt "$n" ] || return 1
               i=$((i+1)) ;;
          *)   i=$((i+1)) ;;
        esac
        wordstart=0 ;;
      '<')
        if [ "$nx" = '<' ]; then
          [ "${s:i+2:1}" = '<' ] || return 1   # heredoc, not a herestring
          i=$((i+3))
        else
          i=$((i+1))
        fi
        wordstart=0 ;;
      '#')
        if [ "$wordstart" -eq 1 ]; then
          SEG_START+=("$start"); SEG_END+=("$i")
          while [ "$i" -lt "$n" ] && [ "${s:i:1}" != $'\n' ]; do i=$((i+1)); done
          [ "$i" -lt "$n" ] && i=$((i+1))
          start=$i; wordstart=1
        else
          i=$((i+1)); wordstart=0
        fi ;;
      '(' | ')')
        # `>(` / `<(` is process substitution, not a subshell.
        { [ "$pv" = '>' ] || [ "$pv" = '<' ]; } && return 1
        SEG_START+=("$start"); SEG_END+=("$i")
        i=$((i+1)); start=$i; wordstart=1 ;;
      '&')
        if [ "$pv" = '>' ] || [ "$pv" = '<' ] || [ "$nx" = '>' ]; then
          i=$((i+1)); wordstart=0            # 2>&1, >&2, &>file
        else
          SEG_START+=("$start"); SEG_END+=("$i")
          i=$((i+1)); start=$i; wordstart=1
        fi ;;
      '|')
        if [ "$pv" = '>' ]; then
          i=$((i+1)); wordstart=0            # >|file
        else
          SEG_START+=("$start"); SEG_END+=("$i")
          i=$((i+1)); start=$i; wordstart=1
        fi ;;
      '{')
        # Only the group-open reserved word splits; `--x={a,b}` must not.
        if [ "$wordstart" -eq 1 ] && { [ "$nx" = ' ' ] || [ "$nx" = $'\t' ] || [ "$nx" = $'\n' ]; }; then
          SEG_START+=("$start"); SEG_END+=("$i")
          i=$((i+1)); start=$i; wordstart=1
        else
          i=$((i+1)); wordstart=0
        fi ;;
      ';' | $'\n')
        SEG_START+=("$start"); SEG_END+=("$i")
        i=$((i+1)); start=$i; wordstart=1 ;;
      ' ' | $'\t')
        i=$((i+1)); wordstart=1 ;;
      *)
        i=$((i+1)); wordstart=0 ;;
    esac
  done
  SEG_START+=("$start"); SEG_END+=("$n")
  return 0
}

# `git push` / `gh pr create`, bare or rtk-prefixed, as the HEAD of the
# segment. Anything between the words but blanks (`env X=1 git push`,
# `then git push`, `git -C /x push`) deliberately does not match: those are
# either unwrappable or would need a real parser to place correctly.
is_push_head() {
  [[ $1 =~ ^(rtk[[:blank:]]+)?git[[:blank:]]+push([[:blank:]]|$) ]] && return 0
  [[ $1 =~ ^(rtk[[:blank:]]+)?gh[[:blank:]]+pr[[:blank:]]+create([[:blank:]]|$) ]] && return 0
  return 1
}

scan_segments "$BASE" || passthrough

RECORDER=${RECORDER_TMPL//@ID@/$TOOL_USE_ID}

# Right to left, so an earlier segment's offsets are still valid after a later
# one has been rewritten. Two pushes in one command therefore both get wrapped.
NEW=$BASE
WRAPPED=0
k=$(( ${#SEG_START[@]} - 1 ))
while [ "$k" -ge 0 ]; do
  st=${SEG_START[$k]}
  en=${SEG_END[$k]}
  seg=${BASE:st:en-st}
  lead=${seg%%[![:blank:]]*}
  head=$(( st + ${#lead} ))
  if is_push_head "${seg:${#lead}}"; then
    NEW="${NEW:0:head}{ ${RECORDER}; ${NEW:head:en-head}; } ${NEW:en}"
    WRAPPED=1
  fi
  k=$((k-1))
done

[ "$WRAPPED" -eq 1 ] || passthrough

# Belt and braces: if the rewrite does not parse cleanly, ship rtk's output
# alone. This is the net under every scanner assumption above.
parses_clean "$NEW" || passthrough

# --- 5. emit merged JSON ---------------------------------------------------
# updatedInput keeps every other tool_input field (description, timeout, ...).
#
# permissionDecision is rtk's when rtk set one, and "allow" otherwise -- rtk
# only ever sets "allow", so in practice a command this wrapper rewrites is
# ALWAYS allowed. That is a deliberate change, authorised by the user, and it
# is the whole point: rtk emits nothing at all for a command that is already
# `rtk git push`, and an ABSENT decision makes Claude Code run its permission
# flow, which in a session started with `--permission-prompt-tool` invokes that
# tool and was observed to fail the Bash call outright. A rewritten command the
# user never typed should not be the thing that triggers a prompt.
#
# THE ONE THING THIS GIVES UP: rtk signals "a deny rule matched" by emitting
# nothing (its exit 2), which is indistinguishable here from "no rewrite
# available" (its exit 1). So a deny rule matching a `git push` / `gh pr
# create` command would now be overridden by this "allow". Measured before
# making the change: permissions.deny and permissions.ask are both empty and
# defaultMode is bypassPermissions, so nothing is being bypassed today. If deny
# rules are ever added for a push-shaped command, this line must go back to
# copying the decision only when rtk set one.
#
# This only affects commands the wrapper actually WRAPS. Every other command
# returns rtk's stdout byte-identical long before reaching this point.
OUT=$(jq -n \
  --argjson payload "$INPUT" \
  --argjson rtk "$RTK_JSON" \
  --arg cmd "$NEW" '
  (($rtk // {}).hookSpecificOutput // {}) as $h
  | (($h.updatedInput // $payload.tool_input // {}) | .command = $cmd) as $ui
  | { hookSpecificOutput:
        { hookEventName: "PreToolUse",
          permissionDecision: ($h.permissionDecision // "allow"),
          permissionDecisionReason:
            ($h.permissionDecisionReason // "pr-review-loop push recorder"),
          updatedInput: $ui } }
' 2>/dev/null) || passthrough
[ -n "$OUT" ] || passthrough

printf '%s\n' "$OUT"
exit 0
