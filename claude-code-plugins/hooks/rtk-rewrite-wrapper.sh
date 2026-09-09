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
#   grammar exactly and REFUSES everything else (see `find_insert_point`).
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

# rtk emitting NOTHING, having exited 0, is rtk DECLINING to rewrite: the
# command will run exactly as the user wrote it. That is a placeable case, so
# the recorder is still spliced in -- the wrapper just has no rtk output to
# merge with.
#
# An earlier revision returned silence for silence here, on the stated grounds
# that empty stdout was ambiguous between "no rtk equivalent" (exit 1), "a deny
# rule matched" (exit 2) and "already in rtk form". That reasoning was wrong:
# non-zero exits are RELAYED above and never reach this line, so only exit 0
# arrives, and a hook cannot deny silently -- a deny is either a non-zero exit
# or a `permissionDecision` in JSON, and both are handled elsewhere.
#
# The cost of being wrong about that was the whole feature: rtk bails on any
# command containing a HEREDOC, and `gh pr create --body "$(cat <<EOF ...)"` is
# how a PR body is written. So rtk is silent on essentially every real
# PR-creation command -- the one command the loop most needs recorded.
#
# What this does NOT do is supply a permissionDecision (see step 5). Claude Code
# therefore permission-checks the rewritten command. Relative to declining, the
# only new subcommands it sees are the recorder's `mkdir` and `printf`; the
# command being checked is otherwise the one it was about to run anyway. The
# failure mode is over-blocking, never a widened permission.
RTK_SILENT=0
[ -n "$RTK_OUT" ] || RTK_SILENT=1

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

# Non-empty but non-JSON stdout from rtk: unexpected -> hands off entirely.
# When rtk declined outright there is nothing to merge, and `{}` makes step 5's
# `// {}` fallbacks resolve to the payload's own tool_input.
if [ "$RTK_SILENT" -eq 1 ]; then
  RTK_JSON='{}'
else
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

# --- 4. head locator -------------------------------------------------------
# Walks the command left to right and stops at the FIRST segment whose head is
# a wrappable push, reporting where that head begins. Segment boundaries are
# the places a simple command can end: ; & | newline ( ) `{ ` and an unquoted
# comment.
#
# Only the text BEFORE that head is ever analysed -- the walk returns the
# instant it finds one. That is what lets
# `gh pr create --body "$(cat <<'EOF' ...)"` be recorded: its command
# substitution, backtick and heredoc all sit to the RIGHT of the head, where
# nothing needs to be understood. An earlier revision spliced the recorder in
# as `{ recorder; CMD; }`, which needed the segment's END as well and so had
# to model those constructs; it refused them, and therefore refused the one
# command that actually opens a PR.
#
# Modelled: '...', "..." (with \ escapes and ${...} inside), \ escapes,
#   ${...}, <<< herestrings, >& <& &> >| redirections, # comments.
# REFUSED, because misplacing a boundary corrupts a command:
#   $(...)  `...`   -- command substitution: needs a nesting stack
#   <<EOF   <<-EOF  -- heredocs: the body is arbitrary text that would be
#                      scanned as if it were code
#   >(...)  <(...)  -- process substitution
#   an unterminated quote or ${
# Each refusal costs at most one missing record.
#
# The delimiter that OPENS a segment is classified too, because the recorder
# becomes a new element of the enclosing list. After `;`, a newline, `&&`, a
# background `&`, `(` or `{ ` that is transparent. After `|` or `||` it is
# NOT: `a | git push` would become `(a | recorder) && git push` and the push
# would lose its stdin. Such segments are skipped -- a later one may match.
HEAD_POS=-1
find_insert_point() {
  # Separate `local` statements: bash expands ALL arguments of the builtin
  # before assigning any of them, so `local s=$1 n=${#s}` would read the
  # OUTER s (and, under `set -u`, kill the hook).
  local s=$1
  local n=${#s}
  local i=0 c nx pv wordstart=1 start=0 safe=1 lead hp hit
  HEAD_POS=-1
  while :; do
    # At a segment start, with a list-transparent delimiter behind it.
    if [ "$safe" -eq 1 ]; then
      lead=${s:start}
      lead=${lead%%[![:blank:]]*}
      hp=$(( start + ${#lead} ))
      if is_push_head "${s:hp}"; then HEAD_POS=$hp; return 0; fi
    fi
    # Advance to the next segment boundary, or run out of command.
    hit=0
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
            while [ "$i" -lt "$n" ] && [ "${s:i:1}" != $'\n' ]; do i=$((i+1)); done
            [ "$i" -lt "$n" ] && i=$((i+1))
            start=$i; wordstart=1; safe=1; hit=1; break
          else
            i=$((i+1)); wordstart=0
          fi ;;
        '(' | ')')
          # `>(` / `<(` is process substitution, not a subshell.
          { [ "$pv" = '>' ] || [ "$pv" = '<' ]; } && return 1
          i=$((i+1)); start=$i; wordstart=1; hit=1
          [ "$c" = '(' ] && safe=1 || safe=0
          break ;;
        '&')
          if [ "$pv" = '>' ] || [ "$pv" = '<' ] || [ "$nx" = '>' ]; then
            i=$((i+1)); wordstart=0            # 2>&1, >&2, &>file
          else
            # `&&`: the FIRST & opens an empty segment, the SECOND opens the
            # right-hand command -- and that one is list-transparent. A lone
            # `&` is a plain list separator, so it is transparent too.
            if [ "$nx" = '&' ]; then safe=0; else safe=1; fi
            i=$((i+1)); start=$i; wordstart=1; hit=1; break
          fi ;;
        '|')
          if [ "$pv" = '>' ]; then
            i=$((i+1)); wordstart=0            # >|file
          else
            i=$((i+1)); start=$i; wordstart=1; safe=0; hit=1; break
          fi ;;
        '{')
          # Only the group-open reserved word splits; `--x={a,b}` must not.
          if [ "$wordstart" -eq 1 ] && { [ "$nx" = ' ' ] || [ "$nx" = $'\t' ] || [ "$nx" = $'\n' ]; }; then
            i=$((i+1)); start=$i; wordstart=1; safe=1; hit=1; break
          else
            i=$((i+1)); wordstart=0
          fi ;;
        ';' | $'\n')
          i=$((i+1)); start=$i; wordstart=1; safe=1; hit=1; break ;;
        ' ' | $'\t')
          i=$((i+1)); wordstart=1 ;;
        *)
          i=$((i+1)); wordstart=0 ;;
      esac
    done
    [ "$hit" -eq 1 ] || return 1
  done
}

# `git push` / `gh pr create`, bare or rtk-prefixed, as the HEAD of the
# segment. Anything between the words but blanks (`env X=1 git push`,
# `then git push`, `git -C /x push`) deliberately does not match: those are
# either unwrappable or would need a real parser to place correctly.
# The head is matched against the whole REST of the command, not against a
# pre-cut segment, so the word can also be terminated by the operator that
# ends its segment -- `{ git push; }` ends at the `;`.
# Held in variables and used UNQUOTED: a bracket expression written inline in
# `[[ ... =~ ... ]]` has to survive bash's own parser first, and `)` there is
# a live metacharacter. Inside the class `; & | )` are all literal, so none of
# them needs a backslash -- and a backslash would not escape them anyway, it
# would join the class as a sixth member.
PUSH_RE='^(rtk[[:space:]]+)?git[[:space:]]+push([[:space:];&|)]|$)'
PRC_RE='^(rtk[[:space:]]+)?gh[[:space:]]+pr[[:space:]]+create([[:space:];&|)]|$)'
is_push_head() {
  [[ $1 =~ $PUSH_RE ]] && return 0
  [[ $1 =~ $PRC_RE ]] && return 0
  return 1
}

find_insert_point "$BASE" || passthrough

RECORDER=${RECORDER_TMPL//@ID@/$TOOL_USE_ID}

# Spliced in as its own list element AHEAD of the push, glued with `&&` so a
# `cd "$WT" && git push` keeps its conditionality: were this a `;`, a failed
# `cd` would no longer stop the push and it would run in the wrong directory.
# The recorder ends in `|| :`, so it can never be the thing that stops a push.
#
# Only the FIRST push in a command is recorded. The record file is keyed by
# tool_use_id, so a second recorder could only overwrite the first.
NEW="${BASE:0:HEAD_POS}{ ${RECORDER}; } && ${BASE:HEAD_POS}"

# Belt and braces: if the rewrite does not parse cleanly, ship rtk's output
# alone. This is the net under every scanner assumption above.
parses_clean "$NEW" || passthrough

# --- 5. emit merged JSON ---------------------------------------------------
# updatedInput keeps every other tool_input field (description, timeout, ...).
#
# permissionDecision / permissionDecisionReason are copied VERBATIM, and only
# when rtk actually set them. Their absence is not an oversight to be filled in:
# rtk omits them on its exit 3 ("an ask rule matched") precisely so that Claude
# Code prompts, and this wrapper never widens a permission. It only ever adds a
# recorder to a command rtk already decided to rewrite.
#
# An earlier revision defaulted this to "allow" and was wrong to. Supplying a
# decision rtk did not make is what widens a permission, and it was not needed:
# the `--permission-prompt-tool` failure this was chasing came from the `$(...)`
# the recorder used to inject, which is gone. When rtk declined entirely there
# is no decision to copy, so none is emitted and Claude Code decides.
OUT=$(jq -n \
  --argjson payload "$INPUT" \
  --argjson rtk "$RTK_JSON" \
  --arg cmd "$NEW" '
  (($rtk // {}).hookSpecificOutput // {}) as $h
  | (($h.updatedInput // $payload.tool_input // {}) | .command = $cmd) as $ui
  | { hookSpecificOutput:
        ( { hookEventName: "PreToolUse" }
          + (if ($h.permissionDecision // null) != null
             then { permissionDecision: $h.permissionDecision } else {} end)
          + (if ($h.permissionDecisionReason // null) != null
             then { permissionDecisionReason: $h.permissionDecisionReason } else {} end)
          + { updatedInput: $ui } ) }
' 2>/dev/null) || passthrough
[ -n "$OUT" ] || passthrough

printf '%s\n' "$OUT"
exit 0
