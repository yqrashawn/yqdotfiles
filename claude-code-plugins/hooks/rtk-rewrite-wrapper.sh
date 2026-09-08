#!/usr/bin/env bash
# pr-review-loop PreToolUse wrapper around rtk-rewrite.sh
# ---------------------------------------------------------------------------
# WHY THIS EXISTS
#   The PostToolUse payload carries only the SESSION cwd, never the directory a
#   `git push` actually ran in (agents routinely do `cd "$SP/wt" && git push`).
#   The only universal way to learn the real directory is to make the shell
#   itself report it -- i.e. append a recorder to the command via `updatedInput`.
#   `updatedInput` is single-owner in practice: rtk-rewrite.sh already emits it
#   and rewrites from the ORIGINAL command, so a second hook's suffix is
#   clobbered. Hence: wrap rtk, delegate to it, then append our recorder to
#   whatever it produced.
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
#   Only commands containing a `git push` / `gh pr create` segment get a
#   recorder appended (see the gate in section 3a). Every other Bash command is
#   passed through with rtk's own output byte-identical, so nothing else on the
#   machine changes shape.
#
# FAIL-OPEN INVARIANT
#   Every failure path degrades to "behave exactly like rtk alone": echo rtk's
#   stdout verbatim (possibly empty) and exit 0. This hook runs on EVERY Bash
#   tool call in EVERY session on this machine; it must never block a command,
#   never change a command's meaning, and never exit non-zero.
#
# KNOWN LIMITATION (documented, not solved)
#   A `cd` confined to a subshell -- `(cd /x && git push)` -- does not change the
#   parent shell's $PWD, so the recorder captures the OUTER directory. Same for
#   `git -C /x push` and `pushd`-in-subshell. Still strictly better than parsing
#   the command text; the consumer layers its own fallback on top.
# ---------------------------------------------------------------------------

set -u

RTK_HOOK=${PRL_RTK_HOOK:-$HOME/.claude/hooks/rtk-rewrite.sh}
# Left unexpanded on purpose: ${TMPDIR} must be resolved by the shell that
# runs the command, not by this hook process.
# shellcheck disable=SC2016
RECORD_DIR_EXPR='${TMPDIR:-/tmp}/pr-review-pushdir'
MARKER='__prl_record_v1'

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

ORIG_CMD=$(printf '%s' "$INPUT" | jq -r '.tool_input.command // empty' 2>/dev/null)
BASE=$(printf '%s' "$RTK_JSON" | jq -r '.hookSpecificOutput.updatedInput.command // empty' 2>/dev/null)
[ -n "$BASE" ] || BASE=$ORIG_CMD
[ -n "$BASE" ] || passthrough

# --- 3a. gate: only push / PR-create commands get a recorder ---------------
# The pr-review-loop trigger only ever fires on `git push` or `gh pr create`, so
# a recorder on anything else is pure risk with no benefit -- and skipping them
# keeps every other command's `updatedInput` byte-identical to rtk's own output,
# which is what Claude Code permission-checks.
#
# The match is DELIBERATELY LIBERAL, and errs toward appending:
#   * a false negative costs nothing -- no record, and the consumer already
#     falls back to command parsing;
#   * a false positive costs one harmless extra file under TMPDIR.
# So: split on `;` `&&` `||` and newline (single `&`/`|` too -- strictly more
# splitting, and neither character can occur inside the phrases we look for),
# squeeze whitespace runs, then substring-match case-sensitively. The bare and
# `rtk`-prefixed forms both fall out of this for free, since `rtk git push`
# contains `git push`. Quoting is NOT analysed -- `echo "git push"` matches, on
# purpose: over-matching is the safe direction.
# Both rtk's rewrite and the original command are tested; either one is enough.
# Known non-matches, by choice: `git -C /x push` and other forms that put words
# between `git` and `push`. They cannot be recorded correctly anyway (the
# recorder reads $PWD, which such a command never changes), so a no-record and
# the consumer's fallback is the honest outcome.
gate_match() {
  # SC2020: the duplicated '\n' in the second set is intentional -- all four
  # separators collapse to a newline.
  # shellcheck disable=SC2020
  printf '%s' "$1" \
    | tr ';&|\n' '\n\n\n\n' \
    | tr -s ' \t' ' ' \
    | grep -q -e 'git push' -e 'gh pr create'
}
gate_match "$BASE" || gate_match "$ORIG_CMD" || passthrough

# --- 4. refuse to append when appending could change meaning ---------------
# Idempotency: never stack a second recorder on a command that already has one.
case $BASE in *"$MARKER"*) passthrough ;; esac

# A trailing odd run of backslashes is a line continuation: our newline would
# splice the recorder into the base command's argument list.
TRAIL=${BASE##*[!\\]}
if [ -n "$TRAIL" ] && [ $(( ${#TRAIL} % 2 )) -eq 1 ]; then passthrough; fi

# The base must be COMPLETE: nothing on a following line may be absorbed into
# it. Three ways that can fail, all refused here:
#   * syntax error  -- `foo &&`, `foo |`, `if true; then`, unterminated quote
#                      (bash -n exits non-zero)
#   * parser warning -- an UNTERMINATED heredoc. bash -n *accepts* `cat <<'EOF'`
#                      with no terminator and only warns; appending would make
#                      the recorder the heredoc's BODY, so `cat` would print it
#                      instead of running it. Any warning therefore refuses too.
#   * line continuation -- handled by the trailing-backslash test above.
# Only a base that parses silently gets a recorder.
parses_clean() {
  local err
  err=$(printf '%s\n' "$1" | bash -n 2>&1) || return 1
  [ -z "$err" ]
}
parses_clean "$BASE" || passthrough

# --- 5. compose base + recorder -------------------------------------------
# Appended after a NEWLINE, never after `;`: commands routinely end in a
# heredoc whose terminator must be alone on its line, and a bare newline is
# already a valid command separator.
# $? is captured first and re-raised last so the Bash tool still sees the real
# exit status; the recorder is fully muted and cannot fail the command.
RECORDER="__prl_rc=\$?; { __prl_d=\"$RECORD_DIR_EXPR\"; mkdir -p \"\$__prl_d\" && printf 'tool_use_id=%s\\npwd=%s\\nbranch=%s\\nts=%s\\n' '$TOOL_USE_ID' \"\$PWD\" \"\$(git rev-parse --abbrev-ref HEAD 2>/dev/null)\" \"\$(date -u +%s 2>/dev/null)\" > \"\$__prl_d/$TOOL_USE_ID\"; } >/dev/null 2>&1 || :; ( exit \$__prl_rc ) # $MARKER"

NEW=$(printf '%s\n%s' "$BASE" "$RECORDER")

# Belt and braces: if the composition does not parse cleanly, ship rtk's
# output alone.
parses_clean "$NEW" || passthrough

# --- 6. emit merged JSON ---------------------------------------------------
# updatedInput keeps every other tool_input field (description, timeout, ...).
# permissionDecision / permissionDecisionReason are copied verbatim, and only
# when rtk actually set them -- their absence is what makes Claude Code prompt
# for rtk's "ask" rules, so it must stay absent.
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
