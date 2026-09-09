(ns pr-review.pushrecord
  "Where the push actually ran, asked of the shell instead of read off the
   command text.

   A `PreToolUse` wrapper around rtk (`~/.claude/hooks/rtk-rewrite-wrapper.sh`)
   locates the push subcommand and replaces it, in place, with a brace group
   that runs a recorder first:

       cd /x && git push -u origin b
       cd /x && { <recorder>; git push -u origin b; }

   So the recorder runs AT the push, inside whatever subshell or background job
   the push is in, and reports that shell's own `$PWD` into a file named for
   the call's `tool_use_id`:

       tool_use_id=toolu_015WAZ23TGGJVjNWCm9P4Yn1
       pwd=/private/tmp/.../scratchpad/prod/wt

   Two fields, and only two. `branch=` and `ts=` were recorded by an earlier
   revision and are gone: `pr-review.trigger` derives the branch itself with
   `gh/current-branch`, `prune-records!` ages records off the file's mtime, and
   writing either one needed a `$(...)` in the rewritten command — which
   Claude Code's permission analysis descends into, so it turned every push
   into a command carrying subcommands the user never wrote. `parse` below
   still accepts both keys, so a record written before that change reads fine.

   `tool_use_id` is carried by both the PreToolUse and the PostToolUse payload
   and correlates between them, so the lookup is exact — never \"the most
   recent record\". That is what makes this a better source than
   `pr-review.workdir`, which can only infer a directory from the command
   string and, being pure string analysis, is imperfect by nature. This
   namespace is only the reader and the janitor; all the parsing stays over
   there, and stays the fallback for every call the recorder never saw.

   BECAUSE THE RECORDER MOVES WITH THE PUSH, the shapes that used to record the
   outer directory no longer do: `(cd /x && git push)` in a subshell and
   `cd /x && git push &` both record `/x`, measured. What remains is not a
   wrong record but NO record — the wrapper refuses to rewrite a command whose
   boundaries it cannot place exactly, and refusing costs nothing, because this
   namespace then returns nil and `pr-review.workdir` is consulted as usual.
   It refuses any command containing `$(...)`, a backtick, a heredoc, process
   substitution, or an unterminated quote; and it declines any push whose
   simple-command head is not literally `git push` / `gh pr create` (bare or
   `rtk`-prefixed) — so `git -C /x push`, `env FOO=1 git push`, and a push
   under `then`/`do` inside an `if`/`for` produce no record either. `git -C /x
   push` is the honest one of those: no `$PWD`-based recorder could ever get it
   right."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]))

(def ^:private dir-name "pr-review-pushdir")

(def ^:private id-re
  "`tool_use_id` is joined onto a record directory, so it is whitelisted
   rather than sanitized. Same character set the PreToolUse wrapper demands
   before it will write a record: anything outside it cannot name a file the
   recorder produced, and `..` must never reach `fs/path`."
  #"[A-Za-z0-9_-]{1,128}")

(defn- tmpdir
  "$TMPDIR as this process sees it. A function and not a top-level `def` so
   the value is read at call time — and so a test can bind it."
  []
  (System/getenv "TMPDIR"))

(defn record-dirs
  "The directories a push record could be in, in search order:
   `$TMPDIR/pr-review-pushdir` first when TMPDIR is set, then
   `/tmp/pr-review-pushdir`.

   Both, always. The recorder expands `${TMPDIR:-/tmp}` in the shell that ran
   the push and this process expands it again; measured, the two agree today,
   but nothing enforces it. A hook started from a different environment that
   looked in only one place would silently fall back to command parsing
   forever, with no signal that the better source existed."
  []
  (let [t        (tmpdir)
        fallback (str "/tmp/" dir-name)]
    (if (str/blank? t)
      [fallback]
      (vec (distinct [(str (str/replace t #"/+$" "") "/" dir-name) fallback])))))

(defn- parse
  "The recorder's `key=value` lines, keeping only the keys this namespace
   promises. A line with no `=`, an unknown key and a blank value are all
   dropped rather than reported: the recorder is a shell one-liner, and a key
   whose value says nothing is worse than an absent one. `branch` and `ts` are
   still accepted although the current recorder writes neither (see the ns
   docstring) — a record left over from an older wrapper must keep reading."
  [text]
  (reduce
   (fn [m line]
     (let [i (str/index-of line "=")]
       (if-not (and i (pos? i))
         m
         (let [v (subs line (inc i))]
           (if (str/blank? v)
             m
             (case (subs line 0 i)
               "pwd"    (assoc m :pwd v)
               "branch" (assoc m :branch v)
               "ts"     (if-let [n (parse-long (str/trim v))] (assoc m :ts n) m)
               m))))))
   {} (str/split-lines (str text))))

(defn read-record
  "The push record for `tool-use-id`, or nil.

   `{:pwd String :branch String :ts long}` — whichever of those keys the file
   actually carried. Never throws, for any input: a missing file, a
   half-written one, a directory where a file should be, an id that is nil,
   blank, or not shaped like an id at all.

   Deliberately WITHOUT side effects — it must not delete the record it just
   read. `hooks.json`'s `if` conditions are documented best-effort and fail
   open, so one command Claude Code cannot classify fires all four entries and
   produces four concurrent `decide` calls for the SAME `tool_use_id`. If the
   first reader deleted the record, the other three would silently fall back
   to command parsing, which can resolve a different directory — the same push
   reviewed against whichever repository won a race. Age-based pruning is
   sufficient, and race-free, precisely because the key is unique: a record
   can never be picked up by a *later* push."
  [tool-use-id]
  (when (and (string? tool-use-id) (re-matches id-re tool-use-id))
    (some (fn [dir]
            (try
              (let [f (fs/path dir tool-use-id)]
                (when (fs/regular-file? f)
                  (not-empty (parse (slurp (fs/file f))))))
              (catch Exception _ nil)))
          (record-dirs))))

(defn prune-records!
  "Deletes every record older than `max-age-ms` from every `record-dirs`
   entry. Returns how many were deleted.

   This is the only cleanup these files get: `read-record` must not delete
   (see there), the recorder never revisits what it wrote, and `$TMPDIR` here
   is `~/.cchp/tmp`, which nothing sweeps. Age comes from the file's mtime and
   not the `ts=` line — the mtime is always present and needs no parsing, so a
   record too garbled to parse still gets collected.

   Never throws. A record directory that does not exist, one that cannot be
   listed, and a file another trigger deleted between the listing and the
   delete are all ordinary here, not failures."
  [max-age-ms]
  (try
    (let [cutoff (- (System/currentTimeMillis) (max 0 (long max-age-ms)))]
      (reduce
       (fn [n dir]
         (+ n (try
                (count
                 (filter (fn [f]
                           (try
                             (and (< (fs/file-time->millis (fs/last-modified-time f))
                                     cutoff)
                                  (fs/delete-if-exists f))
                             (catch Exception _ false)))
                         (fs/list-dir dir)))
                (catch Exception _ 0))))
       0 (record-dirs)))
    (catch Exception _ 0)))
