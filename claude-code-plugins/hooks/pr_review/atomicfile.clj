(ns pr-review.atomicfile
  "Replace a file's whole contents without ever leaving it truncated.

   `spit` opens with O_TRUNC: the file is empty from that instant until the
   write completes, so a process killed in between destroys it, and a
   concurrent reader sees nothing. Every file this plugin rewrites is one
   somebody else also reads or owns — a ledger another trigger is reading, the
   user's `pre-push`, the user's `info/exclude` — so that window is not
   acceptable anywhere.

   Write a sibling temp, set its mode, then rename. A rename within one
   directory is atomic: readers see the old file or the new one, never a
   partial one, and a kill leaves the original intact.

   This does NOT make a read-modify-write safe against a concurrent writer —
   the read can still go stale before the rename. Callers that
   read-modify-write must hold a lock as well; see `pr-review.flock`."
  (:require [babashka.fs :as fs]
            [clojure.string :as str])
  (:import [java.nio.file CopyOption Files StandardCopyOption]))

(def ^:private stale-temp-ms
  "How old a leftover temp must be before a later write removes it. Far longer
   than any write here takes, so a temp another process is mid-write on is
   never in range."
  (* 60 60 1000))

(defn- sweep-stale-temps!
  "Delete this path's abandoned temps.

   The `finally` below covers the exception path; it cannot cover the one this
   namespace exists for. A SIGKILL between the write and the rename leaves a
   temp with no writer that will ever reuse it — the fixed name it replaced was
   self-healing there, the unique name is not. That matters where the hooks
   directory is inside a working tree, which a relative `core.hooksPath` such
   as `.githooks` makes it: `exclude-locally!` excludes `pre-push` and nothing
   else, so each leftover shows in the user's `git status` forever.

   Best effort, and never fatal: this is housekeeping in front of a write that
   must succeed."
  [path]
  (try
    (let [prefix (str (fs/file-name path) ".tmp.")
          cutoff (- (System/currentTimeMillis) stale-temp-ms)]
      (doseq [f (fs/list-dir (fs/parent path))
              :when (and (str/starts-with? (str (fs/file-name f)) prefix)
                         (< (.toMillis (fs/last-modified-time f)) cutoff))]
        (fs/delete-if-exists f)))
    (catch Exception _ nil)))

(defn spit!
  "Write `content` to `path` atomically. `mode` is an optional POSIX string
   applied to the temp file BEFORE the rename, so the file is never visible
   with the wrong permissions."
  ([path content] (spit! path content nil))
  ([path content mode]
   (let [path (str path)
         ;; A UNIQUE temp, not a fixed `<path>.tmp`. `hookinstall/-main` loops
         ;; over every known clone from SessionStart, so two sessions starting
         ;; at once write the same hook: with a shared temp name one truncates
         ;; it between the other's write and its rename, and a PARTIAL pre-push
         ;; gets renamed into place. That file runs under `set -u` with its
         ;; record block inside `{ … }`, so a cut mid-brace is a shell syntax
         ;; error and a non-zero pre-push aborts the user's push — the one
         ;; thing its header forbids by name.
         tmp (str path ".tmp." (System/nanoTime) "." (rand-int 1000000))]
     (fs/create-dirs (fs/parent path))
     (sweep-stale-temps! path)
     (try
       (spit tmp content)
       (when mode (fs/set-posix-file-permissions tmp mode))
       (Files/move (fs/path tmp) (fs/path path)
                   (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE]))
       path
       (finally
         ;; A unique name cannot be reused, so a failed write would litter the
         ;; directory instead of being overwritten by the next attempt. This
         ;; covers the exception path only — a kill cannot run it, which is what
         ;; `sweep-stale-temps!` is for.
         (fs/delete-if-exists tmp))))))
