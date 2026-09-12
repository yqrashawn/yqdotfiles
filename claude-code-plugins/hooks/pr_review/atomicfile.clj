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
  (:require [babashka.fs :as fs])
  (:import [java.nio.file CopyOption Files StandardCopyOption]))

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
     (try
       (spit tmp content)
       (when mode (fs/set-posix-file-permissions tmp mode))
       (Files/move (fs/path tmp) (fs/path path)
                   (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE]))
       path
       (finally
         ;; A unique name cannot be reused, so a failed write would litter the
         ;; directory forever instead of being overwritten by the next attempt.
         ;; After a successful move there is nothing left to delete.
         (fs/delete-if-exists tmp))))))
