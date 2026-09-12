(ns pr-review.flock
  "Cross-process mutual exclusion via an OS-level file lock.

   Shared by pr-review.ledger and pr-review.lock so both modules serialize
   through the same mechanism instead of each hand-rolling file locking.

   Invariant: never flock a path that the locked operation itself renames or
   replaces (e.g. an atomic-rename publish). A rename swaps the inode at
   that path, so a `RandomAccessFile` opened and locked before the rename
   still refers to the old, now-orphaned inode — a second process that
   opens the same path afterwards locks a *different* inode and proceeds
   concurrently, breaking mutual exclusion exactly when it matters most.
   Always flock a sibling guard file (`guard-path`) that the locked
   operation never touches instead."
  (:require [babashka.fs :as fs])
  (:import [java.io RandomAccessFile]))

(defn guard-path
  "Sibling path to flock when serializing an operation that renames or
   replaces `path` itself. Never flock `path` directly in that case — see
   the namespace docstring. Just appends \".guard\" to `path`."
  [path]
  (str path ".guard"))

(defn with-file-lock
  "Run `f` (a no-arg function) while holding an exclusive lock on `path`,
   creating `path` and its parent directories first if missing. Returns
   `f`'s return value.

   The lock is released by closing the channel, not by calling .release —
   babashka does not allow sun.nio.ch.FileLockImpl.release."
  [path f]
  (fs/create-dirs (fs/parent path))
  (when-not (fs/exists? path) (spit path ""))
  (with-open [raf (RandomAccessFile. (str path) "rw")]
    (.lock (.getChannel raf))
    (f)))
