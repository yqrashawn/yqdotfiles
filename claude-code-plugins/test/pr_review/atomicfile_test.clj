(ns pr-review.atomicfile-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [pr-review.atomicfile :as atomicfile]))

(defn- tmp-file [content]
  (let [d (str (fs/create-temp-dir {:prefix "prl-atomic"}))
        f (str (fs/path d "target"))]
    (spit f content)
    f))

(deftest a-write-that-fails-leaves-the-original-intact
  (testing "the whole point, and the only property that distinguishes this from
            `spit`. `spit` opens with O_TRUNC, so the file is empty from that
            instant until the write completes: a process killed in between
            destroys it. Here the content goes to a sibling temp first, so a
            failure before the rename cannot touch the original.

            The failure is forced with an unparsable mode, which throws after
            the temp is written and before the rename — the exact window a
            crash would land in."
    (let [f (tmp-file "the user's original content\n")]
      (is (thrown? Exception (atomicfile/spit! f "replacement" "not-a-mode")))
      (is (= "the user's original content\n" (slurp f))
          "a failed replace must not have touched the original")
      (is (= ["target"] (mapv (comp str fs/file-name) (fs/list-dir (fs/parent f))))
          "and it must not litter: a unique temp name is never reused, so a
           failure that left one behind would accumulate forever"))))

(deftest concurrent-writers-never-expose-a-partial-file
  (testing "`hookinstall/-main` runs from SessionStart over every known clone,
            so two sessions starting at once rewrite the same `pre-push`. With
            one shared temp name that is not safe: writer A truncates the temp
            while writer B is between its own write and its rename, and B
            renames a HALF-WRITTEN file into place. `pre-push` runs under
            `set -u` with its record block inside braces, so a cut mid-brace is
            a shell syntax error — a non-zero pre-push that aborts the user's
            push. Each writer must therefore own its temp."
    (let [f (tmp-file "seed")
          wholes (into #{} (for [c "AB"] (apply str (repeat 20000 c))))
          torn (atom [])
          stop (atom false)
          reader (future (while (not @stop)
                           (let [seen (try (slurp f) (catch Exception _ nil))]
                             (when (and seen (not (contains? wholes seen)) (not= "seed" seen))
                               (swap! torn conj (count seen))))))
          writers (mapv #(future (dotimes [_ 40] (atomicfile/spit! f %)))
                        wholes)]
      (run! deref writers)
      (reset! stop true)
      @reader
      (is (= [] @torn) "every read must have been a whole version, never a fragment")
      (is (contains? wholes (slurp f)))
      (is (= ["target"] (mapv (comp str fs/file-name) (fs/list-dir (fs/parent f))))
          "no temp may survive the storm"))))

(deftest a-temp-abandoned-by-a-kill-is-swept-by-the-next-write
  (testing "the `finally` covers the exception path; it cannot cover the one
            this namespace exists for. A SIGKILL between the write and the
            rename leaves a temp no writer will ever reuse — the fixed name it
            replaced was self-healing there, a unique name is not. Where
            `core.hooksPath` is relative (`.githooks`) the hooks directory is
            inside the working tree and `exclude-locally!` excludes `pre-push`
            and nothing else, so each leftover sits in the user's `git status`
            forever.

            A temp young enough to belong to a live writer must survive: this
            runs in front of a write, concurrently with other writers."
    (let [f (tmp-file "original")
          dead (str f ".tmp.111.222")
          live (str f ".tmp.333.444")
          other (str (fs/path (fs/parent f) "unrelated.tmp.555.666"))]
      (spit dead "half a hook")
      (spit live "another writer, mid-write")
      (spit other "not this path's")
      (fs/set-last-modified-time dead (- (System/currentTimeMillis) (* 25 60 60 1000)))
      (fs/set-last-modified-time other (- (System/currentTimeMillis) (* 25 60 60 1000)))
      (atomicfile/spit! f "new")
      (is (= "new" (slurp f)))
      (is (not (fs/exists? dead)) "the abandoned temp must be gone")
      (is (fs/exists? live) "a live writer's temp must not be deleted under it")
      (is (fs/exists? other) "and only THIS path's temps are swept"))))

(deftest the-replacement-lands-whole
  (let [f (tmp-file "old")]
    (atomicfile/spit! f "new content")
    (is (= "new content" (slurp f)))
    (is (= ["target"] (mapv (comp str fs/file-name) (fs/list-dir (fs/parent f))))
        "the temp must be renamed away, not left")))

(deftest the-mode-is-set-before-the-file-is-visible
  (testing "`spit` then `chmod` leaves a window where the file exists with the
            wrong mode. Setting it on the temp closes that window"
    (let [d (str (fs/create-temp-dir {:prefix "prl-atomic"}))
          f (str (fs/path d "hook"))]
      (atomicfile/spit! f "#!/bin/sh\nexit 0\n" "rwxr-xr-x")
      (is (contains? (fs/posix-file-permissions f)
                     java.nio.file.attribute.PosixFilePermission/OWNER_EXECUTE)))))

(deftest missing-parents-are-created
  (let [d (str (fs/create-temp-dir {:prefix "prl-atomic"}))
        f (str (fs/path d "a" "b" "c.txt"))]
    (atomicfile/spit! f "x")
    (is (= "x" (slurp f)))))
