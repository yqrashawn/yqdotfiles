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

            The failure is forced by making the temp path unwritable — a
            directory sitting where the temp file would go."
    (let [f (tmp-file "the user's original content\n")]
      (fs/create-dirs (str f ".tmp"))          ; temp path is now a directory
      (is (thrown? Exception (atomicfile/spit! f "replacement")))
      (is (= "the user's original content\n" (slurp f))
          "a failed replace must not have touched the original"))))

(deftest the-replacement-lands-whole
  (let [f (tmp-file "old")]
    (atomicfile/spit! f "new content")
    (is (= "new content" (slurp f)))
    (is (not (fs/exists? (str f ".tmp"))) "the temp must be renamed away, not left")))

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
