(ns pr-review.hookinstall-test
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [pr-review.hookinstall :as hi]))

(def ^:private hook-source
  (str (fs/path (or (some-> (System/getProperty "babashka.config") fs/parent str)
                    (System/getProperty "user.dir"))
                "hooks" "pr-push-hook.sh")))

(defn- clone!
  "A real clone — `hooks-dir` shells out to `git rev-parse`, so a fake will
   not do."
  []
  (let [d (str (fs/create-temp-dir {:prefix "pr-review-hookinstall"}))]
    (p/sh ["git" "init" "-q" "--initial-branch=main" d])
    d))

(defn- hooks-of [root] (hi/hooks-dir root {}))

(deftest the-tracked-hook-source-exists
  ;; Every test below installs this file; without it they would all pass
  ;; vacuously on a rename.
  (is (fs/exists? hook-source) (str "hook source missing: " hook-source))
  (is (str/includes? (slurp hook-source) hi/marker)
      "the hook must carry the marker install! identifies it by"))

(deftest a-fresh-clone-gets-the-hook-executable
  (let [root (clone!)
        {:keys [status path]} (hi/install! root hook-source {})]
    (is (= :installed status))
    (is (= (slurp hook-source) (slurp path)))
    (is (contains? (fs/posix-file-permissions path) java.nio.file.attribute.PosixFilePermission/OWNER_EXECUTE)
        "git will not run a hook it cannot execute")))

(deftest re-installing-the-same-content-is-a-no-op
  (let [root (clone!)]
    (is (= :installed (:status (hi/install! root hook-source {}))))
    (is (= :current (:status (hi/install! root hook-source {}))))))

(deftest a-changed-hook-is-updated-in-place-not-chained-to-itself
  ;; Without the marker check, a version bump would park our own old hook as
  ;; the "existing" one and exec it forever.
  (let [root (clone!)
        {:keys [path]} (hi/install! root hook-source {})]
    (spit path (str (slurp hook-source) "\n# stale\n"))
    (is (= :updated (:status (hi/install! root hook-source {}))))
    (is (= (slurp hook-source) (slurp path)))
    (is (not (fs/exists? (fs/path (hooks-of root) hi/chained-name)))
        "our own hook must never be parked as a foreign one")))

(deftest an-existing-foreign-hook-is-preserved-and-chained
  (let [root (clone!)
        dir (hooks-of root)
        theirs (str (fs/path dir "pre-push"))
        body "#!/bin/sh\necho theirs\nexit 0\n"]
    (fs/create-dirs dir)
    (spit theirs body)
    (fs/set-posix-file-permissions theirs "rwxr-xr-x")
    (is (= :chained (:status (hi/install! root hook-source {}))))
    (is (= body (slurp (str (fs/path dir hi/chained-name))))
        "the user's hook must survive byte-for-byte")
    (is (= (slurp hook-source) (slurp theirs)))
    (is (contains? (fs/posix-file-permissions (fs/path dir hi/chained-name))
                   java.nio.file.attribute.PosixFilePermission/OWNER_EXECUTE)
        "ours execs it, so it has to stay executable")))

(deftest a-taken-chain-slot-refuses-rather-than-guess
  ;; Two foreign hooks and one slot: overwriting would drop whichever is
  ;; actually live, and there is no way to tell which from here.
  (let [root (clone!)
        dir (hooks-of root)]
    (fs/create-dirs dir)
    (spit (str (fs/path dir "pre-push")) "#!/bin/sh\nexit 0\n")
    (spit (str (fs/path dir hi/chained-name)) "#!/bin/sh\necho parked\nexit 0\n")
    (is (= :conflict (:status (hi/install! root hook-source {}))))
    (is (= "#!/bin/sh\nexit 0\n" (slurp (str (fs/path dir "pre-push"))))
        "the live hook must be left exactly as it was")
    (is (= "#!/bin/sh\necho parked\nexit 0\n"
           (slurp (str (fs/path dir hi/chained-name)))))))

(deftest outside-a-repository-nothing-is-written-and-nothing-throws
  ;; install! runs from SessionStart, in whatever directory that session sits
  ;; in — frequently not a clone at all.
  (let [d (str (fs/create-temp-dir {:prefix "pr-review-norepo"}))]
    (is (= {:status :no-repo :path nil} (hi/install! d hook-source {})))
    (is (empty? (fs/list-dir d)))))

(deftest one-install-covers-every-worktree-of-the-clone
  ;; This is the property the whole design rests on: the directory problem
  ;; lived in worktrees, and hooks are shared through the common git dir.
  (let [root (clone!)]
    (spit (str (fs/path root "f")) "x")
    (p/sh ["git" "add" "f"] {:dir root})
    (p/sh ["git" "-c" "user.email=t@t" "-c" "user.name=t" "commit" "-qm" "init"] {:dir root})
    (let [wt (str (fs/path (fs/parent root) (str (fs/file-name root) "-wt")))]
      (p/sh ["git" "worktree" "add" "-q" "--detach" wt] {:dir root})
      (is (= (hooks-of root) (hooks-of wt))
          "a linked worktree must resolve to the clone's hooks directory")
      (is (= :installed (:status (hi/install! root hook-source {}))))
      (is (= :current (:status (hi/install! wt hook-source {})))
          "installing from the worktree must find the hook already there"))))

(deftest core-hookspath-is-honoured-not-guessed
  ;; This clone sets core.hooksPath explicitly. Hardcoding <root>/.git/hooks
  ;; would install somewhere git never looks, and the loop would simply never
  ;; fire with no error anywhere.
  (let [root (clone!)
        elsewhere (str (fs/path root "custom-hooks"))]
    (fs/create-dirs elsewhere)
    (p/sh ["git" "config" "core.hooksPath" elsewhere] {:dir root})
    (let [{:keys [status path]} (hi/install! root hook-source {})]
      (is (= :installed status))
      ;; Canonicalized both sides: `--path-format=absolute` resolves symlinks,
      ;; so on macOS it answers /private/var where the temp dir reads /var.
      ;; That canonical form is what we want as the index key — it just has to
      ;; be compared as one.
      (is (= (str (fs/canonicalize (fs/path elsewhere "pre-push")))
             (str (fs/canonicalize path))))
      (is (not (fs/exists? (fs/path root ".git" "hooks" "pre-push")))
          "nothing may be written to the directory git is ignoring"))))
