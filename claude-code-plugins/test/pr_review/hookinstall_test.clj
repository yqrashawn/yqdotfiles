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
    (let [r (hi/install! d hook-source {})]
      (is (= :no-repo (:status r)))
      (is (nil? (:path r)))
      (is (empty? (:results r))))
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

(defn- commit-all! [root msg]
  (p/sh ["git" "add" "-A"] {:dir root})
  (p/sh ["git" "-c" "user.email=t@t" "-c" "user.name=t" "commit" "-qm" msg] {:dir root}))

(deftest a-relative-hookspath-is-installed-into-every-worktree
  (testing "git resolves a RELATIVE core.hooksPath against each working tree,
            so `.githooks` names a different directory per worktree and a hook
            in the main tree never runs for a push made from a linked one.
            Measured: perpdex-chain-scan set `.githooks`, the hook sat in the
            main tree, a push from its worktree recorded nothing, and PR #66
            got no review"
    (let [root (clone!)]
      (spit (str (fs/path root "f")) "x")
      (commit-all! root "init")
      (p/sh ["git" "config" "core.hooksPath" ".githooks"] {:dir root})
      (let [wt (str (fs/path (fs/parent root) (str (fs/file-name root) "-wt")))]
        (p/sh ["git" "worktree" "add" "-q" "--detach" wt] {:dir root})
        (testing "the two trees really do resolve to different directories"
          (is (not= (hi/hooks-dir root {}) (hi/hooks-dir wt {})))
          (is (= 2 (count (hi/hooks-dirs root {})))))
        (let [r (hi/install! root hook-source {})]
          (is (= 2 (count (:results r))))
          (is (every? #{:installed} (map :status (:results r))))
          (is (fs/exists? (fs/path root ".githooks" "pre-push")))
          (is (fs/exists? (fs/path wt ".githooks" "pre-push"))
              "the worktree is where the push comes from"))))))

(deftest the-default-hooks-path-is-still-installed-only-once
  ;; $GIT_DIR/hooks is shared, so the per-worktree walk must not write it twice
  ;; or report two results for one directory.
  (let [root (clone!)]
    (spit (str (fs/path root "f")) "x")
    (commit-all! root "init")
    (let [wt (str (fs/path (fs/parent root) (str (fs/file-name root) "-wt2")))]
      (p/sh ["git" "worktree" "add" "-q" "--detach" wt] {:dir root})
      (is (= 1 (count (hi/hooks-dirs root {}))))
      (is (= 1 (count (:results (hi/install! root hook-source {}))))))))

(deftest a-hooks-dir-inside-the-working-tree-does-not-pollute-git-status
  (testing "a relative core.hooksPath puts our hook in the user's working tree,
            where it showed up as `?? .githooks/pre-push` — one `git add -A`
            from being committed into their repository"
    (let [root (clone!)]
      (spit (str (fs/path root "f")) "x")
      ;; A tracked file alongside, as the real repo has: without one git
      ;; collapses the whole untracked directory to `?? .githooks/` and the
      ;; assertions below would be testing a different report than the one
      ;; that was actually seen.
      (fs/create-dirs (fs/path root ".githooks"))
      (spit (str (fs/path root ".githooks" "pre-commit")) "#!/bin/sh\nexit 0\n")
      (commit-all! root "init")
      (p/sh ["git" "config" "core.hooksPath" ".githooks"] {:dir root})
      (hi/install! root hook-source {})
      (is (= "" (str/trim (str (:out (p/sh ["git" "status" "--short"] {:dir root})))))
          "the working tree must be clean after installing")
      (is (str/includes? (slurp (str (fs/path root ".git" "info" "exclude")))
                         ".githooks/pre-push"))
      (testing "and a hook left by an OLDER version, which never excluded, is
                excluded on the next run — it is already current, so nothing is
                written, and excluding only on write would leave it showing
                forever. That is the state the real repository was found in"
        (fs/delete-if-exists (fs/path root ".git" "info" "exclude"))
        (is (str/includes? (str (:out (p/sh ["git" "status" "--short"] {:dir root})))
                           ".githooks/pre-push")
            "the setup must actually reproduce the dirty state")
        (is (= :current (:status (hi/install! root hook-source {})))
            "nothing is written on this run")
        (is (= "" (str/trim (str (:out (p/sh ["git" "status" "--short"] {:dir root})))))
            "and it is excluded anyway")))))

(deftest a-tracked-hook-file-is-never-overwritten
  (testing "a hooks directory in the working tree can hold files git TRACKS.
            Writing over one shows as a modification and could be committed"
    (let [root (clone!)]
      (fs/create-dirs (fs/path root ".githooks"))
      (spit (str (fs/path root ".githooks" "pre-push")) "#!/bin/sh\ntheirs\n")
      (commit-all! root "their tracked hook")
      (p/sh ["git" "config" "core.hooksPath" ".githooks"] {:dir root})
      (let [r (hi/install! root hook-source {})]
        (is (= :tracked (:status r)))
        (is (= "#!/bin/sh\ntheirs\n" (slurp (str (fs/path root ".githooks" "pre-push"))))
            "their tracked hook must be left exactly as it was")
        (is (= "" (str/trim (str (:out (p/sh ["git" "status" "--short"] {:dir root}))))))))))

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
