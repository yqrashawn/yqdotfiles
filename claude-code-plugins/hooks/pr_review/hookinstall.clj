(ns pr-review.hookinstall
  "Installs the `pre-push` hook that names the pushed clone.

   Two properties matter more than convenience here, because this writes into
   a repository the user owns:

   NEVER SILENTLY DISABLE AN EXISTING HOOK. A repo may already have a
   `pre-push` — husky, lefthook, or the user's own — and overwriting it would
   remove a check they rely on with no signal. An existing hook is moved to
   `pre-push.pr-review-chained` and executed by ours, stdin replayed and exit
   status relayed, so its veto survives. If something is already parked at
   that name, this refuses rather than guess which of the two is live.

   NEVER INSTALL SOMEWHERE INERT. `core.hooksPath` redirects hooks wholesale,
   and this clone sets it explicitly — so the directory comes from
   `git rev-parse --git-path hooks`, never from `<root>/.git/hooks`. That
   resolution also shares hooks across worktrees: asked from a linked
   worktree it answers the main clone's hooks directory, so one install
   covers every worktree of the clone. Worktrees are where the whole
   directory problem lived, so that is the property being relied on."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.string :as str]
            [pr-review.attempts :as attempts]
            [pr-review.atomicfile :as atomicfile]
            [pr-review.flock :as flock]
            [pr-review.gh :as gh])
  (:import [java.nio.file CopyOption Files StandardCopyOption]))

(def marker
  "Identifies a hook as ours, so a re-install updates instead of chaining to
   itself. Bumped only if the hook's contract changes."
  "PR_REVIEW_LOOP_PRE_PUSH v1")

(def chained-name "pre-push.pr-review-chained")

(defn- run
  [opts args dir]
  ((or (:sh opts) (fn [a d] (p/sh a {:dir d}))) args dir))

(defn hooks-dir
  "The directory git will actually look in FROM `repo-root`, or nil outside a
   repository.

   From `repo-root`, not for the clone: a RELATIVE `core.hooksPath` resolves
   against each working tree, so it names a different directory in every
   worktree. See `hooks-dirs`."
  [repo-root opts]
  (let [{:keys [exit out]} (run opts ["git" "rev-parse" "--path-format=absolute"
                                      "--git-path" "hooks"] repo-root)]
    (when (zero? exit)
      (let [d (str/trim (str out))]
        (when (seq d) d)))))

(defn worktrees
  "Every working tree of the clone at `repo-root`, main first."
  [repo-root opts]
  (let [{:keys [exit out]} (run opts ["git" "worktree" "list" "--porcelain"] repo-root)]
    (if (zero? exit)
      (->> (str/split-lines (str out))
           (keep #(when (str/starts-with? % "worktree ")
                    (str/trim (subs % (count "worktree ")))))
           (filter seq)
           vec)
      [repo-root])))

(defn hooks-dirs
  "Every distinct directory git will look in across the clone's worktrees.

   The default `$GIT_DIR/hooks` is shared, so this is one entry and installing
   once covers every worktree. A RELATIVE `core.hooksPath` is not: git resolves
   it against the working tree the hook runs in, so `.githooks` means a
   different directory per worktree, and a hook installed in the main one never
   runs for a push made from a linked one.

   Measured: perpdex-chain-scan sets `core.hooksPath .githooks`; its main tree
   resolved to `<main>/.githooks` and its worktree to `<worktree>/.githooks`,
   and a push from the worktree recorded nothing while the hook sat in the
   main tree. cchp, on the default path, resolved both to the same directory."
  [repo-root opts]
  (->> (worktrees repo-root opts)
       (keep #(hooks-dir % opts))
       distinct
       vec))

(defn- tracked?
  "True when `path` is a file git tracks. Installing over one would show as a
   modification and could be committed into the user's repository."
  [repo-root path opts]
  (zero? (:exit (run opts ["git" "ls-files" "--error-unmatch" (str path)] repo-root))))

(defn- exclude-locally!
  "Adds `path` to the clone's `info/exclude` when it sits inside a working
   tree, so our hook does not show up as untracked in the user's `git status`.

   Only for a hooks directory that lives in the working tree — a relative
   `core.hooksPath` like `.githooks`. `info/exclude` is in the shared git dir,
   so one entry covers every worktree, and it is not itself tracked."
  [repo-root path opts]
  (let [{:keys [exit out]} (run opts ["git" "rev-parse" "--path-format=absolute"
                                      "--show-toplevel"] repo-root)
        top (when (zero? exit) (str/trim (str out)))
        ce (run opts ["git" "rev-parse" "--path-format=absolute"
                      "--git-common-dir"] repo-root)
        common (when (zero? (:exit ce)) (str/trim (str (:out ce))))]
    (when (and top common (str/starts-with? (str path) (str top "/")))
      (let [rel (subs (str path) (inc (count top)))
            f (fs/path common "info" "exclude")]
        ;; Under the lock AND atomic: a read-modify-write of a file the USER
        ;; owns and keeps their own entries in. `-main` loops over every known
        ;; clone, so two SessionStarts can interleave here, and a kill mid-spit
        ;; would truncate their excludes, not just drop our line.
        (flock/with-file-lock
          (flock/guard-path (str f))
          (fn []
            (let [current (if (fs/exists? f) (slurp (str f)) "")]
              (when-not (some #{rel} (str/split-lines current))
                (atomicfile/spit!
                 (str f)
                 (str current
                      (when (and (seq current) (not (str/ends-with? current "\n"))) "\n")
                      rel "\n"))))))
        rel))))

(defn- ours? [f]
  (and (fs/regular-file? f) (str/includes? (slurp (str f)) marker)))

(defn install-into!
  "Puts `hook-source` in place as `pre-push` in one hooks directory.

   Returns {:status :installed|:updated|:current|:chained|:conflict|:tracked
            :path <path>}."
  [dir repo-root hook-source opts]
  (let [target (fs/path dir "pre-push")
        wanted (slurp hook-source)
        write! (fn [status]
                 (fs/create-dirs dir)
                 ;; Mode set on the temp, before the rename: `spit` then
                 ;; `chmod` leaves a window where the hook exists and is not
                 ;; executable, and a kill inside it leaves a truncated one.
                 (atomicfile/spit! (str target) wanted "rwxr-xr-x")
                 {:status status :path (str target)})]
    ;; Before the cond, not inside `write!`: a hook that is already current is
    ;; never written, and it still has to be excluded — otherwise the first
    ;; install hides it and every later run leaves it showing again.
    (when-not (tracked? repo-root target opts)
      (exclude-locally! repo-root target opts))
    (cond
      ;; A hooks directory can live IN the working tree — that is what a
      ;; relative `core.hooksPath` means — and then the file we would write
      ;; may be one git tracks. Overwriting it would show as a modification in
      ;; the user's repository and could be committed.
      (tracked? repo-root target opts)
      {:status :tracked :path (str target)}

      (not (fs/exists? target))
      (write! :installed)

      (ours? target)
      (if (= wanted (slurp (str target)))
        {:status :current :path (str target)}
        (write! :updated))

      ;; Someone else's hook. Park it under a name ours execs, but only if
      ;; that name is free — a taken one means a previous chain, and
      ;; overwriting it would drop whichever hook is really in use.
      (fs/exists? (fs/path dir chained-name))
      {:status :conflict :path (str target)}

      :else
      (do (Files/move (fs/path target) (fs/path dir chained-name)
                      (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE]))
          ;; No chmod after the move. `Files/move` preserves the mode, and
          ;; forcing "rwxr-xr-x" would rewrite it — including making a hook
          ;; EXECUTABLE that the user had deliberately chmod'd non-executable
          ;; to turn it off. Ours execs the chained hook only when it is `-x`,
          ;; so preserving the mode preserves the user's decision either way.
          (write! :chained)))))

(defn install!
  "Puts `hook-source` in place for EVERY hooks directory the clone's worktrees
   resolve to.

   One directory in the ordinary case, because `$GIT_DIR/hooks` is shared. More
   than one when `core.hooksPath` is relative, which git resolves against each
   working tree — and a hook installed only in the main tree never runs for a
   push made from a linked one. That is not hypothetical: it is why
   perpdex-chain-scan's PR #66 got no review.

   `:status` and `:path` describe the directory `repo-root` itself resolves to,
   so a caller that only cares about \"did this repo get the hook\" is unchanged;
   `:results` carries every directory. Never throws for an ordinary condition:
   this runs from SessionStart, where an exception would be noise on every
   session in every repository that happens not to be a clone."
  [repo-root hook-source opts]
  (let [pairs (->> (worktrees repo-root opts)
                   (keep (fn [wt] (when-let [d (hooks-dir wt opts)] [wt d])))
                   ;; distinct on the hooks DIR: the shared `$GIT_DIR/hooks`
                   ;; appears once per worktree and must be installed once.
                   (reduce (fn [acc [wt d]]
                             (if (some #(= d (second %)) acc) acc (conj acc [wt d])))
                           [])
                   ;; the directory `repo-root` itself resolves to comes first,
                   ;; so :status and :path keep describing this repo
                   (sort-by #(if (= (first %) repo-root) 0 1)))]
    (if (empty? pairs)
      {:status :no-repo :path nil :results []}
      ;; each hooks dir is installed with ITS OWN worktree as the root, so
      ;; `exclude-locally!` resolves the top-level the target actually sits in
      (let [results (mapv (fn [[wt d]] (install-into! d wt hook-source opts)) pairs)]
        (assoc (first results) :results results)))))

(defn hook-source
  "The tracked hook body, next to this namespace's parent directory."
  []
  (str (fs/path (or (some-> (System/getenv "CLAUDE_PLUGIN_ROOT") str)
                    (some-> (System/getProperty "babashka.config") fs/parent str)
                    (System/getProperty "user.dir"))
                "hooks" "pr-push-hook.sh")))

(defn -main
  "SessionStart entrypoint. Installs into the session's own clone, and
   refreshes every clone already known to have pushed.

   Exits 0 unconditionally and says nothing on success. A SessionStart hook
   that fails, or that prints, does so at the top of every session in every
   directory — including the many that are not clones at all — so the only
   thing worth reporting is a refusal the user has to act on."
  [& _]
  (let [input (try (json/parse-string (slurp *in*) true) (catch Exception _ nil))
        src (hook-source)
        roots (cons (:cwd input)
                    (keep #(gh/main-worktree % {})
                          (attempts/clones-since (attempts/default-log) 0)))
        results (for [r (distinct (remove nil? roots))]
                  (assoc (try (install! r src {})
                              (catch Exception e {:status :error :path (ex-message e)}))
                         :root r))]
    (doseq [{:keys [status root path]} results]
      (when (= :conflict status)
        (binding [*out* *err*]
          (println (str "pr-review-loop: " root " already has a pre-push hook and "
                        path ".pr-review-chained is taken, so nothing was changed."
                        " Chain it by hand, or move the parked file aside.")))))
    (System/exit 0)))
