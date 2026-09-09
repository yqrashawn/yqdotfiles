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
            [pr-review.cloneindex :as cloneindex]
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
  "The directory git will actually look in, or nil outside a repository."
  [repo-root opts]
  (let [{:keys [exit out]} (run opts ["git" "rev-parse" "--path-format=absolute"
                                      "--git-path" "hooks"] repo-root)]
    (when (zero? exit)
      (let [d (str/trim (str out))]
        (when (seq d) d)))))

(defn- ours? [f]
  (and (fs/regular-file? f) (str/includes? (slurp (str f)) marker)))

(defn install!
  "Puts `hook-source` in place as `pre-push` for the clone at `repo-root`.

   Returns {:status :installed|:updated|:current|:chained|:conflict|:no-repo
            :path <path or nil>}. Never throws for an ordinary condition: this
   runs from SessionStart, where an exception would be noise on every session
   in every repository that happens not to be a clone."
  [repo-root hook-source opts]
  (if-let [dir (hooks-dir repo-root opts)]
    (let [target (fs/path dir "pre-push")
          wanted (slurp hook-source)
          write! (fn [status]
                   (fs/create-dirs dir)
                   (spit (str target) wanted)
                   (fs/set-posix-file-permissions target "rwxr-xr-x")
                   {:status status :path (str target)})]
      (cond
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
            (fs/set-posix-file-permissions (fs/path dir chained-name) "rwxr-xr-x")
            (write! :chained))))
    {:status :no-repo :path nil}))

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
                          (cloneindex/clones-since (cloneindex/default-log) 0)))
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
