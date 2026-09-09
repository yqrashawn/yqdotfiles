(ns pr-review.gh
  "Thin, injectable shell layer over git and gh.

   This namespace runs inside a hook subprocess, not as a Claude Code tool
   call, so rtk's PreToolUse rewriter never sees these commands. That is the
   whole reason the diff produced here is the real diff: `rtk git diff HEAD~1`
   returns 195 bytes where plain git returns 80162.

   Every function takes an opts map with an optional :sh so tests can stub the
   shell without a real repo."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.string :as str]))

(defn default-sh
  [args dir]
  (let [{:keys [exit out err]} (p/sh args {:dir dir})]
    {:exit exit :out (or out "") :err (or err "")}))

(defn- run
  [{:keys [sh]} args dir]
  (try ((or sh default-sh) args dir)
       (catch Exception e {:exit 127 :out "" :err (str (ex-message e))})))

(defn- ok-out
  "Trimmed stdout on exit 0, else nil. Every caller degrades to nil rather
   than throwing — a hook that crashes loses the pass with no diagnosis."
  [{:keys [exit out]}]
  (when (zero? exit)
    (let [s (str/trim out)]
      (when-not (str/blank? s) s))))

(defn repo-root
  [cwd opts]
  (ok-out (run opts ["git" "rev-parse" "--show-toplevel"] cwd)))

(defn git-common-dir
  "Absolute path to the git directory shared by every worktree of this clone.

   Never assume `<repo-root>/.git`. In a linked worktree that path is a
   *file*, so anything that mkdirs it throws FileAlreadyExistsException and
   anything that reads state under it finds none — which is why, before this
   existed, every push from a worktree exited 1 and the cap and re-raise
   rules never engaged there at all.

   `--git-common-dir` also gives the sharing the loop wants: a PR is reviewed
   per repository, not per worktree, so all worktrees of one clone must share
   one ledger, one lock and one context directory or the 10-pass cap and the
   single-reviewer lock mean nothing across them.

   git prints it relative to `repo-root` in an ordinary clone (\".git\") and
   absolute inside a worktree; both resolve correctly against `repo-root`.
   Returns nil when git fails, and every caller falls back to
   `<repo-root>/.git`."
  [repo-root opts]
  (when-let [d (ok-out (run opts ["git" "rev-parse" "--git-common-dir"] repo-root))]
    (str (fs/normalize (fs/path repo-root d)))))

(defn current-branch
  "Branch name, or nil on a detached HEAD (git prints the literal \"HEAD\")."
  [repo-root opts]
  (let [b (ok-out (run opts ["git" "rev-parse" "--abbrev-ref" "HEAD"] repo-root))]
    (when (and b (not= "HEAD" b)) b)))

(defn head-sha
  [repo-root opts]
  (ok-out (run opts ["git" "rev-parse" "HEAD"] repo-root)))

(defn merge-base
  [repo-root base-ref opts]
  (ok-out (run opts ["git" "merge-base" (str "origin/" base-ref) "HEAD"] repo-root)))

(defn diff
  "Full diff of `base...sha`, or nil if the diff command itself failed (e.g.
   an unresolved base ref) — distinct from a successful diff that is merely
   empty, which returns \"\". Three-dot so the review sees only this branch's
   work, not everything that landed on the base since it forked."
  [repo-root base sha opts]
  ;; Bypasses ok-out on purpose: the reviewer trusts these bytes unseen, so trimming git's trailing newline here would silently corrupt the one file the whole module exists to keep faithful.
  ;;
  ;; --no-ext-diff and --no-color, because the user's git decides the format
  ;; otherwise and this repo's `diff.external` is difftastic side-by-side.
  ;; Every review before this flag read that instead of a unified diff:
  ;; measured, the real context diff for PR #395 held 0 `diff --git` lines, so
  ;; `changed-files` was always empty and the prompt's "Changed files" section
  ;; always blank. The prompt tells the reviewer it is reading a diff and the
  ;; fingerprint identity across passes is `path:line`, so the format is not
  ;; cosmetic. --no-pager as well: a configured pager would be a second way
  ;; for a machine-read diff to acquire decoration.
  (let [{:keys [exit out]} (run opts ["git" "--no-pager" "diff" "--no-ext-diff"
                                      "--no-color" (str base "..." sha)]
                                repo-root)]
    (when (zero? exit) out)))

(defn open-pr
  "The open PR whose head is `branch`, or nil. Measured at ~1.3s.

   `headRefOid` is requested because the trigger matches it against the sha
   git recorded in the reflog. A pre-push hook runs before the push, so a
   rejected push still leaves a reflog-shaped candidate; requiring the PR to
   actually point at that sha is what proves the push landed."
  [repo-root branch opts]
  (let [res (run opts ["gh" "pr" "list" "--head" branch "--state" "open"
                       "--json" "number,isDraft,baseRefName,headRefOid"]
                 repo-root)]
    (when (zero? (:exit res))
      (try (first (json/parse-string (:out res) true))
           (catch Exception _ nil)))))

(defn main-worktree
  "The clone's main worktree, given only its git directory, or nil.

   The trigger starts from a git dir — that is what the `pre-push` hook
   records — and `gh` needs a directory inside a checkout to resolve the
   remote. `<git-dir>/..` is not that directory in general: with
   `--separate-git-dir`, or for a git dir that is not named `.git`, it is
   somewhere else entirely. `worktree list` answers exactly, and its first
   entry is always the main worktree."
  [git-dir opts]
  (let [{:keys [exit out]} (run opts ["git" (str "--git-dir=" git-dir)
                                      "worktree" "list" "--porcelain"] nil)]
    (when (zero? exit)
      (some->> (str/split-lines (str out))
               (some #(when (str/starts-with? % "worktree ") %))
               (#(subs % (count "worktree ")))
               str/trim
               not-empty))))
