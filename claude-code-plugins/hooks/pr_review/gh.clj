(ns pr-review.gh
  "Thin, injectable shell layer over git and gh.

   This namespace runs inside a hook subprocess, not as a Claude Code tool
   call, so rtk's PreToolUse rewriter never sees these commands. That is the
   whole reason the diff produced here is the real diff: `rtk git diff HEAD~1`
   returns 195 bytes where plain git returns 80162.

   Every function takes an opts map with an optional :sh so tests can stub the
   shell without a real repo."
  (:require [babashka.process :as p]
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
  "Full diff of `base...sha`. Three-dot so the review sees only this branch's
   work, not everything that landed on the base since it forked."
  [repo-root base sha opts]
  ;; Bypasses ok-out on purpose: the reviewer trusts these bytes unseen, so trimming git's trailing newline here would silently corrupt the one file the whole module exists to keep faithful.
  (let [{:keys [exit out]} (run opts ["git" "diff" (str base "..." sha)] repo-root)]
    (if (zero? exit) out "")))

(defn open-pr
  "The open PR whose head is `branch`, or nil. Measured at ~1.3s."
  [repo-root branch opts]
  (let [res (run opts ["gh" "pr" "list" "--head" branch "--state" "open"
                       "--json" "number,isDraft,baseRefName"]
                 repo-root)]
    (when (zero? (:exit res))
      (try (first (json/parse-string (:out res) true))
           (catch Exception _ nil)))))
