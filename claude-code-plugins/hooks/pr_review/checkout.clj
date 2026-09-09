(ns pr-review.checkout
  "A throwaway detached worktree at the reviewed sha.

   The reviewer used to read the agent's live working tree, which the agent is
   still editing — so a review could describe a file that had already changed,
   and two runs of the same pass were not reproducible. A worktree pinned to
   the sha under review fixes that by construction: the tree cannot move while
   the review runs, and it is exactly the code the PR points at.

   `git --git-dir=<gd> worktree add --detach` works from the git directory
   alone, which is all the `pre-push` hook records — no main checkout has to be
   located or borrowed. Verified against a real clone: 37 entries checked out
   at a named sha and removed cleanly.

   Detached, never a branch: checking the branch out would fight the agent's
   own worktree over the same ref."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]))

(defn- run
  [opts args]
  ((or (:sh opts) (fn [a d] (p/sh a {:dir d}))) args nil))

(defn- git [git-dir opts & args]
  (run opts (into ["git" (str "--git-dir=" git-dir)] args)))

(defn prune!
  "Drops worktree administrative entries whose directory is gone.

   A reviewer killed mid-run — a session restart does exactly this, measured —
   leaves its checkout registered. Git refuses to reuse the path until the
   stale entry is pruned, so an un-pruned leak would eventually stop every
   later review in the clone."
  [git-dir opts]
  (zero? (:exit (git git-dir opts "worktree" "prune"))))

(def stale-after-ms
  "How old a review checkout has to be before it is treated as leaked.

   Twelve hours, against a review that takes about seven minutes: the margin
   is what makes age alone a safe test. A running reviewer's checkout is
   minutes old, so nothing in flight can be inside this window, and no
   correlation with the lock records is needed to avoid pulling a tree out
   from under a live review."
  (* 12 60 60 1000))

(def ^:private review-dir-re
  "Exactly what `add!` names its checkouts: `pr-review-` and the sha prefix.

   Not a bare `pr-review-` prefix test. That also matched a clone whose own
   directory happened to start with those characters — caught by a test whose
   temp clone was named `pr-review-checkout...`, which would have made the
   MAIN worktree a pruning candidate."
  #"^pr-review-[0-9a-f]{1,40}$")

(defn- review-worktrees
  "Registered worktree paths this namespace created, from `worktree list`.

   The first entry `worktree list` prints is always the main worktree, and it
   is dropped before anything else is considered: nothing this function can
   return may be the clone itself."
  [git-dir opts]
  (->> (str/split-lines (str (:out (git git-dir opts "worktree" "list" "--porcelain"))))
       (keep #(when (str/starts-with? % "worktree ")
                (str/trim (subs % (count "worktree ")))))
       (drop 1)
       (filter #(re-matches review-dir-re (str (fs/file-name %))))))

(defn prune-stale!
  "Removes review checkouts older than `stale-after-ms`, and returns how many.

   `worktree prune` only drops entries whose DIRECTORY IS GONE, so it cannot
   reclaim the case that actually happens: a reviewer killed while its temp
   directory survives leaves a registered worktree that git will keep
   refusing to reuse. Measured — a review killed at the two-minute mark left
   exactly that, and prune reported nothing prunable.

   Age comes from the directory's own mtime rather than from any record we
   keep, so a leak from a previous version of this code is reclaimed too."
  [git-dir opts]
  (let [cutoff (- (System/currentTimeMillis) stale-after-ms)]
    (count
     (for [d (review-worktrees git-dir opts)
           :when (and (fs/exists? d)
                      (< (.toMillis (fs/last-modified-time d)) cutoff))]
       (do (git git-dir opts "worktree" "remove" "--force" d)
           (fs/delete-tree d)
           d)))))

(defn add!
  "Checks `sha` out into a fresh directory under `parent`. Returns the path,
   or nil if git refused."
  [git-dir sha parent opts]
  (let [dir (str (fs/path parent (str "pr-review-" (subs sha 0 (min 12 (count sha))))))]
    (fs/delete-tree dir)
    (when (zero? (:exit (git git-dir opts "worktree" "add" "--detach" "-q" dir sha)))
      dir)))

(defn remove!
  "Unregisters and deletes the checkout. `--force` because the reviewer is
   read-only by construction but a crash can still leave the tree dirty, and a
   refused removal would leak the directory."
  [git-dir dir opts]
  (let [ok (zero? (:exit (git git-dir opts "worktree" "remove" "--force" dir)))]
    (when-not ok (fs/delete-tree dir))
    (prune! git-dir opts)
    ok))

(defn with-checkout
  "Calls `(f dir)` with a detached checkout of `sha`, removing it afterwards
   whichever way `f` ends. Calls `(f nil)` if the checkout could not be made,
   so the caller decides whether that is fatal rather than seeing an
   exception from here.

   Stale entries are pruned FIRST, both kinds: `prune!` for entries whose
   directory is gone, `prune-stale!` for leaked checkouts whose directory
   survives. The path is derived from the sha, so a previous killed run at
   the same sha would otherwise own it forever."
  [git-dir sha parent opts f]
  (prune! git-dir opts)
  (prune-stale! git-dir opts)
  (let [dir (add! git-dir sha parent opts)]
    (try (f dir)
         (finally (when dir (remove! git-dir dir opts))))))
