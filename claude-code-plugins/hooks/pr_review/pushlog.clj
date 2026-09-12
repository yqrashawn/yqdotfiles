(ns pr-review.pushlog
  "Pushes, read from git's own remote-tracking reflogs.

   This replaces every attempt to learn what was pushed by reading the shell
   command. Five separate defects came out of that approach — a worktree the
   session cwd did not name, `gh pr create` skipped for its heredoc, rtk
   silent on heredocs, `$(...)` before the push, a two-level variable chain —
   and each fix covered one more command shape while the next shape broke
   again. Git already records the fact, exactly, for every push however the
   command was written:

     44e4bbf9 ceab8f69 Name <mail> 1788929441 +0800\\tupdate by push

   Old sha, new sha, branch and timestamp, in
   `<git-dir>/logs/refs/remotes/<remote>/<branch>`. It is written only when a
   push SUCCEEDS, which a `pre-push` hook cannot know, so a push that was
   rejected leaves nothing here to review.

   `update by push` is the exact message and it excludes every fetch: in this
   clone, 72 push entries against 297 fetch entries across 439 files.

   The record's lifetime is the remote-tracking ref's. Deleting the branch on
   the remote — which merging a PR normally does — prunes the ref and this log
   with it. Measured on PR #396: merged, branch gone, reflog gone, while both
   still-open PRs kept theirs. So the record expires precisely when there is
   no longer anything to review, and a merged PR cannot be resurrected from
   here.

   This also answers `gh pr create`, which pushes nothing and would otherwise
   have no record at all. The branch was pushed moments earlier, that entry is
   still here, and the PR now exists — so both trigger commands reduce to one
   question: is there a push whose new sha is the head of an open PR with no
   ledger row? Verified against the real #395 and #397 pushes, the two the
   command parser resolved to the wrong repository."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]))

(def ^:private push-message
  "Git's reflog message for a remote-tracking ref updated by a push."
  "update by push")

(def ^:private line-re
  ;; <old> <new> <who> <ts> <tz>\t<message>. The identity is skipped rather
  ;; than parsed: it holds a name and address this namespace has no use for.
  #"^([0-9a-f]{40}) ([0-9a-f]{40}) .* (\d+) [-+]\d{4}\t(.*)$")

(defn parse-line
  "One reflog line as {:old-sha :new-sha :ts :message}, or nil if it is not
   one. `:ts` is milliseconds, to match the ledger's clock."
  [line]
  (when-let [[_ old new ts msg] (re-matches line-re (str/trimr line))]
    {:old-sha old :new-sha new :ts (* 1000 (parse-long ts)) :message msg}))

(defn- ref-of
  "The remote and branch a reflog file stands for, or nil.

   `logs/refs/remotes/origin/fix/a-b` is remote `origin`, branch `fix/a-b`:
   a remote name cannot contain a slash, so it is exactly the first segment
   and the branch is everything after it. `origin/HEAD` is the remote's
   default-branch symref, never a push target."
  [git-dir file]
  (let [rel (str (fs/relativize (fs/path git-dir "logs" "refs" "remotes") file))
        [remote & rest] (str/split rel #"/")]
    (when (and remote (seq rest))
      (let [branch (str/join "/" rest)]
        (when-not (= "HEAD" branch)
          {:remote remote :branch branch})))))

(defn pushes-since
  "Every successful push recorded in `git-dir` at or after `since-ms`, newest
   first, as {:remote :branch :old-sha :new-sha :ts}.

   File mtime prefilters the walk. It is a superset — a fetch touches the same
   file — so it only avoids reading, never decides."
  [git-dir since-ms]
  (let [root (fs/path git-dir "logs" "refs" "remotes")]
    (when (fs/directory? root)
      (->> (fs/glob root "**")
           (filter fs/regular-file?)
           (filter #(>= (.toMillis (fs/last-modified-time %)) since-ms))
           (mapcat (fn [f]
                     (when-let [r (ref-of git-dir f)]
                       (->> (str/split-lines (slurp (str f)))
                            (keep parse-line)
                            (filter #(= push-message (:message %)))
                            (filter #(>= (:ts %) since-ms))
                            (map #(merge r (dissoc % :message)))))))
           (sort-by :ts >)))))
