(ns pr-review.trigger
  "PostToolUse hook entrypoint.

   Exit 0 means silence. Exit 2 wakes agent A with whatever this process wrote
   to stderr — and only stderr: the harness discards a hook's stdout even when
   stderr is empty. Never exit anything else; other codes produce a
   `Failed with non-blocking status code:` notice and the pass is lost.

   That contract is why every filesystem-touching call below sits inside
   `review!`'s try, `lock/acquire!` included. A worktree used to make
   `acquire!` throw from the `case` head — outside the try — and babashka
   exited 1 on every push."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [pr-review.checkout :as checkout]
            [pr-review.attempts :as attempts]
            [pr-review.context :as context]
            [pr-review.gh :as gh]
            [pr-review.ledger :as ledger]
            [pr-review.lock :as lock]
            [pr-review.prompt :as prompt]
            [pr-review.pushlog :as pushlog]
            [pr-review.reviewer :as reviewer]))

(def ^:private context-keep 5)

(def ^:private abandoned-artifact-ms
  "How long a per-PR artifact outlives its last write. Every retention rule
   here became per PR — the lock, the checkout, the diff — and nothing bounds
   the number of PRs, so a merged PR's files stayed forever: measured, 33 PRs
   in one clone, five diffs each, this PR's diff alone 667 KB.

   Two weeks is far longer than any review takes and longer than a PR stays
   active here, and everything swept is rewritten by the next pass if the PR
   comes back."
  (* 14 24 60 60 1000))

(def ^:private push-slack-ms
  "Added to the tool call's own duration when looking back for the push it
   made. A PostToolUse hook fires when the Bash call returns, so the reflog
   entry it wants is seconds old; this is slack for clock granularity — the
   reflog stores whole seconds — not a window."
  (* 60 1000))

(def ^:private pr-head-attempts
  "How many times to ask GitHub for the PR head before giving up on it.

   Measured: a push recorded at 20:25:31 had its PostToolUse trigger fire at
   20:25:32, and the PR's `updated_at` shows GitHub only moved the head at
   20:25:34. `gh pr list` takes ~1.3s on top, so the query lands squarely
   inside the propagation window. The mismatch made the trigger drop the push
   in silence -- with no lock written, so nothing retried it either.

   For a push to an EXISTING PR the first ask races every time, by
   construction: GitHub has to move the head. So this is the normal path, not
   an exceptional one, and the shape matters. A fixed delay before asking --
   the obvious alternative -- pays its full cost on every trigger including
   the ones with nothing to review, and still drops the push whenever GitHub
   takes longer than the guess. Backing off from a mismatch costs nothing when
   the head is already right (`gh pr create`, or a trigger that fired late)
   and returns as soon as GitHub catches up."
  6)

(defn- pr-head-delay-ms
  "Exponential from 1s, capped at 8s, plus up to a second of jitter: about 24s
   of total patience across `pr-head-attempts`.

   The jitter is for concurrent pushes, which this workflow does constantly --
   three sessions pushing within a minute of each other would otherwise line
   their retries up and ask GitHub in lockstep."
  [attempts-left rand-fn]
  (let [n (- pr-head-attempts attempts-left)]
    (+ (* 1000 (min 8 (bit-shift-left 1 n)))
       (rand-fn 1000))))

(def ^:private create-lookback-ms
  "How far back `gh pr create` looks. It pushes nothing, so the entry it needs
   belongs to the push that created the branch — measured at 29s and 69s
   before the create in the two real cases. An hour is generous because the
   ledger, not this window, is what stops a sha being reviewed twice; the
   window's only job is R2, keeping a human's terminal push from being
   attributed to an agent."
  (* 60 60 1000))

(defn core-prompt
  "The generic review core, read off the classpath.

   `hooks/` is a :paths root in bb.edn, so `review_core.md` resolves as a
   resource regardless of the working directory and without CLAUDE_PLUGIN_ROOT —
   which is set only for hooks declared in a plugin's hooks/hooks.json, and is
   therefore nil when the /review command invokes this same entrypoint."
  []
  (if-let [r (io/resource "review_core.md")]
    (slurp r)
    (throw (ex-info "review_core.md not on the classpath" {}))))

(defn- trigger-verb
  "Which trigger command the agent ran, or nil. A CHEAP FILTER ONLY.

   R2 no longer rests on this. The `pre-push` hook records the pushing
   session, so whether a push was made by an agent is decided by what made it
   rather than by how the command was spelled — which is what lets `git -C /x
   push`, aliases and scripts through, none of which any command-text match
   can see. What this still buys is not spawning a reviewer process on every
   Bash call whose `if` filter fails open, and telling the two lookback
   windows apart.

   A command that matches nothing is not refused outright — that is the
   mistake this design exists to stop making, and it is how `git -C /x push`
   was lost. It is narrowed instead: with no verb, only a push made by THIS
   session counts. So the session that actually pushed still gets its review
   on any later Bash call, while an unrelated session running an unrelated
   command cannot start one. Two accidental reviews of PR #395 came from
   exactly that — one from the agent's own `gh pr view`, one from a
   diagnostic command of mine."
  [command]
  (let [c (str command)]
    (cond
      (re-find #"\bgh\s+pr\s+create\b" c) :create
      (re-find #"\bgit\s+push\b" c)        :push
      :else nil)))

(defn- lookback-ms
  [verb duration-ms]
  (if (= :push verb)
    (+ (or duration-ms 0) push-slack-ms)
    create-lookback-ms))

(defn- agent-pushed
  "Index of [git-dir branch sha] -> best attempt, over attempts made from
   inside a Claude session.

   `best` prefers this session's own attempt, so a review wakes the session
   that made the push. It does not REQUIRE it: a subagent, or a future change
   to how the session id is exported, would otherwise silently stop every
   review — and a review whose wake lands in a sibling session is far better
   than no review at all."
  [since session opts]
  (let [log ((or (:log-fn opts) attempts/default-log))]
    (->> ((or (:attempts-fn opts) attempts/attempts-since) log since)
         (filter attempts/by-agent?)
         (reduce (fn [m {:keys [git-dir branch sha] :as a}]
                   (let [k [git-dir branch sha]
                         cur (get m k)]
                     (if (or (nil? cur)
                             (and (= session (:session a))
                                  (not= session (:session cur))))
                       (assoc m k a)
                       m)))
                 {}))))

(defn- candidate-pushes
  "Pushes that BOTH landed and were made by an agent, globally newest first.

   Three sources, no inference. `pr-review.attempts` says which clones and who
   pushed; `pr-review.pushlog` reads git's own reflog, which is written only
   when a push succeeds; the intersection is a push that really happened and
   really came from an agent. A rejected push leaves an attempt and no reflog
   entry; a human's leaves both but with no session.

   The sort is across clones, not within one, because the decision takes the
   newest candidate overall."
  [since session verb opts]
  (let [by-agent (agent-pushed since session opts)]
    (->> (keys by-agent)
         (map first)
         distinct
         (mapcat (fn [git-dir]
                   (map #(assoc % :git-dir git-dir)
                        ((or (:pushes-fn opts) pushlog/pushes-since) git-dir since))))
         (keep (fn [{:keys [git-dir branch new-sha] :as p}]
                 (when-let [a (get by-agent [git-dir branch new-sha])]
                   (assoc p :session (:session a)))))
         ;; No trigger verb in the command: only this session's own push
         ;; counts. See `trigger-verb`.
         (filter #(or (some? verb) (= session (:session %))))
         (sort-by :ts >))))

(defn- abandoned-candidates
  "Reviews that started and were killed before writing a ledger row, shaped
   like reflog candidates so one selection path serves both.

   Deliberately NOT limited by the lookback window, and deliberately NOT
   scoped to this session. A killed review's push is minutes or hours old and
   the session that made it is gone — a restart is the commonest way to kill
   one — so any window or session test would mean it is never retried, which
   is the defect. R2 is not weakened by that: a lock record exists only
   because a review already started, so the push behind it was provably an
   agent's, and `actionable` still requires the PR to be open at that sha with
   no ledger row.

   Every clone the log knows, with no `since`. There is no time bound here on
   purpose — a killed review is retried however old its push is — and none is
   needed: `attempts/tail-lines` caps the bytes read, and what comes back is
   distinct directories that still exist, measured at 4.

   `:session` is filled from the push log rather than left out. It is what
   becomes `:pushed-by`, and this is the ONE path that needs it: a reflog
   candidate already carries the session that pushed, while a retry is
   deliberately delivered to whichever session happens to be running, which is
   routinely not the one whose PR it is."
  [opts]
  (let [log ((or (:log-fn opts) attempts/default-log))
        pusher-of (or (:pusher-fn opts) attempts/pusher)]
    (->> ((or (:clones-fn opts) attempts/clones-since) log 0)
         (mapcat (fn [git-dir]
                   (map #(assoc % :git-dir git-dir)
                        ((or (:abandoned-fn opts) lock/abandoned) git-dir))))
         (keep (fn [{:keys [branch sha started] :as a}]
                 (when (and branch sha)
                   (let [git-dir (:git-dir a)
                         who (pusher-of log git-dir branch sha)]
                     (cond-> {:git-dir git-dir :branch branch :new-sha sha
                              :ts (or started 0) :retry? true}
                       who (assoc :session who))))))
         (sort-by :ts >))))

(defn- pr-at-sha
  "The open PR for `branch` once its head is `sha`, or nil.

   Re-asks on a mismatch rather than treating it as an answer. A mismatch has
   two causes and they need opposite handling: GitHub has not caught up yet
   (wait), or this push was superseded by a newer one (drop). Only time tells
   them apart, so it waits a bounded amount and then drops -- a superseded
   push costs at most `pr-head-attempts` * `pr-head-delay-ms` of sleep in a
   background hook, and the newer push has its own trigger."
  [root branch sha opts]
  (let [fetch (or (:open-pr-fn opts) gh/open-pr)
        sleep (or (:sleep-fn opts) #(Thread/sleep %))
        rnd   (or (:rand-fn opts) rand-int)]
    (loop [n (or (:pr-head-attempts opts) pr-head-attempts)]
      ;; `when-let`, not `let`: no PR at all is a final answer, not a lag.
      ;; Waiting on it would put the whole budget into every Bash call whose
      ;; branch has no PR, which is most of them.
      (when-let [pr (fetch root branch opts)]
        (cond
          (= sha (:headRefOid pr)) pr
          (<= n 1) nil
          :else (do (sleep (pr-head-delay-ms n rnd))
                    (recur (dec n))))))))

(defn- actionable
  "The newest candidate that has an open PR at its pushed sha AND no ledger
   row for that sha, with the clone's main worktree, the PR and the already
   read passes attached. nil if none is.

   The sha equality is load-bearing twice over. It proves the push actually
   landed — a rejected push leaves no reflog entry at all, but a superseded
   one leaves a stale entry whose sha the PR no longer points at — and it
   makes `gh pr create` need no special case, since the branch's earlier push
   entry becomes reviewable the moment the PR exists.

   An already-reviewed candidate is SKIPPED rather than ending the search.
   Two branches are routinely pushed before either PR is opened, so the newest
   push is frequently one already reviewed while an older one is not; stopping
   at the newest would leave that PR unreviewed for good. This is why the
   ledger is read here and threaded out, rather than consulted afterwards."
  [cands opts]
  (some (fn [{:keys [git-dir branch new-sha] :as c}]
          (when-let [root ((or (:main-worktree-fn opts) gh/main-worktree) git-dir opts)]
            (when-let [pr (pr-at-sha root branch new-sha opts)]
              (let [passes (ledger/read-passes git-dir (:number pr))]
                (when-not (ledger/reviewed-sha? passes new-sha)
                  (assoc c :repo-root root :pr-info pr :passes passes))))))
        cands))

(defn- decide-outside-a-reviewer
  [input opts]
  (let [verb (trigger-verb (get-in input [:tool_input :command]))
        now ((or (:now-fn opts) #(System/currentTimeMillis)))
        since (- now (lookback-ms verb (:duration_ms input)))
        cands (concat (candidate-pushes since (:session_id input) verb opts)
                      (abandoned-candidates opts))]
    (if-let [{:keys [git-dir repo-root branch new-sha pr-info passes retry? session]}
             (actionable cands opts)]
      (let [pr-num (:number pr-info)
            ;; :pr-url and :pushed-by ride on the decision, not just the
            ;; manual path. Without them the prompt's "What the author says
            ;; this change does" section was silently absent on EVERY
            ;; hook-triggered review (prompt.clj gates on :pr-url), and the
            ;; wake never named the pushing session — which matters because
            ;; `abandoned-candidates` is deliberately neither session- nor
            ;; verb-scoped, so a wake routinely lands somewhere that did not
            ;; push. `gh/open-pr` already requests `url` for exactly this.
            base (cond-> {:trigger verb :candidates (count cands)
                          :git-dir git-dir :repo-root repo-root
                          :branch branch :pr pr-num :sha new-sha
                          :pr-url (:url pr-info)}
                   session (assoc :pushed-by session)
                   retry? (assoc :retry? true))]
        (if (ledger/cap-reached? passes)
          (assoc base :action :cap-reached
                 :reason (str "review cap of " ledger/max-passes
                              " passes reached for PR #" pr-num))
          (assoc base :action :review
                 :pass (ledger/next-pass-number passes)
                 :base-ref (:baseRefName pr-info)
                 :draft? (boolean (:isDraft pr-info))
                 :prior-fingerprints (ledger/suppressed-fingerprints passes))))
      {:action :silent :trigger verb :candidates (count cands)
       :reason (str "none of " (count cands)
                    " agent pushes is an unreviewed open-PR head")})))

(defn decide
  "Pure decision from the hook input. No side effects, no spawning.

   One question, asked the same way for both trigger commands: is there a push
   whose new sha is the head of an open PR with no ledger row?

   Nothing here reads the payload's `cwd`, and nothing parses the command for
   a directory. Both were tried. `cwd` is the SESSION's directory, and an
   agent that pushes from a worktree is on another branch of another checkout;
   parsing the command to find that worktree produced five separate defects
   and still missed 25 of 260 real push commands. Git knows, so git is asked.

   A candidate must have BOTH landed and been made by an agent: git's reflog
   proves the first, the `pre-push` hook's session id the second. That is R2
   without consulting the command text, so `git -C /x push` and any alias or
   script are caught, and the user's own terminal push never is.

   `:trigger` and `:candidates` ride on every decision, silent ones included:
   which lookback a review came through, and how many agent pushes were in
   scope, are the first things to ask when one lands on the wrong PR or on
   none."
  [input opts]
  (if ((or (:reviewer-env-fn opts) #(System/getenv reviewer/reviewer-env-var)))
    ;; Inside a reviewer. It is a plain `claude -p`, so it loads this very
    ;; plugin and its Bash calls fire this hook -- measured, a review-trigger
    ;; process spawning inside the reviewer. Most such triggers would go
    ;; silent, but the abandoned-review path is deliberately neither
    ;; session-scoped nor verb-gated, so one can find outstanding work and
    ;; start a REVIEW INSIDE A REVIEW: a grandchild that dies when the outer
    ;; reviewer exits, leaving a dead lock and a leaked worktree.
    {:action :silent :trigger nil
     :reason "running inside a reviewer; a review must not review"}
    (decide-outside-a-reviewer input opts)))

(defn- sweep-abandoned-artifacts!
  "Delete per-PR files nothing will write again. Returns how many were removed.

   `context/prune!` bounds a LIVE PR's diffs; this bounds the PRs. Age-based
   rather than PR-state-based, so it needs no GitHub call and cannot reach a
   running review's file — those are minutes old, not weeks.

   Best effort: housekeeping must never fail a review."
  [git-dir]
  (try
    (let [cutoff (- (System/currentTimeMillis) abandoned-artifact-ms)
          stale  (->> (concat (fs/glob (context/context-dir git-dir) "*.diff")
                              (fs/glob git-dir "pr-review.*.findings.md")
                              (fs/glob git-dir "pr-review.*.stderr"))
                      (filter #(< (.toMillis (fs/last-modified-time %)) cutoff)))]
      (doseq [f stale] (fs/delete-if-exists f))
      (count stale))
    (catch Exception _ 0)))

(defn findings-path
  "Where the latest pass's findings text is kept, so a session that did not
   receive the wake can still read it."
  [git-dir pr]
  (str git-dir "/pr-review." pr ".findings.md"))

(defn findings-comment
  "What gets posted to the PR: the review, and nothing addressed to agent A.

   Deliberately not `findings-message`. That one ends with instructions for
   the session being woken — use the skill, hand the path over, do not act on
   someone else's PR — which is noise to anyone reading the PR on GitHub, and
   in the ownership line's case actively confusing."
  [{:keys [pass sha retry?]} parsed warnings]
  (str "## pr-review-loop — pass " pass
       (when retry? " (retried after an interrupted review)")
       "\n\n`" (subs (str sha) 0 (min 12 (count (str sha)))) "` — **"
       (:verdict parsed) "**\n\n"
       (:body parsed)
       (when (seq warnings)
         (str "\n\n" (str/join "\n" warnings)))))

(defn findings-message
  "The text agent A will see. The harness prefixes it with a fixed, unhelpful
   wrapper and ignores rewakeMessage for third-party plugins, so this string
   has to introduce itself."
  ([d parsed] (findings-message d parsed nil))
  ([{:keys [branch pr pass retry? git-dir pushed-by]} parsed warnings]
   (str "pr-review-loop — " branch
        " PR #" pr ", pass " pass
        (when retry? " (retried after an interrupted review)")
        ": " (:verdict parsed) "\n\n"
        (:body parsed)
        (when (seq warnings)
          (str "\n\n" (str/join "\n" warnings)))
        "\n\nThese findings are also at " (findings-path git-dir pr)
        (when pushed-by
          (str ", and this PR was pushed by session " pushed-by))
        ". If that is not you, hand that path over rather than acting on it or"
        " re-running the review."
        "\n\n"
        ;; The verdict was reported and then buried. Measured on PR #410: two
        ;; MERGEABLE passes, both delivered, and the agent fixed the
        ;; [coverage] finding and pushed again both times instead of merging
        ;; — because nothing in the message said MERGEABLE means it may. On a
        ;; test-only PR every pass finds another coverage nit, and coverage
        ;; findings are never suppressed by the re-raise rules, so it cannot
        ;; converge by itself. The verdict has to carry its own instruction.
        (if (= "MERGEABLE" (:verdict parsed))
          (str "MERGEABLE means you may merge this PR now. Only correctness"
               " findings are work you owe:"
               "\n  [correctness/followup] — real but bounded. Open a"
               " follow-up PR; do not fix it here."
               "\n  [coverage] [docs-accuracy] [style] — your judgment. Fix"
               " one only if you independently agree STRONGLY that it is"
               " worth a commit;"
               " otherwise say you are dropping it. No follow-up PR is owed."
               "\n  The one exception: a [coverage] finding saying a test"
               " certifies a property it does not check is worth acting on,"
               " because a test that cannot fail emits a false safety signal."
               "\n\nPushing another fix to this PR earns another pass against"
               " the same rules, and on a PR whose diff is mostly tests that"
               " does not terminate — coverage findings are never suppressed"
               " by the re-raise rules, so there is always another one."
               "\n\nNext: post the single summary comment, then merge. The"
               " pr-review-loop skill has the summary format.")
          (str "NOT MERGEABLE means at least one [correctness/blocking]"
               " finding stands. Next: use the pr-review-loop skill — verify"
               " each blocking finding against the source before changing"
               " anything, fix the class rather than the instances, and push"
               " onto THIS PR rather than a new one.")))))

(defn- unresolved-base-message
  "Names the unresolved ref and the exact fix, so agent A does not have to
   guess why a branch with a real diff came back with nothing to say."
  [{:keys [branch pr pass base-ref]}]
  (str "pr-review-loop — " branch
       " PR #" pr ", pass " pass
       ": could not diff against base ref \"" base-ref "\" — this clone likely"
       " never fetched it, so the diff command itself failed rather than"
       " finding no changes. Fix: `git fetch origin " base-ref "`, then push"
       " again."))

(defn- failed-review-message
  "A review that produced no findings still has to wake agent A — the real
   diagnosis is sitting unread in the reviewer's stderr — but it must say
   plainly that no slot was spent, because none was."
  [{:keys [branch pr pass]} parsed res]
  (str "pr-review-loop — " branch
       " PR #" pr ", pass " pass ": review did not complete ("
       (:verdict parsed) ")"
       "\n\nreviewer process exited " (:exit res) ": " (:err res)
       (when-not (str/blank? (str (:body parsed)))
         (str "\n\n" (:body parsed)))
       "\n\nNo ledger row was written, so this attempt did not consume one of"
       " the " ledger/max-passes " review slots for PR #" pr
       ". Fix the cause and push again."))

(defn- no-checkout-message
  "A review that could not be pinned to its sha is refused, not downgraded to
   the agent's live tree. Reading that tree is the defect this checkout
   exists to remove — the agent is still editing it — so falling back would
   quietly restore it and spend a ledger slot on a review of whatever the
   files happened to say."
  [{:keys [branch pr pass sha]}]
  (str "pr-review-loop — " branch
       " PR #" pr ", pass " pass
       ": could not check out " (subs sha 0 (min 12 (count sha)))
       " into a review worktree, so no review ran. No ledger row was written."
       " Fix: `git worktree prune`, then push again."))

(defn- crash-message
  [{:keys [branch pr pass]} e]
  (str "pr-review-loop — " branch
       " PR #" pr ", pass " pass " crashed: " (ex-message e)))

(defn- release-quietly!
  "release! reaches the filesystem — flock/with-file-lock creates its guard
   file and parent — so it can throw. A throw out of a `finally` replaces the
   value the body already computed, so an unwrapped release! is a second
   route to losing a completed review, and (before `review!` had an outer
   try) to a non-2 exit. A failed release is self-healing anyway: the record
   names a pid, and acquire! treats a dead holder's lock as free."
  [git-dir pr opts]
  (try (lock/release! git-dir pr opts) (catch Exception _ nil)))

(defn- review-in!
  "The reviewer pass proper, against `review-root` — a checkout pinned to the
   reviewed sha, never the agent's live worktree."
  [{:keys [git-dir repo-root pr pass sha base-ref draft? prior-fingerprints
           pr-url] :as d}
   review-root opts]
  (let [ctx (context/build! review-root git-dir
                            {:pr pr :sha sha :base-ref base-ref} opts)]
    (if (:diff-failed? ctx)
      ;; The diff command itself failed — almost always an unresolved base
      ;; ref. A 0-byte diff here looks exactly like a real empty one, so
      ;; spawning the reviewer would have it correctly report "nothing to
      ;; review" and the loop would record a false MERGEABLE. Refuse to
      ;; review, and refuse to spend a ledger slot on a pass that reviewed
      ;; nothing — the PR would still owe a real review even after the cap.
      {:exit 2 :message (unresolved-base-message d)}
      (let [;; Minted here, used twice and never reused: the prompt asks for it
            ;; on the verdict line and the parser accepts only that spelling, so
            ;; a verdict quoted from an earlier pass — which review_core.md asks
            ;; a re-review to do while verifying closure — carries an older tag
            ;; and cannot be read as this pass's answer.
            tag    ((or (:verdict-tag-fn opts) #(subs (str (random-uuid)) 0 8)))
            text   (prompt/build {:core (core-prompt)
                                  :repo-root review-root :git-dir git-dir
                                  :ctx ctx :pr pr :pass pass :draft? draft?
                                  :pr-url pr-url :verdict-tag tag
                                  :prior-fingerprints prior-fingerprints})
            ;; A killed reviewer used to leave nothing behind to explain
            ;; itself. This file survives a SIGKILL, and the next pass
            ;; overwrites it, so it is always about the most recent attempt.
            res    (reviewer/run! text review-root
                                  (assoc opts :err-file
                                         (str git-dir "/pr-review." pr ".stderr")))
            ;; reconcile is mergeable? wired in: a count block that
            ;; contradicts the verdict line loses, here, once, so both the
            ;; headline and the ledger row carry the same reconciled verdict.
            parsed (reviewer/reconcile (reviewer/parse-output (:out res) tag))]
        (cond
          ;; A newer push superseded this trigger and killed its reviewer
          ;; mid-answer. Recording that truncated output would spend a slot
          ;; and wake agent A with findings for a SHA that is already stale.
          (lock/superseded? git-dir pr opts)
          {:exit 0 :message nil}

          ;; A crashed or unparsed reviewer produced no findings, so it does
          ;; not consume a cap slot — the same ruling the unresolved-base-ref
          ;; path already makes. Six pushes against an expired token used to
          ;; write six MALFORMED rows and, with four real passes, exhaust the
          ;; PR's budget permanently.
          (or (not (zero? (:exit res))) (= "MALFORMED" (:verdict parsed)))
          {:exit 2 :message (failed-review-message d parsed res)}

          :else
          (do (ledger/append-pass!
               git-dir
               {:pr pr :sha sha :pass pass
                :verdict (:verdict parsed)
                :blocking (get (:counts parsed) "correctness/blocking" 0)
                :followup (get (:counts parsed) "correctness/followup" 0)
                :coverage (get (:counts parsed) "coverage" 0)
                :fingerprints (:fingerprints parsed)})
              (context/prune! git-dir pr context-keep)
              (sweep-abandoned-artifacts! git-dir)
              (let [warnings (reviewer/parse-warnings parsed)
                    msg (findings-message d parsed warnings)]
                ;; The findings text lived only in the wake. The ledger keeps
                ;; verdict, counts and fingerprints -- enough to decide, not
                ;; enough to READ -- so a review whose wake was lost, or one
                ;; run by hand in a different session, could not be handed to
                ;; whoever is working the PR. Latest pass only: handing off
                ;; wants the current findings, and history is the ledger plus
                ;; the summary comment.
                (spit (findings-path git-dir pr) msg)
                ;; Posted by the TRIGGER, never by the reviewer: the review is
                ;; generated once, and `Bash(gh pr comment:*)` stays denied to it.
                ;; Last, and after both the ledger row and the findings file
                ;; are on disk, so a GitHub failure costs the convenience of
                ;; reading the review there and nothing else.
                (let [url ((or (:post-comment-fn opts) gh/post-comment!)
                           repo-root pr (findings-comment d parsed warnings)
                           opts)]
                  {:exit 2
                   :message (if url
                              (str msg "\n\nPosted to the PR: " url)
                              (str msg "\n\nPosting this review to the PR"
                                   " FAILED — it exists only in the ledger"
                                   " and at the path above."))}))))))))

(defn- run-review!
  "Pins a worktree to the reviewed sha and reviews that, removing it however
   the pass ends."
  [{:keys [git-dir pr sha] :as d} opts]
  ((or (:with-checkout-fn opts) checkout/with-checkout)
    git-dir pr sha
    (or (:checkout-parent opts)
        (str (fs/path (fs/temp-dir) "pr-review-worktrees")))
    opts
    (fn [review-root]
      (if review-root
        (review-in! d review-root opts)
        {:exit 2 :message (no-checkout-message d)}))))

(defn- review!
  "Always returns an :exit of 0 or 2. Nothing inside — acquire!, the context
   build, the reviewer, the ledger write, the release — may propagate, because
   -main has no try of its own and any other exit code is a silently lost
   pass rather than a loud failure."
  [{:keys [git-dir pr sha branch] :as d} opts]
  (try
    ;; `:branch` is not decoration. It is the only field in the lock record
    ;; that lets `abandoned-candidates` rebuild a decision from a killed
    ;; review, and omitting it made the whole retry path dead code in
    ;; production while its tests passed on a stub that supplied one.
    (if (= :duplicate (:status (lock/acquire! git-dir {:pr pr :sha sha
                                                       :branch branch} opts)))
      {:exit 0 :message nil}
      (try
        (run-review! d opts)
        ;; Same `opts` acquire! was called with, not just git-dir: release!
        ;; only deletes the record if its :pid still matches, and a test that
        ;; stubs :pid in opts to acquire! must have that same stub honoured on
        ;; release! or the two would disagree about who holds the lock.
        (finally (release-quietly! git-dir pr opts))))
    (catch Exception e
      {:exit 2 :message (crash-message d e)})))

(defn- respond
  "The exit code and wake message a decision earns.

   Split out of -main so the exit-code contract is testable without
   System/exit: an action this `case` never learned falls through to the
   default and is silently inert — which is precisely the defect class this
   namespace keeps paying for."
  [d opts]
  (case (:action d)
    :silent      {:exit 0 :message nil}
    :cap-reached {:exit 2 :message (str "pr-review-loop — " (:reason d)
                                        ". No further reviews will run"
                                        " on this PR. Decide manually.")}
    :review      (review! d opts)
    {:exit 0 :message nil}))

(defn wake-log-path
  "Where this process records that it asked for a wake.

   Nothing recorded the trigger's own exit, so a missing wake was
   indistinguishable between \"the trigger never got there\" and \"the harness
   dropped it\". Measured on PR #409: the review completed, wrote its ledger
   row and its findings file, and no session's queue ever received the
   message — while a different review's wake was delivered 57 seconds later
   into the same session, idle since seven minutes before. Both explanations
   fit those facts and they need different fixes."
  [git-dir]
  (str git-dir "/pr-review.wake.log"))

(defn- record-wake!
  "Appends one line recording what this process decided, immediately before it
   exits. Guarded: housekeeping must never be able to change the exit code.

     <epoch-ms>  <pr>  <action>  fresh|retry  <session>  exit=<n>  msg=<bytes>

   `exit=2` with a non-zero message length says the trigger asked for a wake.
   If no session then received one, the harness dropped it — which is the
   distinction PR #409 could not be diagnosed without."
  [{:keys [git-dir pr action retry?]} exit message session]
  (try
    (when git-dir
      (spit (wake-log-path git-dir)
            (format "%d\t%s\t%s\t%s\t%s\texit=%d\tmsg=%d\n"
                    (System/currentTimeMillis)
                    (or pr "-")
                    (name (or action :none))
                    (if retry? "retry" "fresh")
                    (or session "-")
                    exit
                    (count (or message "")))
            :append true))
    (catch Exception _ nil)))

(defn finish!
  "Everything -main does after the decision, except exiting.

   Split out for the same reason `respond` was: -main calls System/exit, so
   nothing inside it can be tested, and the last thing added there was silently
   removable — a negative control that deleted the wake record from -main
   failed no test at all."
  [d {:keys [exit message]} session]
  (when message
    (binding [*out* *err*] (println message) (flush)))
  (record-wake! d exit message session)
  exit)

(defn -main
  [& _]
  (let [input (try (json/parse-string (slurp *in*) true)
                   (catch Exception _ nil))
        d     (decide (or input {}) {})
        r     (respond d {})
        exit  (finish! d r (:session_id input))]
    (System/exit exit)))
