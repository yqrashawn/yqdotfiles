(ns pr-review.tokens
  "A pool of Claude credentials for the reviewer, rotated across runs.

   The reviewer used to run on ONE pinned token (`reviewer/default-token-file`).
   One token is one account's limit: when it is spent every review fails
   `MALFORMED (exited 1)` until it resets, which on an org seat's monthly
   spend limit is days. cchp already load-balances the same kind of pool for
   the sessions it spawns (`cchp.module.claude-code.token-manager`), and this
   is that idea for the reviewer.

   WHAT IS NOT SHARED WITH cchp: its rotation state is an in-process atom in a
   long-lived JVM. The reviewer is a fresh babashka process per review, so
   nothing in memory survives to the next one and the two processes cannot see
   each other's parks. The cursor and the parked set therefore live in a file
   (`state-path`), under a flock, and the clock is WALL CLOCK — cchp uses a
   monotonic reading precisely because it never leaves its process, and a
   monotonic reading means nothing to the process that reads this file next.
   The cost is the one cchp's monotonic.clj documents: an NTP step or a
   suspended host mis-times a park. A park runs to the reset its banner states
   (`reset-at-ms`) and falls back to `park-ms` when the prose names none, so a
   mis-timed park is bounded by `max-park-ms` rather than by the hour it used
   to be — and `park!` will not shorten one, so a park too long is not
   corrected by the next rejection. Issue #175.

   ATTRIBUTION. `reviewer/default-token-file`'s docstring argues for one pinned
   credential so reviewer spend stays attributable. Rotating gives that up on
   purpose: spend now lands on whichever pool account answered. The pool is the
   same one cchp draws from, so it is the same set of accounts either way."
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [pr-review.atomicfile :as atomicfile]
            [pr-review.flock :as flock]))

(def default-env-file
  "Where the pool is read from when it is not already in the environment.

   cchp's `.env.local`, because that is the file the operator edits when they
   change the pool — it holds several commented-out CLAUDE_TOKENS lines and one
   live one, and keeping a second copy anywhere else would mean a swap that
   takes effect for sessions and not for reviews. Overridable with
   PR_REVIEW_TOKENS_ENV_FILE."
  (str (fs/path (System/getProperty "user.home")
                "workspace" "home" "claude-code-http-proxy" ".env.local")))

(def ^:private park-ms
  "The FALLBACK park, for a rejection whose prose names no reset this can
   place on a clock: 1 hour.

   Not the usual case any more — `reset-at-ms` parses the reset out of the
   banner and `park!` runs the park to that instant. This is what is left when
   it cannot: the same guess cchp's token-manager makes, for a reason that
   still holds here, since `claude -p` gives prose and not the CLI's
   structured `rate_limit_event` with its `resetsAt`.

   Short on purpose: a token parked past its reset is idle capacity, and a
   token retried early costs one failed review pass, which `MALFORMED` already
   treats as free."
  (* 60 60 1000))

;;; Where the pool comes from

(defn- unquote-value
  "A shell assignment's value, as `source` would see it.

   A QUOTED value ends at its closing quote and whatever follows it —
   including a ` # comment` — is not part of the value. An UNQUOTED one ends
   at the first unescaped ` #`.

   Written as one scan over the two cases rather than as two independent
   rules, because two independent rules is what the first version was and it
   handled quotes OR a comment, never both: `CLAUDE_TOKENS=\"a,b\" # jason`
   came back as `[\"\\\"a\" \"b\\\"\"]` — a non-empty pool of mangled
   credentials, which is worse than an empty one. `pool` non-empty means
   `select!` returns something, and `(or (tokens/select!) (oauth-token))`
   short-circuits, so the pinned-file fallback never runs and every review
   authenticates with a quote-mangled token. The file this reads is documented
   as carrying comments on exactly these lines."
  [v]
  (let [v (str/trim v)
        q (first v)]
    (if (and (seq v) (or (= \' q) (= \" q)))
      (let [rest-of (subs v 1)
            close (str/index-of rest-of (str q))]
        (if close
          (subs rest-of 0 close)
          ;; An unterminated quote is not a value `source` would accept
          ;; either. Return what is there minus the opener rather than guess.
          rest-of))
      (str/trim (str/replace v #"\s+#.*$" "")))))

(defn env-file-var
  "The value of `var-name` in a dotenv-style `path`, or nil.

   The LAST assignment wins, because `source` runs the file top to bottom and
   that is which line cchp ends up with. Comment lines are skipped, which is
   what makes the several disabled CLAUDE_TOKENS pools in that file stay
   disabled here too.

   Never throws: an unreadable or absent file means nil, and the caller falls
   back to the single pinned token. Losing rotation is survivable; a review
   that cannot start is not."
  [path var-name]
  (try
    (when (and path (fs/regular-file? path))
      (->> (str/split-lines (slurp (str path)))
           (keep (fn [line]
                   (let [l (str/triml line)]
                     (when-not (str/starts-with? l "#")
                       (let [l (str/replace-first l #"^export\s+" "")
                             [k v] (str/split l #"=" 2)]
                         (when (and v (= var-name (str/trim (str k))))
                           (unquote-value v)))))))
           last))
    (catch Exception _ nil)))

(defn pool
  "The ordered token pool, or an empty vector.

   Precedence, and why:

   1. CLAUDE_TOKENS in this process's environment. cchp passes it through to
      everything it spawns on purpose (`cchp.child-env`), so a reviewer
      triggered from a mydeck Run already has the live pool — no file, no path
      to keep in step.
   2. CLAUDE_TOKENS in `default-env-file`, or in PR_REVIEW_TOKENS_ENV_FILE.
      The reviewer also runs from the user's own terminal, where nothing has
      sourced that file.

   An EXPLICIT `:env-file` skips the environment and reads that file. Naming a
   file is the caller saying which pool it means, and the environment silently
   winning over it makes the argument a no-op wherever the variable happens to
   be set — which is every reviewer cchp spawns, and was every run of this
   namespace's own tests until they said so.

   An empty result is the signal to fall back to the single pinned token, so
   it must be empty rather than nil-vs-empty ambiguous."
  ([] (pool {}))
  ([{:keys [env-file]}]
   (let [raw (if env-file
               (env-file-var env-file "CLAUDE_TOKENS")
               (or (some-> (System/getenv "CLAUDE_TOKENS") not-empty)
                   (env-file-var (or (System/getenv "PR_REVIEW_TOKENS_ENV_FILE")
                                     default-env-file)
                                 "CLAUDE_TOKENS")))]
     (->> (str/split (str raw) #",")
          (map str/trim)
          (remove str/blank?)
          vec))))

;;; Cross-process rotation state

(defn known-tokens
  "EVERY token in the env file, including the ones on commented-out lines,
   plus whatever the environment holds. Not a pool — a marker set.

   `pool` answers \"which token do I authenticate with\", and for that,
   skipping comments and preferring the environment are both right. This
   answers a different question: which strings, if the reviewer printed them,
   would be a leaked credential. The disabled CLAUDE_TOKENS lines this file is
   documented as carrying are prior pools — live tokens for real accounts —
   and the live file line is still a credential on a run where the inherited
   environment won instead. Neither reaches `redact` through `pool`, and one
   `cat` of a path this source names prints all of them.

   So: every assignment in the file, commented or not, and — on the no-argument
   arity only — the environment's too. Order is meaningless here and nothing
   authenticates with the result.

   An EXPLICIT `path` is the file alone, for the same reason `pool`'s explicit
   `:env-file` is: naming a file is the caller saying which one it means, and
   an ambient variable winning over it makes the argument a no-op wherever it
   happens to be set — which is every reviewer cchp spawns, and every run of
   this namespace's own tests.

   Never throws — an unreadable file means whatever the environment gave."
  ([] (vec (distinct (concat (known-tokens (or (System/getenv "PR_REVIEW_TOKENS_ENV_FILE")
                                               default-env-file))
                             (->> (str/split (str (System/getenv "CLAUDE_TOKENS")) #",")
                                  (map str/trim)
                                  (remove str/blank?))))))
  ([path]
   (let [lines (try
                 (when (and path (fs/regular-file? path))
                   (str/split-lines (slurp (str path))))
                 (catch Exception _ nil))
         assignments (->> lines
                          (keep (fn [line]
                                  ;; the comment marker is STRIPPED rather than
                                  ;; used to skip the line: a disabled pool is
                                  ;; exactly what this is here for
                                  (let [l (-> (str/triml line)
                                              (str/replace #"^#+\s*" "")
                                              (str/replace-first #"^export\s+" ""))
                                        [k v] (str/split l #"=" 2)]
                                    (when (and v (= "CLAUDE_TOKENS" (str/trim (str k))))
                                      (unquote-value v))))))]
     (->> assignments
          (mapcat #(str/split (str %) #","))
          (map str/trim)
          (remove str/blank?)
          distinct
          vec))))

(defn state-path
  "Where the cursor and the parked set live.

   Under XDG_CACHE_HOME beside `attempts/default-log`, for the reason stated
   there: TMPDIR measured at three different values on this machine, so a temp
   path is a place two reviewer processes would never meet."
  []
  (str (fs/path (or (System/getenv "XDG_CACHE_HOME")
                    (str (fs/path (System/getProperty "user.home") ".cache")))
                "pr-review-loop" "tokens.edn")))

(defn token-key
  "A token's identity in the state file: a SHA-256 prefix, never the token.

   The file is a plain-text cache under the user's home and the token already
   has exactly one home (`.env.local`, or the environment). A second copy is a
   second place to leak it from, and identity is all this file needs."
  [token]
  (let [md (java.security.MessageDigest/getInstance "SHA-256")
        bs (.digest md (.getBytes (str token) "UTF-8"))]
    (->> (take 12 bs)
         (map #(format "%02x" %))
         (apply str))))

(defn- read-state [path]
  (try
    (if (fs/exists? path)
      (let [s (edn/read-string (slurp path))]
        (if (map? s) s {}))
      {})
    (catch Exception _ {})))

(defn- write-state! [path state]
  (fs/create-dirs (fs/parent path))
  (atomicfile/spit! path (pr-str state)))

(defn- prune
  "Drop parks that have expired. Keeps the file from growing a row per token
   the pool has ever held, and makes `available?` a plain lookup."
  [parked now-ms]
  (into {} (remove (fn [[_ until]] (<= (long until) now-ms)) parked)))

(defn- available? [parked now-ms token]
  (let [until (get parked (token-key token))]
    (or (nil? until) (<= (long until) now-ms))))

(defn choose
  "Pure core of `select!`: the token to use, and the state to store.

   Round-robin from `:cursor`, skipping parked tokens. Returns
   `{:token t :state s}`, or nil when `tokens` is empty.

   WHEN EVERY TOKEN IS PARKED it still returns one — the one whose park ends
   soonest. A park is a stated reset at best and `park-ms` at worst, and
   either can be wrong; refusing to run would turn that into a review that
   never happens, and the caller has no other
   credential to offer. Getting it wrong costs one `MALFORMED` attempt, which
   the ledger does not charge a pass for."
  [tokens state now-ms]
  (when (seq tokens)
    (let [parked (prune (:parked state {}) now-ms)
          n (count tokens)
          start (mod (long (:cursor state 0)) n)
          order (map #(nth tokens (mod (+ start %) n)) (range n))
          token (or (first (filter #(available? parked now-ms %) order))
                     ;; every one parked: the earliest to come back
                    (first (sort-by #(get parked (token-key %) 0) tokens)))
          idx (.indexOf ^java.util.List (vec tokens) token)]
      {:token token
       :state (assoc state :parked parked :cursor (inc idx))})))

(defn select!
  "Pick this run's token and advance the cursor, or nil when the pool is empty.

   Read and write happen under ONE flock: two reviews can start within the same
   second (a push to two branches), and a read-then-write without the lock
   hands both the same token and loses one increment — which is the whole
   mechanism.

   The lock is taken on a guard file, never on the state file itself:
   `write-state!` publishes by atomic rename, and flocking a path that is about
   to be renamed locks an orphaned inode (see `pr-review.flock`)."
  ([] (select! (pool) (System/currentTimeMillis)))
  ([tokens now-ms]
   (when (seq tokens)
     (let [path (state-path)]
       (try
         (fs/create-dirs (fs/parent path))
         (flock/with-file-lock
           (flock/guard-path path)
           (fn []
             (let [{:keys [token state]} (choose tokens (read-state path) now-ms)]
               (write-state! path state)
               token)))
         ;; A cache directory that cannot be written must not cost a review:
         ;; fall back to the first token, which is the pinned-token behaviour
         ;; the reviewer had before rotation existed.
         (catch Exception _ (first tokens)))))))

;;; Parking a spent token

(def limit-patterns
  "CLI limit banners that mean \"this account is spent\".

   Copied from cchp's `claude-code.query/rate-limit-patterns`, which was
   derived from banners in llm_request_logs and rewritten once because the one
   pattern it started with silently stopped matching. Same two shapes:
   personal/subscription, and the org seat's monthly spend limit.

   ONE DELIBERATE DIVERGENCE: the qualifier is `[\\w-]+` where cchp's is
   `\\w+`, so a hyphenated window — a `5-hour` limit — matches. That wording
   is not in the sample cchp's set was derived from, and the asymmetry decides
   it: an unmatched banner hands a spent token straight back out on the next
   rotation and every review keeps failing until the account resets, where a
   pattern one word too wide costs one token parked until whatever reset the
   text names — `park-ms` if it names none, and at most `max-park-ms`.

   The org pattern is assembled from two halves at load time, and the banner
   is written out contiguously nowhere in this file — for cchp's reason, which
   applies here too: the reviewer reads this repository, and a file containing
   a banner verbatim matches its own pattern."
  [#"You've hit your (?:[\w-]+ )?limit · resets \d{1,2}(?::\d{2})?(?:a|p)m \("
   (re-pattern (str "You've hit your org" "'s monthly spend limit"))])

(defn limited?
  "True when `text` carries a limit banner."
  [text]
  (boolean (and (string? text) (some #(re-find % text) limit-patterns))))

(def ^:private reset-re
  "The `resets …` tail of a limit banner, with the parts needed to place it on
   a clock: an optional month and day, a 12-hour time with optional minutes,
   and the zone the CLI printed it in.

   Only the TAIL. The banner's opening clause is `limit-patterns`' job, and a
   file that contains a whole banner matches its own pattern — this fragment
   does not, which is why the two are separate and why `limited?` still
   decides whether there is a limit at all."
  #"resets (?:(\p{Alpha}{3}) (\d{1,2}) at )?(\d{1,2})(?::(\d{2}))?(a|p)m \(([^)]{1,60})\)")

(def ^:private max-park-ms
  "How far ahead a parsed reset may point and still be believed: 8 days, one
   day past the longest window the CLI reports.

   cchp's `max-rate-limit-duration-ms` with cchp's reasoning, which applies to
   a parsed instant as much as to a reported one: the number comes from
   another machine's clock, and a skewed or misread value would otherwise park
   an account for years — the failure where reviews stop and nothing says why.
   A value beyond this is not clamped but DISBELIEVED: clamping would still
   park for 8 days on the strength of a number already known to be wrong,
   where falling back to the hour costs one retry to find out."
  (* 8 24 60 60 1000))

(defn- reset-candidate
  "One `resets …` match placed on a clock, in epoch ms, or nil.

   nil for anything not clearly in the future and within `max-park-ms`: a
   reset already past says nothing about when the next one is.

   Undated means the next occurrence of that time, which for a weekly window
   is a LOWER bound — and a lower bound is the right error here, for the
   reason `reset-at-ms` gives.

   A dated reset carries no year, so the years either side of now are tried
   too: a December banner naming January is next year, and `max-park-ms`
   rejects whatever is absurd. `Month/valueOf` wants the full name, and
   `java.time` has no three-letter parse that does not also drag in a
   locale."
  [mon day hh mm ap tz now-ms]
  (try
    (let [zone (java.time.ZoneId/of tz)
          h12 (parse-long hh)
          hour (cond (and (= "a" ap) (= 12 h12)) 0
                     (= "a" ap) h12
                     (= 12 h12) 12
                     :else (+ 12 h12))
          time (java.time.LocalTime/of hour (if mm (parse-long mm) 0))
          now (java.time.Instant/ofEpochMilli now-ms)
          candidates
          (if mon
            (let [month (.getValue (java.time.Month/valueOf
                                    (str/upper-case
                                     (case (str/lower-case mon)
                                       "jan" "january" "feb" "february"
                                       "mar" "march" "apr" "april"
                                       "may" "may" "jun" "june"
                                       "jul" "july" "aug" "august"
                                       "sep" "september" "oct" "october"
                                       "nov" "november" "dec" "december"
                                       mon))))
                  y (.getYear (java.time.ZonedDateTime/ofInstant now zone))]
              (keep (fn [yy]
                      (try
                        (-> (java.time.LocalDate/of yy month (parse-long day))
                            (.atTime time)
                            (.atZone zone)
                            .toInstant)
                        (catch Exception _ nil)))
                    [(dec y) y (inc y)]))
            (let [today (-> (java.time.ZonedDateTime/ofInstant now zone)
                            (.with time))]
              [(.toInstant today) (.toInstant (.plusDays today 1))]))]
      (when-let [i (->> candidates
                        (filter #(.isAfter ^java.time.Instant % now))
                        sort
                        first)]
        (let [ms (.toEpochMilli ^java.time.Instant i)]
          (when (<= (- ms now-ms) max-park-ms) ms))))
    ;; A malformed zone, an impossible date, a number that is not one: the
    ;; caller has an hour to fall back on, and losing a park is survivable
    ;; where losing the review that is already paid for is not.
    (catch Exception _ nil)))

(defn reset-at-ms
  "The instant `banner` says the limit resets, in epoch ms, or nil.

   This is the difference between a park that is right and a park that is a
   guess. `park-ms` is one hour because the structured `rate_limit_event` cchp
   reads is not available to a `claude -p`, only prose — but the prose STATES
   the reset, and one hour is measurably the wrong number for the limit that
   actually bit: an org seat parked at 22:24 whose banner said `resets 3am`
   came back into rotation at 23:24, 3h36m before the account was live again.
   Under-parking is self-correcting only in the sense that the next rejection
   re-parks it; that rejection is a review that did not happen, because a
   MALFORMED attempt is not retried automatically.

   THE EARLIEST of every `resets …` in `banner`, not the first. What arrives
   here is the reviewer's whole output, and the reviewer reads repositories
   and quotes what it finds — on this repository, including the fixtures in
   this namespace's own tests. Taking the first match let quoted prose above a
   real banner decide the park: measured, a quoted `Sep 28 at 3am` above a
   genuine `resets 3am` gave a 138.7-hour park where the banner said 18.7.

   Earliest, rather than trying to identify which match is the banner: there
   is no reliable way to tell a quotation from the real thing in a text the
   reviewer composed, and the two errors are not symmetric. Parking too
   briefly costs one retry, which re-parks. Parking too long cannot be
   corrected at all — there is no unpark, and `park!` refuses to shorten. So
   under this rule a quoted instant can only ever make the park SHORTER than
   the banner asked, never longer, and the worst a forged one can do is what
   no banner at all already does.

   nil when nothing parses, and the caller then falls back to the hour."
  [banner now-ms]
  (try
    (->> (when (string? banner) (re-seq reset-re banner))
         (keep (fn [[_ mon day hh mm ap tz]]
                 (reset-candidate mon day hh mm ap tz now-ms)))
         sort
         first)
    (catch Exception _ nil)))

(defn park!
  "Mark `token` unusable until the limit lifts. No-op without a token.

   `banner` is the reviewer's output, and when it states a reset (`reset-at-ms`)
   the park runs to THAT instant. Without one — no banner passed, or prose
   this cannot place on a clock — it is `park-ms`, one hour, which is a guess
   and was measurably too short for the limit that actually bit.

   A park never SHORTENS an existing one: two parks on one token mean two
   rejections, and the later instant is the one both agree on. Without that,
   a banner-less second rejection would replace a parsed multi-day park with
   an hour and hand the account straight back out. (cchp's `park-until` makes
   the same choice for the same reason.)

   Best-effort by design: a failure to record a park costs the next review one
   wasted attempt against a spent token, where throwing here would cost the
   CURRENT review its result, which has already been paid for."
  ([token] (park! token (System/currentTimeMillis) nil))
  ([token now-ms] (park! token now-ms nil))
  ([token now-ms banner]
   (when (seq (str token))
     (let [path (state-path)]
       (try
         (fs/create-dirs (fs/parent path))
         (flock/with-file-lock
           (flock/guard-path path)
           (fn []
             (let [state (read-state path)
                   k (token-key token)
                   until (or (reset-at-ms banner now-ms) (+ now-ms park-ms))
                   pruned (prune (:parked state {}) now-ms)
                   parked (assoc pruned k (max (long until)
                                               (long (get pruned k 0))))]
               (write-state! path (assoc state :parked parked)))))
         (catch Exception _ nil))))
   nil))
