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

(defn pool-file
  "Which file the POOL is read from: the override, else `default-env-file`.

   Precedence, not union — that is the difference from `secret-files`, which
   marks BOTH because a file's contents do not stop being credentials just
   because the pool comes from elsewhere.

   `not-empty`, because an empty PR_REVIEW_TOKENS_ENV_FILE is not a path: a
   bare `or` read it as one, found no pool in it and fell back to the pinned
   token without saying so. The override arrives as an ARGUMENT on the
   1-arity for the reason `secret-files` gives — a process cannot set a
   variable in its own environment for its own `System/getenv`, so the rule
   is otherwise asserted and never exercised."
  ([] (pool-file (System/getenv "PR_REVIEW_TOKENS_ENV_FILE")))
  ([override] (or (not-empty (str override)) default-env-file)))

(defn pool
  "The ordered token pool, or an empty vector.

   Precedence, and why:

   1. CLAUDE_TOKENS in this process's environment. cchp passes it through to
      everything it spawns on purpose (`cchp.child-env`), so a reviewer
      triggered from a mydeck Run already has the live pool — no file, no path
      to keep in step.
   2. CLAUDE_TOKENS in `default-env-file`, or in PR_REVIEW_TOKENS_ENV_FILE.
      The reviewer also runs from the user's own terminal, where nothing has
      sourced that file. `pool-file` decides which, and says why an empty
      override is not a path.

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
                   (env-file-var (pool-file) "CLAUDE_TOKENS")))]
     (->> (str/split (str raw) #",")
          (map str/trim)
          (remove str/blank?)
          vec))))

;;; Which strings are credentials

(def credential-shape-re
  "What a credential LOOKS like, independent of where it came from.

   The FLOOR under the marker set, and a deliberate reversal of what `redact`
   used to say (\"guessing at shapes would give false confidence\"). Three
   review passes each found one more source outside the enumerated marker set
   — the rest of the pool, then the pinned file, then the disabled pools — and
   enumerating sources cannot converge, because each pass names the next
   spelling. A shape test does not care where a value came from: a credential
   in the output is redacted whether this process can name it or not.

   It misses whatever does not match, which is why it is a floor and not the
   whole mechanism. `known-secrets` still names values, and that layer is what
   covers a credential shaped like nothing in particular.

   WRITTEN NOT TO MATCH ITSELF. Every prefix here is followed by a character
   class, and `[` is in none of those classes, so this source's own text is
   not a match — the mistake `limit-patterns` made once (#176). The reviewer
   reads this repository.

   Anthropic (`sk-ant-…`, the reviewer's own credential) plus the generic
   `sk-` key, Slack and GitHub, because the file this reads is cchp's
   `.env.local` and that file holds far more than Claude tokens."
  #"sk-ant-[A-Za-z0-9_-]{16,}|sk-[A-Za-z0-9]{20,}|xox[abeoprs]-[A-Za-z0-9-]{10,}|gh[pousr]_[A-Za-z0-9]{20,}")

(defn marker-worthy?
  "Whether `v`, read out of a credential file, may become a redaction marker.

   `redact` substitutes a marker EVERYWHERE it appears, so a marker that is
   also ordinary text garbles the review. Credential files are exactly where
   placeholders live — `# CLAUDE_TOKENS=REPLACE_ME_WITH_TOKEN` is a comment in
   the kind of file this reads — and marking every assignment without a filter
   turns each placeholder into a substitution over unrelated prose.

   Three rules, none of them an enumeration of placeholders:

   - 16 characters and no whitespace. Shorter matches prose everywhere.
     `reviewer/credentials` keeps its own 8-character floor for the values
     this process KNOWS are credentials; this is the higher bar a value has to
     clear to be GUESSED into the set.
   - SHOUTING SEGMENTS are a placeholder: `REPLACE_ME_WITH_TOKEN`,
     `YOUR_TOKEN_HERE`, `REPLACE-ME-WITH-YOUR-TOKEN`. Upper-case WORDS joined
     by `_` or `-` is the convention for \"fill this in\". A single upper-case
     run is NOT covered, on purpose: `AKIAIOSFODNN7EXAMPLE` (an AWS key id)
     and a base32 TOTP secret are both spelled that way, and an earlier
     version of this rule rejected every `[A-Z0-9_]+` value — which took both
     of those out of the marker set while `credential-shape-re` does not
     cover them either, so neither layer held them. `<…>` is the other
     placeholder convention and goes with this rule.
   - A `/` means a URL or a path, not a credential.

   WHAT IT STILL ADMITS, stated rather than implied: any other ≥16-character
   configuration value in the file — a model id, a client id, a hostname —
   becomes a marker and is substituted wherever it appears in the review. That
   is the deliberate direction of error. A word redacted out of a review costs
   a reread; a credential left unmarked costs the credential, and the three
   passes this replaces were all the second kind.

   A value matching `credential-shape-re` passes regardless — a known
   credential shape outranks every rule here, and that is what keeps the shape
   floor from being weakened by this gate."
  [v]
  (boolean
   (and (string? v)
        (let [v (str/trim v)]
          (and (>= (count v) 16)
               (not (re-find #"\s" v))
               (or (re-find credential-shape-re v)
                   (and (not (re-matches #"[A-Z0-9]+(?:[_-][A-Z0-9]+)+" v))
                        (not (re-find #"[<>/]" v)))))))))

(defn env-secrets
  "The pool in THIS process's environment, split.

   A function rather than an inline `System/getenv` so a test can say what the
   environment holds. The property the explicit arity of `known-secrets`
   carries — an ambient variable must not win over a named file — is otherwise
   only falsifiable where CLAUDE_TOKENS happens to be set, and `bb test` runs
   under `env -u CLAUDE_TOKENS`, so it was asserted and never exercised."
  []
  (->> (str/split (str (System/getenv "CLAUDE_TOKENS")) #",")
       (map str/trim)
       (remove str/blank?)
       vec))

(defn secret-files
  "Every credential file this source names a path to.

   BOTH, not either. The marker set used to scan PR_REVIEW_TOKENS_ENV_FILE
   `or` `default-env-file`, so an override pointed anywhere else left the
   default file's pools unmarked while its path stayed a literal in this
   source — one `cat` away from the PR comment. The override says which file
   the POOL comes from; it does not make the other file's contents stop being
   credentials.

   `not-empty`, because an empty PR_REVIEW_TOKENS_ENV_FILE is not a path.

   The override arrives as an ARGUMENT on the 1-arity so the rule above is
   testable: a process cannot set a variable in its own environment for its
   own `System/getenv` to read, and asserting it against whatever the tester
   happens to export is how the last version of this property came to be
   asserted and never exercised."
  ([] (secret-files (System/getenv "PR_REVIEW_TOKENS_ENV_FILE")))
  ([override]
   (->> [(not-empty (str override)) default-env-file]
        (remove nil?)
        distinct
        vec)))

(defn known-secrets
  "Every string this process can NAME that would be a leaked credential if the
   reviewer printed it. Not a pool — a marker set, and nothing authenticates
   with the result.

   `pool` answers \"which token do I log in with\", and for that, skipping
   comments and preferring the environment are both right. This answers the
   other question, and it is keyed on the FILE, not on a variable name: every
   value assigned in every file `secret-files` names, commented or not,
   whatever the key. Keying on `CLAUDE_TOKENS` cost three review passes, one
   spelling at a time — `# # CLAUDE_TOKENS=`, `# set CLAUDE_TOKENS=`,
   `# OLD_CLAUDE_TOKENS=` — while every non-Claude credential in the same file
   stayed unmarked the whole time. What the reviewer can `cat` is the file;
   the key inside it is not a boundary.

   `marker-worthy?` decides what may become a marker, because marking every
   assignment also marks every placeholder.

   Values are kept whole AND split on `,`: a pool is a comma list, and a leak
   can print one member or the whole line.

   An EXPLICIT `path` is that file alone — no environment, no second file —
   for the same reason `pool`'s explicit `:env-file` is: naming a file is the
   caller saying which one it means, and an ambient variable winning over it
   makes the argument a no-op wherever it is set.

   Never throws: an unreadable file means whatever the rest gave."
  ([] (vec (distinct (concat (mapcat known-secrets (secret-files))
                             (env-secrets)))))
  ([path]
   (let [lines (try
                 (when (and path (fs/regular-file? path))
                   (str/split-lines (slurp (str path))))
                 (catch Exception _ nil))]
     (->> lines
          (keep (fn [line]
                  ;; The comment marker is STRIPPED rather than used to skip
                  ;; the line: a disabled pool is a prior pool, which is a live
                  ;; credential for a real account. Repeated `#`s and a
                  ;; `set`/`export` verb strip too — each of those spellings
                  ;; was measured dropping a credential out of the set.
                  (let [l (-> (str/triml line)
                              (str/replace #"^(?:#+\s*)+" "")
                              (str/replace-first #"^(?:export|set)\s+" ""))
                        [k v] (str/split l #"=" 2)]
                    ;; A shell identifier on the left, so a prose line that
                    ;; happens to carry `=` is not read as an assignment.
                    (when (and v (re-matches #"[A-Za-z_][A-Za-z0-9_]*"
                                             (str/trim (str k))))
                      (unquote-value v)))))
          (mapcat (fn [v] (cons v (str/split (str v) #","))))
          (map str/trim)
          (filter marker-worthy?)
          distinct
          vec))))

;;; Cross-process rotation state

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
          ;; 1-12 or nothing. The CLI prints a valid 12-hour clock, so a
          ;; bogus hour is only reachable from text the reviewer quoted — but
          ;; there it WAS reachable, and `LocalTime/of` caught far less of it
          ;; than it looks: measured by running the old mapping, `am` passed
          ;; everything up to 23 (`14am` gave 14:00) and was first rejected at
          ;; 24, while `pm` was first rejected at 13. `0am` gave midnight.
          ;; A bogus hour shortens a park below what the banner asked, which
          ;; is the direction that costs a review.
          hour (cond (not (<= 1 h12 12)) (throw (ex-info "not a 12-hour hour" {}))
                     (and (= "a" ap) (= 12 h12)) 0
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

(defn- banner-starts
  "Where each limit banner begins in `text`.

   The anchor the parse needs. `limited?` answers whether there is a banner
   and throws away WHERE, which is the whole difficulty: what reaches
   `reset-at-ms` is the reviewer's entire output, and a `resets …` in it may
   belong to a banner or to prose the reviewer quoted."
  [text]
  (sort
   (mapcat (fn [p]
             (let [m (re-matcher p text)]
               (loop [acc []]
                 (if (.find m) (recur (conj acc (.start m))) acc))))
           limit-patterns)))

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

   TWO RULES, and both are load-bearing, because what arrives here is the
   reviewer's whole output and the reviewer reads repositories and quotes what
   it finds — on this repository, including the fixtures in this namespace's
   own tests:

   1. ANCHORED TO ONE LINE. A candidate is only read from the LINE a limit
      banner starts on (`banner-starts` plus the next newline). A `resets …`
      on any other line is prose, and prose does not park anything. The line,
      not the rest of the text: anchoring at the banner's start alone still
      let a quotation three lines below it win, because the org banner's
      pattern carries no reset tail and the search ran on to the first one it
      could find — measured at 138.6 h. A banner is one line.
   2. EARLIEST. Of the candidates that survive, the soonest wins.

   Rule 1 is what defeats quoted prose, and it took two tries to get there.
   Reading the FIRST match in the whole text let a quotation above the banner
   win — measured, 138.7 h against a banner saying 18.7 h. Reading from the
   banner's start but on to the end of the text let a quotation three lines
   BELOW win whenever the banner's own pattern carries no reset tail, which
   the org one does not — measured, 138.6 h where the fallback is 1 h. Neither
   was correctable afterwards: there is no unpark and `park!` refuses to
   shorten (issue #175).

   Rule 2 is for a different case and is NOT what closed those: output
   carrying more than one banner line, which a retry or the concatenation of
   stdout and stderr can produce. The soonest is then the one to believe,
   because a stale earlier banner must not extend a park. Structural rather
   than measured — no such output has been seen.

   What remains is a forged WHOLE banner, opening clause and reset on one
   line, which parks its own instant. That is also what `limited?` needs to
   be fooled, and `limited?` is the gate: `run!` parks only on a NON-ZERO
   exit, so the forgery has to arrive from a `claude -p` that also failed.

   nil when nothing parses, and the caller then falls back to the hour."
  [banner now-ms]
  (try
    (when (string? banner)
      (->> (banner-starts banner)
           (keep (fn [start]
                   (let [nl (str/index-of banner "\n" start)
                         line (subs banner start (or nl (count banner)))]
                     (when-let [[_ mon day hh mm ap tz] (re-find reset-re line)]
                       (reset-candidate mon day hh mm ap tz now-ms)))))
           sort
           first))
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
