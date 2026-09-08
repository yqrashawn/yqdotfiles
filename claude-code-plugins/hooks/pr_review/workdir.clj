(ns pr-review.workdir
  "Which directory did a tool call actually run in?

   The `PostToolUse` payload's `cwd` is the *session's* directory, and agents
   routinely `cd` into a worktree and push from there. Measured on the first
   live push: session cwd on branch `docs/mydeck-design` with no open PR, the
   worktree the push ran in on a branch with open PR #391. The trigger
   resolved the repository from `cwd`, correctly-by-its-own-logic found
   nothing, and exited 0 — a real PR went unreviewed and nothing said so.

   No payload field carries the call's real directory. Probed keys: cwd,
   duration_ms, hook_event_name, permission_mode, prompt_id, scratchpad_dir,
   session_id, tool_input{command,description}, tool_name, tool_response,
   tool_use_id, transcript_path — and the hook process's own PWD is the
   session cwd too. Parsing the command string is the only route left.

   So: pure string analysis, with no IO beyond the existence and repository
   checks that reject a directory nothing could have pushed from. Anything
   this cannot resolve *literally* — a command substitution, a variable it
   did not see assigned, a glob, a leading `~` — yields :ambiguous and the
   fallback rather than a guess. Reviewing the wrong repository is worse than
   staying silent."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]))

;; ------------------------------------------------------------- tokenizing

(defn- end-token
  [{:keys [tok toks] :as st}]
  (assoc st :tok nil :toks (if tok (conj toks tok) toks)))

(defn- end-segment
  [st]
  (let [{:keys [toks segs]} (end-token st)]
    (assoc st :tok nil :toks [] :segs (if (seq toks) (conj segs toks) segs))))

(defn- segments
  "The command split on unquoted `;`, `&&`, `||` and newlines, each segment a
   vector of quote-stripped tokens.

   Quote-aware on purpose: the real command that exposed the defect carries a
   `;` inside a `grep -aE` pattern, and splitting through a quote would make
   the tokens downstream lie. A single `|` is deliberately *not* a separator —
   a pipeline stays one segment and is read by its leading command, exactly
   like any other."
  [command]
  (let [s (str command)
        n (count s)]
    (loop [i 0 st {:q nil :tok nil :toks [] :segs []}]
      (if (>= i n)
        (:segs (end-segment st))
        (let [c   (nth s i)
              nxt (nth s (inc i) nil)]
          (cond
            ;; single quotes: everything is literal until the closer
            (= \' (:q st))
            (if (= c \')
              (recur (inc i) (assoc st :q nil))
              (recur (inc i) (update st :tok str c)))

            (= \" (:q st))
            (cond
              (= c \")                (recur (inc i) (assoc st :q nil))
              (and (= c \\) nxt)      (recur (+ i 2) (update st :tok str nxt))
              :else                   (recur (inc i) (update st :tok str c)))

            ;; an opening quote starts a token even when it encloses nothing,
            ;; so `cd ""` reports a target this namespace cannot resolve
            ;; instead of no target at all
            (= c \')  (recur (inc i) (-> st (assoc :q \') (update :tok #(or % ""))))
            (= c \")  (recur (inc i) (-> st (assoc :q \") (update :tok #(or % ""))))

            (and (= c \\) nxt)        (recur (+ i 2) (update st :tok str nxt))
            (or (= c \;) (= c \newline)) (recur (inc i) (end-segment st))
            (and (#{\& \|} c) (= c nxt))  (recur (+ i 2) (end-segment st))
            (Character/isWhitespace c)    (recur (inc i) (end-token st))
            :else                         (recur (inc i) (update st :tok str c))))))))

;; ------------------------------------------------------------- resolvability

(def ^:private assignment-re #"^([A-Za-z_][A-Za-z0-9_]*)=(.*)$")
(def ^:private var-re #"\$\{([A-Za-z_][A-Za-z0-9_]*)\}|\$([A-Za-z_][A-Za-z0-9_]*)")

(defn- literal?
  "True when `s` is a path and nothing else: no variable, no command
   substitution, no backtick, no glob, no leading `~`. Everything this
   predicate rejects is a shape whose real value lives in a shell this
   process cannot see."
  [s]
  (and (not (str/blank? s))
       (nil? (re-find #"[$`*?]" s))
       (not (str/starts-with? s "~"))))

(defn- assignment
  [tok]
  (when-let [[_ k v] (re-matches assignment-re tok)] [k v]))

(defn- assignments
  "Simple leading `VAR=value` assignments, literal values only.

   A non-literal value maps its name to ::unresolvable rather than being
   dropped: a shell expands an unset variable to the empty string, and
   silently turning `cd \"$SP/wt\"` into `cd /wt` is exactly the guess this
   namespace exists to refuse."
  [segs]
  (reduce (fn [m seg]
            (reduce (fn [m tok]
                      (if-let [[k v] (assignment tok)]
                        (assoc m k (if (literal? v) v ::unresolvable))
                        ;; leading only — the first real word ends the run
                        (reduced m)))
                    m seg))
          {} segs))

(defn- expand
  "`$VAR` / `${VAR}` substituted from `vars` **only** — never from the real
   process environment, which belongs to this hook process and not to the
   shell that ran the command. nil when any reference is not resolvable."
  [s vars]
  (let [refs (map (fn [[_ braced bare]] (or braced bare)) (re-seq var-re s))]
    (when (every? #(string? (get vars %)) refs)
      (str/replace s var-re (fn [[_ braced bare]] (get vars (or braced bare)))))))

;; ------------------------------------------------------------- dir signals

(defn- command-tokens
  "A segment's tokens with its leading assignments and a leading `rtk`
   dropped. rtk's PreToolUse rewriter turns `git push` into `rtk git push`
   before PostToolUse ever sees it, so both shapes arrive here."
  [tokens]
  (let [toks (drop-while assignment tokens)]
    (cond-> toks (= "rtk" (first toks)) next)))

(defn- cd-target
  "The literal directory argument of a `cd`, or nil when there is none to
   resolve: a bare `cd` (the home directory) and `cd -` (the previous one)
   both name state this process cannot see."
  [args]
  (first (remove #(str/starts-with? % "-") args)))

(defn- dash-c-target
  "The path of a `git -C <path>` / `git -C<path>`, or nil."
  [args]
  (some (fn [[a b]]
          (cond (= "-C" a)                (when b b)
                (str/starts-with? a "-C") (subs a 2)))
        (partition-all 2 1 args)))

(defn- dir-signal
  "Where this segment says the work happens: a raw (unexpanded) directory
   string, ::unresolvable when it moves somewhere this process cannot name,
   or nil when the segment says nothing about the directory.

   `git -C` is treated as a move exactly like `cd`, and the last signal in
   the command wins either way — which is what the shapes in the wild look
   like (`cd \"$SP/wt\" && git push`, `git -C /path push`)."
  [tokens]
  (let [toks (command-tokens tokens)]
    (case (first toks)
      "cd"  (or (cd-target (next toks)) ::unresolvable)
      "git" (dash-c-target (next toks))
      nil)))

;; ------------------------------------------------------------- resolution

(defn- resolve-against
  [current target]
  (str (fs/normalize (if (str/starts-with? target "/")
                       target
                       (str current "/" target)))))

(defn- in-git-repo?
  "Walks up for a `.git` entry. Deliberately not `git rev-parse`: this is one
   stat per parent, and a linked worktree's `.git` is a regular file, which
   `fs/exists?` sees and a `fs/directory?` check would not."
  [dir]
  (loop [p (fs/absolutize dir)]
    (cond
      (nil? p)                       false
      (fs/exists? (fs/path p ".git")) true
      :else                          (recur (fs/parent p)))))

(defn- usable-dir?
  [dir]
  (and (fs/directory? dir) (in-git-repo? dir)))

(defn resolve-dir
  "Which directory `command` ran in, given the payload's `fallback`.

   Returns `{:dir String :basis :explicit|:fallback|:ambiguous}`:

     :explicit  — `dir` is what the command says, or the command says nothing
                  about the directory and the payload cwd is genuinely right
     :ambiguous — the command moves somewhere this namespace cannot name;
                  `dir` is `fallback`, unguessed
     :fallback  — the command names a directory that does not exist or is not
                  inside a git repository; `dir` is `fallback`"
  [command fallback]
  (let [segs (segments command)
        vars (assignments segs)
        raws (keep dir-signal segs)]
    (if (empty? raws)
      {:dir fallback :basis :explicit}
      (let [dir (reduce (fn [cur raw]
                          (if-let [t (and (string? raw)
                                          (when-let [e (expand raw vars)]
                                            (when (literal? e) e)))]
                            (resolve-against cur t)
                            (reduced ::unresolvable)))
                        fallback raws)]
        (cond
          (= ::unresolvable dir) {:dir fallback :basis :ambiguous}
          (not (usable-dir? dir)) {:dir fallback :basis :fallback}
          :else                   {:dir dir :basis :explicit})))))
