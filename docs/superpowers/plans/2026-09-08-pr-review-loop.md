# pr-review-loop Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a Claude Code plugin that, whenever agent A pushes to a PR branch or creates a PR in any repo, spawns an independent reviewer in the background and wakes A with its findings — replacing the GitHub Actions reviewer at zero Actions minutes.

**Architecture:** A `PostToolUse` hook with `asyncRewake: true` fires on A's `git push` / `gh pr create`. A babashka script gates on "does this branch have an open PR", takes a per-repo lock, precomputes the true diff (the hook is a plain process, so it escapes rtk's 411× diff truncation), assembles a prompt from a generic core plus an optional per-repo overlay, spawns `claude -p` with only `Read,Grep,Glob`, appends a pass to a `.git/`-local ledger, then writes the findings to **stderr** and exits **2** — which wakes A even if A's turn ended minutes ago.

**Tech Stack:** babashka 1.12.209 (`cheshire.core`, `babashka.fs`, `babashka.process`, `clojure.test`), Claude Code 2.1.263 plugin format, `gh` 2.83.2, `git`.

**Spec:** [../specs/2026-09-08-pr-review-loop.md](../specs/2026-09-08-pr-review-loop.md)

## Global Constraints

- Target directory is `~/.nixpkgs/claude-code-plugins`. It exists and is empty. It is **ordinary tracked content of the `~/.nixpkgs` repo** — never `git init` inside it. A local-directory marketplace does not require a git repo (verified); a nested one would hide every commit from anything diffing `~/.nixpkgs`.
- All commits land in the `~/.nixpkgs` repo. Task commands run with the plugin root as cwd, and git resolves paths from any subdirectory, so `git add hooks/foo.clj` works as written. `git add .` is used where a whole-subtree add is wanted — never `git add -A`, which from a subdirectory stages the entire parent repo.
- Plugin name: `pr-review-loop`. Marketplace name: `nixpkgs-plugins`.
- Hook payload channel is **stderr**. stdout is discarded by the harness even when stderr is empty. Every user-visible finding must go to stderr.
- Wake requires **exit code 2**. Exit 0 is silent. Any other exit code produces a `Failed with non-blocking status code:` notice and loses the pass.
- `rewakeMessage` / `rewakeSummary` do not work for third-party plugins. The wrapper text is fixed, so the **first stderr line must be self-describing**: repo, PR number, pass number.
- Never set a `timeout` on the trigger hook. It is unenforced for `async` hooks anyway, and the sync default (600 s) is above the worst observed review (343 s).
- B (the reviewer) gets exactly `--allowedTools "Read,Grep,Glob"`. No Bash, no Edit, no Write. This is what makes B provably non-mutating and immune to rtk rewriting.
- Never run `git diff` from inside B. `rtk git diff HEAD~1` returns 195 bytes where plain `git diff` returns 80 162.
- The trigger writes only under `<repo>/.git/pr-review*`. It never touches the work tree, the index, or any ref.
- Hard cap: **10 passes per PR**.
- All babashka namespaces live under `hooks/pr_review/` with `pr-review.*` namespace names. `bb.edn` sets `:paths ["hooks" "test"]`.
- Shell-outs are injected via an `opts` map key `:sh` so they can be stubbed in tests. Default is `babashka.process/sh`.
- Bump `.claude-plugin/plugin.json` `version` on every change you want to reinstall — `claude plugin update` is a no-op when the version string is unchanged.
- Plugin hooks do not hot-load. A new `claude` session is required after install and after every reinstall.
- File locking: `RandomAccessFile` + `.getChannel` + `.lock`, released by closing the channel inside `with-open`. Do **not** call `.release` — babashka blocks `sun.nio.ch.FileLockImpl.release`.
- Process liveness: `java.lang.ProcessHandle/of` → `.isPresent`, then `.isAlive`.

---

## WBS

| # | Work package | Deliverable | Task |
|---|---|---|---|
| 1 | Foundation | | |
| 1.1 | Plugin skeleton, `bb.edn`, test harness, install verified | `claude plugin list` shows it; `bb test` runs | Task 1 |
| 2 | Durable state | | |
| 2.1 | Pass ledger | append/read/next-pass/raise-count, flocked | Task 2 |
| 2.2 | Reviewer lock with supersede | acquire/release, PID liveness | Task 3 |
| 3 | Repo interrogation | | |
| 3.1 | git/gh shell layer | repo-root, branch, open-pr, head-sha | Task 4 |
| 3.2 | Context builder | true diff on disk + changed-file list | Task 5 |
| 4 | Reviewer | | |
| 4.1 | Prompt assembly | core + overlay + hint + pass state | Task 6 |
| 4.2 | Spawn and output parse | verdict, counts, fingerprints | Task 7 |
| 5 | Wiring | | |
| 5.1 | Trigger `-main` + `hooks.json` + end-to-end | a real push records a pass and wakes A | Task 8 |
| 6 | Prompts and driver | | |
| 6.1 | Generic review core | `hooks/review_core.md` | Task 9 |
| 6.2 | A's loop skill + manual command | `skills/`, `commands/` | Task 10 |
| 7 | Rollout | | |
| 7.1 | cchp overlay, README, install docs | overlay in cchp; README in plugin | Task 11 |

Dependencies: 1.1 → everything. 2.x and 3.x are mutually independent. 4.1 needs 3.2. 4.2 needs 4.1. 5.1 needs all of 2–4. 6.x needs 5.1 only for end-to-end checks. 7.1 last.

---

### Task 1: Plugin skeleton and test harness

**Files:**
- Modify: `~/.nixpkgs/.gitignore` (append the plugin's build-artifact patterns)
- Create: `~/.nixpkgs/claude-code-plugins/bb.edn`
- Create: `~/.nixpkgs/claude-code-plugins/.claude-plugin/marketplace.json`
- Create: `~/.nixpkgs/claude-code-plugins/.claude-plugin/plugin.json`
- Create: `~/.nixpkgs/claude-code-plugins/hooks/pr_review/version.clj`
- Test: `~/.nixpkgs/claude-code-plugins/test/pr_review/version_test.clj`

**Interfaces:**
- Consumes: nothing
- Produces: `pr-review.version/plugin-version` → String. The `bb test` task. The `review-trigger` bb task name that `hooks.json` will call in Task 8.

- [ ] **Step 1: Create the directory tree**

```bash
cd ~/.nixpkgs/claude-code-plugins
mkdir -p .claude-plugin hooks/pr_review skills/pr-review-loop commands test/pr_review
printf '\nclaude-code-plugins/.cpcache/\nclaude-code-plugins/.clj-kondo/.cache/\n' >> ~/.nixpkgs/.gitignore
```

Do **not** `git init` here. The plugin is ordinary tracked content inside the
`~/.nixpkgs` repo. A nested repo would turn `claude-code-plugins` into a gitlink
in the parent and hide every commit from anything diffing `~/.nixpkgs`.

`claude plugin marketplace add` does **not** require a git repo — verified by
installing a plugin from a plain directory. The `gitCommitSha` that appears in
`installed_plugins.json` is recorded when one is available, not demanded.

- [ ] **Step 2: Write `bb.edn`**

```clojure
{:paths ["hooks" "test"]
 :tasks
 {review-trigger {:doc  "PostToolUse hook entrypoint. Reads hook JSON on stdin."
                  :task (exec 'pr-review.trigger/-main)}

  test {:doc "Run every test namespace whose file exists."
        :requires ([babashka.fs :as fs] [clojure.test])
        :task (let [nses '[pr-review.version-test
                           pr-review.ledger-test
                           pr-review.lock-test
                           pr-review.gh-test
                           pr-review.context-test
                           pr-review.prompt-test
                           pr-review.reviewer-test
                           pr-review.trigger-test]
                    ;; Select on FILE EXISTENCE, never on whether `require`
                    ;; succeeds: a namespace with a compile error would be
                    ;; silently skipped, and a green `bb test` would then be
                    ;; hiding a broken test file.
                    present (filterv
                             (fn [ns-sym]
                               (fs/exists?
                                (str "test/"
                                     (-> (name ns-sym)
                                         (clojure.string/replace "-" "_")
                                         (clojure.string/replace "." "/"))
                                     ".clj")))
                             nses)]
                (apply require present)
                (let [{:keys [fail error]} (apply clojure.test/run-tests present)]
                  (System/exit (if (pos? (+ fail error)) 1 0))))}}}
```

Selecting on file existence lets `bb test` pass in Task 1 while later namespaces
do not exist yet, and picks each up automatically as its file lands — without
masking a namespace that exists but fails to compile.

`fs/exists?` resolves against the **process** working directory, not against
`bb.edn`, so `bb test` must always be run from the plugin root. Every test
command in this plan is prefixed with `cd ~/.nixpkgs/claude-code-plugins` for
that reason. `bb --config <abs>/bb.edn <task>` from elsewhere works for
`review-trigger` (which uses no relative paths) but would select zero test
namespaces.

- [ ] **Step 3: Write the failing test**

`test/pr_review/version_test.clj`:

```clojure
(ns pr-review.version-test
  (:require [clojure.test :refer [deftest is]]
            [pr-review.version :as version]))

(deftest plugin-version-is-semver
  (is (re-matches #"\d+\.\d+\.\d+" version/plugin-version)))

(deftest plugin-version-matches-manifest
  (is (= version/plugin-version (version/manifest-version "."))
      "plugin-version and plugin.json must agree — otherwise a version bump
       updates one and `claude plugin update` silently no-ops on the other"))
```

- [ ] **Step 4: Run it to make sure it fails**

```bash
cd ~/.nixpkgs/claude-code-plugins && bb test
```
Expected: FAIL. The test file exists so it is selected, then `require` throws
`Could not find namespace: pr-review.version`. That error — not `Ran 0 tests` —
is the signal that the harness is wired correctly and the implementation is
missing.

- [ ] **Step 5: Write the manifests and the minimal implementation**

`.claude-plugin/plugin.json`:

```json
{
  "name": "pr-review-loop",
  "version": "0.1.0",
  "description": "Local PR review loop. A PostToolUse hook spawns an independent reviewer on push and wakes the session with its findings.",
  "author": { "name": "yqrashawn" },
  "skills": ["./skills/pr-review-loop"],
  "commands": ["./commands"]
}
```

`.claude-plugin/marketplace.json`:

```json
{
  "$schema": "https://anthropic.com/claude-code/marketplace.schema.json",
  "name": "nixpkgs-plugins",
  "description": "Local plugins maintained in ~/.nixpkgs.",
  "owner": { "name": "yqrashawn" },
  "plugins": [
    {
      "name": "pr-review-loop",
      "description": "Local PR review loop replacing the GitHub Actions reviewer.",
      "source": "./",
      "category": "workflow"
    }
  ]
}
```

`hooks/pr_review/version.clj`:

```clojure
(ns pr-review.version
  (:require [cheshire.core :as json]))

(def plugin-version
  "Semver of this plugin, single source of truth for tests.
   Must be kept equal to .claude-plugin/plugin.json — bump both together,
   because `claude plugin update` is a no-op when the version is unchanged."
  "0.1.0")

(defn manifest-version
  "Version recorded in the plugin manifest at `plugin-root`."
  [plugin-root]
  (-> (str plugin-root "/.claude-plugin/plugin.json")
      slurp
      (json/parse-string true)
      :version))
```

- [ ] **Step 6: Run the tests to verify they pass**

```bash
cd ~/.nixpkgs/claude-code-plugins && bb test
```
Expected: PASS, `Ran 2 tests containing 2 assertions. 0 failures, 0 errors.`

- [ ] **Step 7: Verify the plugin installs**

```bash
cd ~/.nixpkgs/claude-code-plugins
git add . && git commit -qm "feat: pr-review-loop plugin skeleton"
claude plugin marketplace add ~/.nixpkgs/claude-code-plugins
claude plugin install pr-review-loop@nixpkgs-plugins
claude plugin list | rg -A3 pr-review-loop
```
Expected: `Status: ✔ enabled`, `Version: 0.1.0`.

- [ ] **Step 8: Verify the bb task is reachable the way `hooks.json` will call it**

```bash
echo '{}' | bb --config ~/.nixpkgs/claude-code-plugins/bb.edn review-trigger
```
Expected: an error naming `pr-review.trigger` — the task resolves, the namespace does not exist yet. If instead you get "No such task", `bb.edn` is wrong.

- [ ] **Step 9: Commit**

```bash
cd ~/.nixpkgs/claude-code-plugins
git add . && git commit -qm "test: version/manifest agreement"
```

---

### Task 2: Pass ledger

> **Shipped code is authoritative for this task.** Two fix rounds hardened it
> after the blocks below were written: `append-pass!` now publishes via a temp
> file plus `ATOMIC_MOVE`, `acquire!` and `release!` run under a shared guard
> flock from `pr-review.flock`, `release!` is a compare-and-delete on `:pid`,
> and the test files carry six tests the blocks below do not. Read
> `claude-code-plugins/hooks/pr_review/{flock,ledger,lock}.clj` and
> `claude-code-plugins/test/pr_review/{ledger,lock}_test.clj` as built, and see
> commits `07ef0aeb0`, `11e85b8dd`. Transcribing the Step 1 test block and the
> Step 3 implementation block verbatim would produce a self-inconsistent pair.

**Files:**
- Create: `~/.nixpkgs/claude-code-plugins/hooks/pr_review/flock.clj`
- Create: `~/.nixpkgs/claude-code-plugins/hooks/pr_review/ledger.clj`
- Test: `~/.nixpkgs/claude-code-plugins/test/pr_review/ledger_test.clj`

**Interfaces:**
- Consumes: nothing
- Produces:
  - `(flock/with-file-lock path f)` → runs no-arg `f` while holding an exclusive
    OS-level lock on `path`, returns `f`'s value
  - `(flock/guard-path path)` → String, `path` + `".guard"` — the sibling path to
    flock when the locked operation itself renames or replaces `path`; also used
    by Task 3's `pr-review.lock`
  - `(ledger-path repo-root)` → String, `<repo-root>/.git/pr-review-ledger.jsonl`
  - `(read-passes repo-root pr-number)` → vector of entry maps, oldest first
  - `(next-pass-number repo-root pr-number)` → long, 1 when none
  - `(append-pass! repo-root entry)` → the entry as written
  - `(raise-count repo-root pr-number fingerprint)` → long
  - `(cap-reached? repo-root pr-number)` → boolean, true at ≥ `max-passes`
  - `max-passes` → 10
  - Entry shape: `{:pr long :sha String :pass long :verdict String :blocking long :followup long :coverage long :fingerprints [String] :ts long}`

- [ ] **Step 1: Write the failing test**

`test/pr_review/ledger_test.clj`:

```clojure
(ns pr-review.ledger-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [pr-review.flock :as flock]
            [pr-review.ledger :as ledger]))

(defn- tmp-repo
  "A directory with a .git subdir, standing in for a clone."
  []
  (let [d (str (fs/create-temp-dir {:prefix "pr-review-ledger"}))]
    (fs/create-dirs (str d "/.git"))
    d))

(deftest ledger-path-is-under-dot-git
  (is (= "/r/.git/pr-review-ledger.jsonl" (ledger/ledger-path "/r"))))

(deftest empty-ledger-starts-at-pass-one
  (let [r (tmp-repo)]
    (is (= [] (ledger/read-passes r 370)))
    (is (= 1 (ledger/next-pass-number r 370)))
    (is (false? (ledger/cap-reached? r 370)))))

(deftest passes-are-per-pr-and-monotonic
  (let [r (tmp-repo)]
    (ledger/append-pass! r {:pr 370 :sha "aaa" :pass 1 :verdict "NOT_MERGEABLE"
                            :blocking 2 :followup 0 :coverage 0 :fingerprints []})
    (ledger/append-pass! r {:pr 371 :sha "bbb" :pass 1 :verdict "MERGEABLE"
                            :blocking 0 :followup 1 :coverage 0 :fingerprints []})
    (ledger/append-pass! r {:pr 370 :sha "ccc" :pass 2 :verdict "MERGEABLE"
                            :blocking 0 :followup 1 :coverage 0 :fingerprints []})
    (testing "reads are filtered by PR"
      (is (= ["aaa" "ccc"] (mapv :sha (ledger/read-passes r 370))))
      (is (= ["bbb"] (mapv :sha (ledger/read-passes r 371)))))
    (testing "next pass number counts only that PR"
      (is (= 3 (ledger/next-pass-number r 370)))
      (is (= 2 (ledger/next-pass-number r 371))))))

(deftest append-stamps-a-timestamp
  (let [r (tmp-repo)
        e (ledger/append-pass! r {:pr 1 :sha "x" :pass 1 :verdict "MERGEABLE"
                                  :blocking 0 :followup 0 :coverage 0 :fingerprints []})]
    (is (pos? (:ts e)) "append-pass! must stamp :ts so the cap can be reasoned about over time")))

(deftest raise-count-counts-fingerprint-occurrences
  (let [r (tmp-repo)
        fp "src/a.clj:12:correctness/followup"]
    (ledger/append-pass! r {:pr 5 :sha "a" :pass 1 :verdict "NOT_MERGEABLE"
                            :blocking 1 :followup 0 :coverage 0 :fingerprints [fp]})
    (is (= 1 (ledger/raise-count r 5 fp)))
    (ledger/append-pass! r {:pr 5 :sha "b" :pass 2 :verdict "MERGEABLE"
                            :blocking 0 :followup 1 :coverage 0 :fingerprints [fp]})
    (is (= 2 (ledger/raise-count r 5 fp))
        "a second raise must be visible so the one-re-raise rule can fire")
    (is (= 0 (ledger/raise-count r 5 "other:1:style")))))

(deftest cap-blocks-at-max-passes
  (let [r (tmp-repo)]
    (doseq [n (range 1 (inc ledger/max-passes))]
      (ledger/append-pass! r {:pr 9 :sha (str n) :pass n :verdict "NOT_MERGEABLE"
                              :blocking 1 :followup 0 :coverage 0 :fingerprints []}))
    (is (true? (ledger/cap-reached? r 9))
        "at max-passes the trigger must refuse to spawn another reviewer")
    (is (false? (ledger/cap-reached? r 10)) "the cap is per PR, not global")))

(deftest corrupt-lines-are-skipped-not-fatal
  (let [r (tmp-repo)]
    (ledger/append-pass! r {:pr 2 :sha "ok" :pass 1 :verdict "MERGEABLE"
                            :blocking 0 :followup 0 :coverage 0 :fingerprints []})
    (spit (ledger/ledger-path r) "{not json\n" :append true)
    (is (= ["ok"] (mapv :sha (ledger/read-passes r 2)))
        "a truncated write from a killed reviewer must not break every later read")))

(deftest interrupted-write-does-not-lose-prior-passes
  (let [r (tmp-repo)]
    (ledger/append-pass! r {:pr 42 :sha "first" :pass 1 :verdict "MERGEABLE"
                            :blocking 0 :followup 0 :coverage 0 :fingerprints []})
    ;; Simulate a reviewer killed after the new pass is durably written to the
    ;; temp file but before it is published — the exact window lock/acquire!
    ;; can interrupt by killing a superseded reviewer mid-run.
    (with-redefs [ledger/atomic-replace! (fn [_ _] (throw (ex-info "simulated crash before publish" {})))]
      (is (thrown? Exception
                   (ledger/append-pass! r {:pr 42 :sha "second" :pass 2 :verdict "MERGEABLE"
                                           :blocking 0 :followup 0 :coverage 0 :fingerprints []}))))
    (is (= ["first"] (mapv :sha (ledger/read-passes r 42)))
        "a crash between the temp write and the atomic rename must leave every
         previously recorded pass intact, not zero the ledger")))

(deftest append-pass-flocks-a-guard-file-not-the-ledger-path
  (let [r (tmp-repo)
        seen (atom [])]
    (with-redefs [flock/with-file-lock (fn [path f] (swap! seen conj path) (f))]
      (ledger/append-pass! r {:pr 77 :sha "one" :pass 1 :verdict "MERGEABLE"
                              :blocking 0 :followup 0 :coverage 0 :fingerprints []})
      (ledger/append-pass! r {:pr 77 :sha "two" :pass 2 :verdict "MERGEABLE"
                              :blocking 0 :followup 0 :coverage 0 :fingerprints []}))
    (testing "both sequential appends still land"
      (is (= ["one" "two"] (mapv :sha (ledger/read-passes r 77)))))
    (testing "the flock target is a sibling guard file, never the ledger path append-pass! renames over"
      (is (= [(flock/guard-path (ledger/ledger-path r)) (flock/guard-path (ledger/ledger-path r))]
             @seen))
      (is (not-any? #{(ledger/ledger-path r)} @seen)
          "flocking the path that gets renamed over lets a second process later lock a
           different inode after the rename and run concurrently with this one — the
           exact defect this test guards against"))))
```

- [ ] **Step 2: Run the tests to verify they fail**

```bash
cd ~/.nixpkgs/claude-code-plugins && bb test
```
Expected: FAIL with `java.io.FileNotFoundException` naming `pr_review/ledger`, exit 1. The
selector picks the test file up because it exists, then `(apply require present)`
throws on the missing implementation namespace. That crash — not a reduced test
count — is the correct pre-implementation failure.

- [ ] **Step 3: Write the implementation**

`hooks/pr_review/flock.clj`:

```clojure
(ns pr-review.flock
  "Cross-process mutual exclusion via an OS-level file lock.

   Shared by pr-review.ledger and pr-review.lock so both modules serialize
   through the same mechanism instead of each hand-rolling file locking.

   Invariant: never flock a path that the locked operation itself renames or
   replaces (e.g. an atomic-rename publish). A rename swaps the inode at
   that path, so a `RandomAccessFile` opened and locked before the rename
   still refers to the old, now-orphaned inode — a second process that
   opens the same path afterwards locks a *different* inode and proceeds
   concurrently, breaking mutual exclusion exactly when it matters most.
   Always flock a sibling guard file (`guard-path`) that the locked
   operation never touches instead."
  (:require [babashka.fs :as fs])
  (:import [java.io RandomAccessFile]))

(defn guard-path
  "Sibling path to flock when serializing an operation that renames or
   replaces `path` itself. Never flock `path` directly in that case — see
   the namespace docstring. Just appends \".guard\" to `path`."
  [path]
  (str path ".guard"))

(defn with-file-lock
  "Run `f` (a no-arg function) while holding an exclusive lock on `path`,
   creating `path` and its parent directories first if missing. Returns
   `f`'s return value.

   The lock is released by closing the channel, not by calling .release —
   babashka does not allow sun.nio.ch.FileLockImpl.release."
  [path f]
  (fs/create-dirs (fs/parent path))
  (when-not (fs/exists? path) (spit path ""))
  (with-open [raf (RandomAccessFile. (str path) "rw")]
    (.lock (.getChannel raf))
    (f)))
```

`hooks/pr_review/ledger.clj`:

```clojure
(ns pr-review.ledger
  "Append-only, per-clone record of review passes.

   Lives under .git/ so it survives session restarts, context compaction and
   `claude` upgrades, needs no network, and works before a PR exists. Same
   precedent as Claude Code's own .git/claude-trailers."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.string :as str]
            [pr-review.flock :as flock])
  (:import [java.nio.file CopyOption Files StandardCopyOption]))

(def max-passes
  "Hard cap on review passes per PR. Ported from a real incident: a sibling
   repo ran twelve passes and ~40 correctness findings on one PR without
   converging, two thirds of them defects in fixes for the previous pass."
  10)

(def ^:private max-lines
  "Ledger is trimmed to this many lines under the write lock. At ~200 bytes a
   line this bounds the file at ~100KB."
  500)

(defn ledger-path
  [repo-root]
  (str repo-root "/.git/pr-review-ledger.jsonl"))

(defn- parse-line
  [line]
  (try (json/parse-string line true)
       (catch Exception _ nil)))

(defn- read-all
  [repo-root]
  (let [p (ledger-path repo-root)]
    (if-not (fs/exists? p)
      []
      (into [] (keep parse-line) (str/split-lines (slurp p))))))

(defn read-passes
  "Every recorded pass for `pr-number`, oldest first. Unparseable lines are
   skipped: a reviewer killed mid-write must not break all later reads."
  [repo-root pr-number]
  (filterv #(= pr-number (:pr %)) (read-all repo-root)))

(defn next-pass-number
  [repo-root pr-number]
  (inc (count (read-passes repo-root pr-number))))

(defn cap-reached?
  [repo-root pr-number]
  (>= (count (read-passes repo-root pr-number)) max-passes))

(defn raise-count
  "How many passes have reported `fingerprint` for `pr-number`.
   The one-re-raise rule fires when this is already >= 2."
  [repo-root pr-number fingerprint]
  (count (filter #(contains? (set (:fingerprints %)) fingerprint)
                 (read-passes repo-root pr-number))))

(defn- atomic-replace!
  "Atomically replace `path`'s content with `tmp`'s.

   Renamed into place rather than written in place: `spit` truncates on
   open, so a process killed between truncate and flush would zero the
   entire ledger — exactly what lock/acquire! does to a superseded
   reviewer mid-run. Same filesystem (tmp is a sibling of path), so
   ATOMIC_MOVE is a real rename, not a copy."
  [tmp path]
  (Files/move (fs/path tmp) (fs/path path)
              (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE])))

(defn append-pass!
  "Append one pass entry, stamping :ts. Trims to `max-lines` under the same
   lock so concurrent triggers cannot interleave a read-trim-write. Writes
   the full trimmed content to a temp file and renames it into place, so a
   process killed mid-write can never truncate the previously recorded
   passes — it only ever loses its own not-yet-published entry.

   Flocks `(flock/guard-path p)`, never `p` itself: `p` is the path this
   function renames over, and a lock held on a path that gets renamed away
   from under it stops protecting anything the instant the rename happens.
   See pr-review.flock's namespace docstring."
  [repo-root entry]
  (let [entry (assoc entry :ts (System/currentTimeMillis))
        p (ledger-path repo-root)]
    (flock/with-file-lock (flock/guard-path p)
      (fn []
        (let [existing (if (fs/exists? p)
                         (vec (remove str/blank? (str/split-lines (slurp p))))
                         [])
              lines (conj existing (json/generate-string entry))
              kept (vec (take-last max-lines lines))
              tmp (str p ".tmp")]
          (spit tmp (str (str/join "\n" kept) "\n"))
          (atomic-replace! tmp p))))
    entry))
```

- [ ] **Step 4: Run the tests to verify they pass**

```bash
cd ~/.nixpkgs/claude-code-plugins && bb test
```
Expected: PASS, `Ran 9 tests`.

- [ ] **Step 5: Commit**

```bash
cd ~/.nixpkgs/claude-code-plugins
git add hooks/pr_review/flock.clj hooks/pr_review/ledger.clj test/pr_review/ledger_test.clj
git commit -qm "feat: per-clone review pass ledger under .git/"
```

---

### Task 3: Reviewer lock with supersede

> **Shipped code is authoritative for this task.** Two fix rounds hardened it
> after the blocks below were written: `append-pass!` now publishes via a temp
> file plus `ATOMIC_MOVE`, `acquire!` and `release!` run under a shared guard
> flock from `pr-review.flock`, `release!` is a compare-and-delete on `:pid`,
> and the test files carry six tests the blocks below do not. Read
> `claude-code-plugins/hooks/pr_review/{flock,ledger,lock}.clj` and
> `claude-code-plugins/test/pr_review/{ledger,lock}_test.clj` as built, and see
> commits `07ef0aeb0`, `11e85b8dd`. Transcribing the Step 1 test block and the
> Step 3 implementation block verbatim would produce a self-inconsistent pair.

**Files:**
- Create: `~/.nixpkgs/claude-code-plugins/hooks/pr_review/lock.clj`
- Test: `~/.nixpkgs/claude-code-plugins/test/pr_review/lock_test.clj`

**Interfaces:**
- Consumes: `pr-review.flock/{with-file-lock,guard-path}` — created alongside Task 2, so Task 2 must land before this task.
- Produces:
  - `(lock-path repo-root)` → String, `<repo-root>/.git/pr-review.lock`
  - `(read-lock repo-root)` → nil or `{:pid long :pr long :sha String :started long}`
  - `(alive? pid)` → boolean
  - `(acquire! repo-root {:pr long :sha String} opts)` → `{:status :acquired}` | `{:status :duplicate}` | `{:status :superseded :killed-pid long}`
  - `(release! repo-root)` / `(release! repo-root opts)` → nil. Deletes the lock
    record only if it is still held by `opts`'s `:pid` (default this process),
    under the same guard flock `acquire!` uses
  - `opts` accepts `:kill-fn` (default kills by PID) and `:pid` (default this process)

- [ ] **Step 1: Write the failing test**

`test/pr_review/lock_test.clj`:

```clojure
(ns pr-review.lock-test
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
            [pr-review.flock :as flock]
            [pr-review.lock :as lock]))

(defn- tmp-repo []
  (let [d (str (fs/create-temp-dir {:prefix "pr-review-lock"}))]
    (fs/create-dirs (str d "/.git"))
    d))

(defn- write-lock! [repo m]
  (spit (lock/lock-path repo) (json/generate-string m)))

(deftest lock-path-is-under-dot-git
  (is (= "/r/.git/pr-review.lock" (lock/lock-path "/r"))))

(deftest alive-tracks-real-processes
  (is (true? (lock/alive? (.pid (java.lang.ProcessHandle/current)))))
  (is (false? (lock/alive? 999999)) "an absent PID must read as dead, not as held"))

(deftest acquire-on-free-repo-succeeds-and-records-identity
  (let [r (tmp-repo)
        res (lock/acquire! r {:pr 370 :sha "abc"} {:pid 4242})]
    (is (= :acquired (:status res)))
    (is (= {:pid 4242 :pr 370 :sha "abc"}
           (select-keys (lock/read-lock r) [:pid :pr :sha])))
    (is (pos? (:started (lock/read-lock r))))))

(deftest same-sha-in-flight-is-a-duplicate
  (let [r (tmp-repo)
        self (.pid (java.lang.ProcessHandle/current))]
    (write-lock! r {:pid self :pr 370 :sha "abc" :started 1})
    (is (= :duplicate (:status (lock/acquire! r {:pr 370 :sha "abc"} {:pid 1})))
        "two hooks for one push must not run two reviewers")
    (is (= self (:pid (lock/read-lock r))) "the incumbent keeps the lock")))

(deftest newer-sha-supersedes-and-kills-the-incumbent
  (let [r (tmp-repo)
        self (.pid (java.lang.ProcessHandle/current))
        killed (atom nil)]
    (write-lock! r {:pid self :pr 370 :sha "old" :started 1})
    (let [res (lock/acquire! r {:pr 370 :sha "new"}
                             {:pid 777 :kill-fn #(reset! killed %)})]
      (is (= :superseded (:status res)))
      (is (= self (:killed-pid res)))
      (is (= self @killed) "reviewing a stale SHA is waste; kill it")
      (is (= {:pid 777 :sha "new"} (select-keys (lock/read-lock r) [:pid :sha]))))))

(deftest dead-holder-lock-is-taken-without-killing
  (let [r (tmp-repo)
        killed (atom nil)]
    (write-lock! r {:pid 999999 :pr 370 :sha "old" :started 1})
    (let [res (lock/acquire! r {:pr 370 :sha "new"}
                             {:pid 5 :kill-fn #(reset! killed %)})]
      (is (= :acquired (:status res))
          "a crashed reviewer must not block every future review")
      (is (nil? @killed) "nothing to kill when the holder is already gone"))))

(deftest corrupt-lock-file-is-treated-as-free
  (let [r (tmp-repo)]
    (spit (lock/lock-path r) "{not json")
    (is (= :acquired (:status (lock/acquire! r {:pr 1 :sha "s"} {:pid 6}))))))

(deftest release-removes-the-lock
  (let [r (tmp-repo)]
    (lock/acquire! r {:pr 1 :sha "s"} {:pid 7})
    (lock/release! r)
    (is (nil? (lock/read-lock r)))))

(deftest acquire-runs-under-the-shared-flock-on-the-guard-path
  (let [r (tmp-repo)
        seen (atom nil)]
    (with-redefs [flock/with-file-lock (fn [path f] (reset! seen path) (f))]
      (is (= :acquired (:status (lock/acquire! r {:pr 1 :sha "s"} {:pid 9})))))
    (is (some? @seen)
        "acquire! must run its read-check-write through pr-review.flock/with-file-lock")
    (is (= (flock/guard-path (lock/lock-path r)) @seen)
        "acquire! must flock the sibling guard path")
    (is (not= (lock/lock-path r) @seen)
        "acquire! must never flock the lock record path itself: acquire! rewrites
         that path, so a lock held on it would stop protecting anything the moment
         it's rewritten")))

(deftest acquire-on-lock-missing-pid-is-acquired-not-an-npe
  (let [r (tmp-repo)]
    (spit (lock/lock-path r) "{}")
    (is (= :acquired (:status (lock/acquire! r {:pr 1 :sha "s"} {:pid 10})))
        "a lock record that is valid JSON but missing :pid must read as free, not
         throw: read-lock returning {} truthy would send (alive? nil) into
         (long nil), an NPE")))
```

- [ ] **Step 2: Run the tests to verify they fail**

```bash
cd ~/.nixpkgs/claude-code-plugins && bb test
```
Expected: FAIL with `java.io.FileNotFoundException` naming `pr_review/lock`, exit 1.

- [ ] **Step 3: Write the implementation**

`hooks/pr_review/lock.clj`:

```clojure
(ns pr-review.lock
  "At most one live reviewer per clone, always on the newest pushed SHA.

   Two rapid pushes must not leave a reviewer grinding on a SHA that is
   already stale — the newer push kills the older reviewer and takes over."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [pr-review.flock :as flock]))

(defn lock-path
  [repo-root]
  (str repo-root "/.git/pr-review.lock"))

(defn read-lock
  "Current lock record, or nil when absent, unparseable, or missing a
   usable :pid. A corrupt or incomplete lock reads as free: a half-written
   file must not wedge the loop forever."
  [repo-root]
  (let [p (lock-path repo-root)]
    (when (fs/exists? p)
      (let [parsed (try (json/parse-string (slurp p) true)
                        (catch Exception _ nil))]
        (when (:pid parsed)
          parsed)))))

(defn alive?
  [pid]
  (let [h (java.lang.ProcessHandle/of (long pid))]
    (and (.isPresent h) (.isAlive (.get h)))))

(defn- default-kill!
  [pid]
  (try (p/sh ["kill" (str pid)]) (catch Exception _ nil)))

(defn- write-lock!
  [repo-root {:keys [pid pr sha]}]
  (fs/create-dirs (fs/parent (lock-path repo-root)))
  (spit (lock-path repo-root)
        (json/generate-string {:pid pid :pr pr :sha sha
                               :started (System/currentTimeMillis)})))

(defn acquire!
  "Take the reviewer lock for (`pr`, `sha`).

   :duplicate  — a live reviewer already holds this exact SHA. Caller exits 0.
   :superseded — a live reviewer held an older SHA; it was killed. Proceed.
   :acquired   — the lock was free, corrupt, or held by a dead process.

   The whole read-check-write runs under a shared flock on
   `(flock/guard-path (lock-path repo-root))`, never on the lock path
   itself: acquire! rewrites (and release! deletes) that path, so a lock
   held on it would stop protecting anything the instant it's rewritten.
   A guard file that acquire! never touches keeps the flock's identity
   independent of the record's lifecycle, so two concurrent triggers in
   the same clone cannot both observe a free or dead lock and both
   proceed."
  [repo-root {:keys [pr sha]} {:keys [pid kill-fn]}]
  (flock/with-file-lock
    (flock/guard-path (lock-path repo-root))
    (fn []
      (let [pid (or pid (.pid (java.lang.ProcessHandle/current)))
            kill-fn (or kill-fn default-kill!)
            held (read-lock repo-root)]
        (cond
          (and held (alive? (:pid held)) (= sha (:sha held)))
          {:status :duplicate}

          (and held (alive? (:pid held)))
          (do (kill-fn (:pid held))
              (write-lock! repo-root {:pid pid :pr pr :sha sha})
              {:status :superseded :killed-pid (:pid held)})

          :else
          (do (write-lock! repo-root {:pid pid :pr pr :sha sha})
              {:status :acquired}))))))

(defn release!
  "Release the lock, but only when it is still held by `pid` (default this
   process's own pid, matching acquire!'s default). Runs under the same
   guard flock acquire! uses, so a reviewer finishing normally can never
   interleave with an in-flight acquire! that is concurrently superseding
   it.

   Both halves matter together: without the flock, release! could still
   run between acquire!'s kill and its write of the new record; without
   the pid check, release! would delete whatever record it finds even
   after losing that race. Either alone lets a reviewer that has already
   been superseded delete the new holder's record — the lock then reads
   free while a reviewer is actually still running, which is exactly what
   acquire!'s duplicate/superseded logic exists to prevent."
  ([repo-root] (release! repo-root {}))
  ([repo-root {:keys [pid]}]
   (let [pid (or pid (.pid (java.lang.ProcessHandle/current)))]
     (flock/with-file-lock
       (flock/guard-path (lock-path repo-root))
       (fn []
         (let [held (read-lock repo-root)]
           (when (= pid (:pid held))
             (fs/delete-if-exists (lock-path repo-root))))))
     nil)))
```

- [ ] **Step 4: Run the tests to verify they pass**

```bash
cd ~/.nixpkgs/claude-code-plugins && bb test
```
Expected: PASS, `Ran 17 tests`.

- [ ] **Step 5: Commit**

```bash
cd ~/.nixpkgs/claude-code-plugins
git add hooks/pr_review/lock.clj test/pr_review/lock_test.clj
git commit -qm "feat: reviewer lock with stale-SHA supersede"
```

---

### Task 4: git and gh shell layer

**Files:**
- Create: `~/.nixpkgs/claude-code-plugins/hooks/pr_review/gh.clj`
- Test: `~/.nixpkgs/claude-code-plugins/test/pr_review/gh_test.clj`

**Interfaces:**
- Consumes: nothing
- Produces (every fn takes a trailing `opts` map honouring `:sh`):
  - `(default-sh args dir)` → `{:exit long :out String :err String}`
  - `(repo-root cwd opts)` → String or nil
  - `(current-branch repo-root opts)` → String or nil
  - `(head-sha repo-root opts)` → String or nil
  - `(open-pr repo-root branch opts)` → nil or `{:number long :isDraft boolean :baseRefName String}`
  - `(merge-base repo-root base-ref opts)` → String or nil
  - `(diff repo-root base sha opts)` → String

- [ ] **Step 1: Write the failing test**

`test/pr_review/gh_test.clj`:

```clojure
(ns pr-review.gh-test
  (:require [clojure.test :refer [deftest is testing]]
            [pr-review.gh :as gh]))

(defn- stub
  "A :sh replacement that answers from `responses`, keyed by the first two
   argv elements, and records every invocation in `calls`."
  [responses calls]
  (fn [args _dir]
    (swap! calls conj args)
    (get responses (vec (take 2 args))
         {:exit 1 :out "" :err "unstubbed"})))

(deftest repo-root-uses-rev-parse
  (let [calls (atom [])
        sh (stub {["git" "rev-parse"] {:exit 0 :out "/repo\n" :err ""}} calls)]
    (is (= "/repo" (gh/repo-root "/repo/sub" {:sh sh})))
    (is (= ["git" "rev-parse" "--show-toplevel"] (first @calls)))))

(deftest repo-root-is-nil-outside-a-repo
  (let [sh (fn [_ _] {:exit 128 :out "" :err "not a git repository"})]
    (is (nil? (gh/repo-root "/tmp" {:sh sh})))))

(deftest current-branch-trims-output
  (let [sh (stub {["git" "rev-parse"] {:exit 0 :out "feat/x\n" :err ""}} (atom []))]
    (is (= "feat/x" (gh/current-branch "/repo" {:sh sh})))))

(deftest detached-head-has-no-branch
  (let [sh (fn [_ _] {:exit 0 :out "HEAD\n" :err ""})]
    (is (nil? (gh/current-branch "/repo" {:sh sh}))
        "a detached HEAD has no branch, so there is no PR to look up")))

(deftest open-pr-parses-the-first-match
  (let [calls (atom [])
        sh (stub {["gh" "pr"]
                  {:exit 0
                   :out "[{\"number\":370,\"isDraft\":true,\"baseRefName\":\"main\"}]"
                   :err ""}}
                 calls)]
    (is (= {:number 370 :isDraft true :baseRefName "main"}
           (gh/open-pr "/repo" "feat/x" {:sh sh})))
    (testing "the query is scoped to the branch and to open PRs"
      (let [argv (first @calls)]
        (is (some #{"--head"} argv))
        (is (some #{"feat/x"} argv))
        (is (some #{"--state"} argv))
        (is (some #{"open"} argv))))))

(deftest no-open-pr-returns-nil
  (let [sh (stub {["gh" "pr"] {:exit 0 :out "[]" :err ""}} (atom []))]
    (is (nil? (gh/open-pr "/repo" "feat/x" {:sh sh}))
        "a push to a branch with no open PR must be silent, not an error")))

(deftest gh-failure-returns-nil-rather-than-throwing
  (let [sh (fn [_ _] {:exit 1 :out "" :err "gh: not authenticated"})]
    (is (nil? (gh/open-pr "/repo" "feat/x" {:sh sh}))
        "an unauthenticated gh must degrade to silence, never crash the hook")))

(deftest diff-asks-for-the-three-dot-range
  (let [calls (atom [])
        sh (stub {["git" "diff"] {:exit 0 :out "DIFFTEXT" :err ""}} calls)]
    (is (= "DIFFTEXT" (gh/diff "/repo" "base1" "head1" {:sh sh})))
    (is (= ["git" "diff" "base1...head1"] (first @calls))
        "three-dot compares against the merge base, which is what a review wants")))
```

- [ ] **Step 2: Run the tests to verify they fail**

```bash
cd ~/.nixpkgs/claude-code-plugins && bb test
```
Expected: FAIL with `java.io.FileNotFoundException` naming `pr_review/gh`, exit 1.

- [ ] **Step 3: Write the implementation**

`hooks/pr_review/gh.clj`:

```clojure
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
```

- [ ] **Step 4: Run the tests to verify they pass**

```bash
cd ~/.nixpkgs/claude-code-plugins && bb test
```
Expected: PASS, `Ran 25 tests`.

- [ ] **Step 5: Verify against the real tools once**

```bash
cd ~/workspace/home/claude-code-http-proxy
bb --config ~/.nixpkgs/claude-code-plugins/bb.edn \
   -e '(require (quote [pr-review.gh :as gh])) (let [r (gh/repo-root "." {})] (println "root:" r "branch:" (gh/current-branch r {})))'
```

`--config` must come **before** `-e`. Placed after, babashka silently ignores it
— the expression then runs with no classpath and dies on the first `require`,
which looks like a missing namespace rather than a misplaced flag.

Expected: the cchp repo root and its current branch. If `gh` prompts or errors, note it — R31 in the spec flags the two-account `gh` config as untested.

- [ ] **Step 6: Commit**

```bash
cd ~/.nixpkgs/claude-code-plugins
git add hooks/pr_review/gh.clj test/pr_review/gh_test.clj
git commit -qm "feat: injectable git/gh shell layer"
```

---

### Task 5: Context builder

**Files:**
- Create: `~/.nixpkgs/claude-code-plugins/hooks/pr_review/context.clj`
- Test: `~/.nixpkgs/claude-code-plugins/test/pr_review/context_test.clj`

**Interfaces:**
- Consumes: `pr-review.gh/{merge-base,diff}` (head SHA is supplied by the caller, not read here)
- Produces:
  - `(context-dir repo-root)` → String, `<repo-root>/.git/pr-review-context`
  - `(build! repo-root {:pr long :sha String :base-ref String} opts)` → `{:diff-path String :changed-files [String] :base String :sha String :diff-bytes long}`
  - `(prune! repo-root keep)` → long, number of files deleted

- [ ] **Step 1: Write the failing test**

`test/pr_review/context_test.clj`:

```clojure
(ns pr-review.context-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [pr-review.context :as context]))

(defn- tmp-repo []
  (let [d (str (fs/create-temp-dir {:prefix "pr-review-ctx"}))]
    (fs/create-dirs (str d "/.git"))
    d))

(def ^:private sample-diff
  (str "diff --git a/src/a.clj b/src/a.clj\n"
       "index 111..222 100644\n"
       "--- a/src/a.clj\n"
       "+++ b/src/a.clj\n"
       "@@ -1 +1 @@\n"
       "-(def a 1)\n"
       "+(def a 2)\n"
       "diff --git a/test/b_test.clj b/test/b_test.clj\n"
       "new file mode 100644\n"
       "--- /dev/null\n"
       "+++ b/test/b_test.clj\n"
       "@@ -0,0 +1 @@\n"
       "+(ns b-test)\n"))

(defn- stub-sh [_args _dir] {:exit 0 :out "" :err ""})

(deftest build-writes-the-full-diff-to-disk
  (let [r (tmp-repo)
        res (context/build! r {:pr 370 :sha "headsha" :base-ref "main"}
                            {:merge-base-fn (constantly "basesha")
                             :diff-fn (constantly sample-diff)
                             :sh stub-sh})]
    (is (= "basesha" (:base res)))
    (is (= "headsha" (:sha res)))
    (is (fs/exists? (:diff-path res)))
    (is (= sample-diff (slurp (:diff-path res)))
        "the diff on disk must be byte-identical to git's output — the whole
         point of precomputing it is that B never runs a truncating rtk git diff")
    (is (= (count sample-diff) (:diff-bytes res)))))

(deftest diff-path-is-namespaced-by-sha
  (let [r (tmp-repo)
        res (context/build! r {:pr 370 :sha "abc123" :base-ref "main"}
                            {:merge-base-fn (constantly "b") :diff-fn (constantly "d")
                             :sh stub-sh})]
    (is (= (str (context/context-dir r) "/abc123.diff") (:diff-path res))
        "one file per SHA so a superseded reviewer's diff is never overwritten
         under it mid-read")))

(deftest changed-files-are-extracted-from-the-diff
  (let [r (tmp-repo)
        res (context/build! r {:pr 1 :sha "s" :base-ref "main"}
                            {:merge-base-fn (constantly "b")
                             :diff-fn (constantly sample-diff)
                             :sh stub-sh})]
    (is (= ["src/a.clj" "test/b_test.clj"] (:changed-files res)))))

(deftest missing-merge-base-falls-back-to-the-base-ref
  (let [r (tmp-repo)
        res (context/build! r {:pr 1 :sha "s" :base-ref "main"}
                            {:merge-base-fn (constantly nil)
                             :diff-fn (constantly "d") :sh stub-sh})]
    (is (= "origin/main" (:base res))
        "an unfetched base must still produce a reviewable range, not nil")))

(deftest empty-diff-is-reported-not-hidden
  (let [r (tmp-repo)
        res (context/build! r {:pr 1 :sha "s" :base-ref "main"}
                            {:merge-base-fn (constantly "b")
                             :diff-fn (constantly "") :sh stub-sh})]
    (is (= 0 (:diff-bytes res)))
    (is (= [] (:changed-files res)))
    (testing "the file still exists so the prompt never names a missing path"
      (is (fs/exists? (:diff-path res))))))

(deftest prune-keeps-the-newest-contexts
  (let [r (tmp-repo)]
    (doseq [n ["a" "b" "c" "d"]]
      (context/build! r {:pr 1 :sha n :base-ref "main"}
                      {:merge-base-fn (constantly "b") :diff-fn (constantly "d")
                       :sh stub-sh})
      (Thread/sleep 5))
    (is (= 2 (context/prune! r 2)))
    (is (= 2 (count (fs/glob (context/context-dir r) "*.diff"))))))
```

- [ ] **Step 2: Run the tests to verify they fail**

```bash
cd ~/.nixpkgs/claude-code-plugins && bb test
```
Expected: FAIL with `java.io.FileNotFoundException` naming `pr_review/context`, exit 1.

- [ ] **Step 3: Write the implementation**

`hooks/pr_review/context.clj`:

```clojure
(ns pr-review.context
  "Precompute everything the reviewer needs to read, so the reviewer needs no
   Bash at all.

   Dropping Bash from B removes two problems in one move: rtk cannot truncate
   a diff B never runs, and a reviewer with no shell cannot mutate the tree."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [pr-review.gh :as gh]))

(defn context-dir
  [repo-root]
  (str repo-root "/.git/pr-review-context"))

(defn changed-files
  "Repo-relative paths touched by a unified diff, in first-appearance order.

   Takes the b/ side, not the a/ side: for a rename they differ, and the
   reviewer needs the path that exists after the change."
  [diff-text]
  (->> (str/split-lines (or diff-text ""))
       (keep (fn [line]
               (when (str/starts-with? line "diff --git ")
                 (let [[_ _a b] (re-find #"^diff --git a/(.+?) b/(.+)$" line)]
                   b))))
       distinct
       vec))

(defn build!
  "Write the true diff for this push to `<context-dir>/<sha>.diff` and return
   the paths and metadata the prompt will reference.

   opts may override :merge-base-fn and :diff-fn for testing; both default to
   the real git calls in pr-review.gh."
  [repo-root {:keys [sha base-ref]} opts]
  (let [merge-base-fn (or (:merge-base-fn opts)
                          #(gh/merge-base repo-root base-ref opts))
        diff-fn       (or (:diff-fn opts)
                          #(gh/diff repo-root %1 %2 opts))
        base          (or (merge-base-fn) (str "origin/" base-ref))
        diff-text     (or (diff-fn base sha) "")
        dir           (context-dir repo-root)
        diff-path     (str dir "/" sha ".diff")]
    (fs/create-dirs dir)
    (spit diff-path diff-text)
    {:diff-path     diff-path
     :changed-files (changed-files diff-text)
     :base          base
     :sha           sha
     :diff-bytes    (fs/size diff-path)}))

(defn prune!
  "Delete all but the `keep` newest .diff files. Returns how many were removed."
  [repo-root keep]
  (let [files (->> (fs/glob (context-dir repo-root) "*.diff")
                   (sort-by #(fs/last-modified-time %))
                   reverse
                   (drop keep))]
    (doseq [f files] (fs/delete-if-exists f))
    (count files)))
```

- [ ] **Step 4: Run the tests to verify they pass**

```bash
cd ~/.nixpkgs/claude-code-plugins && bb test
```
Expected: PASS, `Ran 31 tests`.

- [ ] **Step 5: Commit**

```bash
cd ~/.nixpkgs/claude-code-plugins
git add hooks/pr_review/context.clj test/pr_review/context_test.clj
git commit -qm "feat: precompute untruncated diff context for the reviewer"
```

---

### Task 6: Prompt assembly

**Files:**
- Create: `~/.nixpkgs/claude-code-plugins/hooks/pr_review/prompt.clj`
- Test: `~/.nixpkgs/claude-code-plugins/test/pr_review/prompt_test.clj`

**Interfaces:**
- Consumes: nothing. Pass number and already-twice-raised fingerprints arrive as arguments — Task 8's trigger reads them from the ledger and passes them in, so this namespace stays pure apart from consuming the hint file.
- Produces:
  - `(overlay-path repo-root)` → String, `<repo-root>/.claude/pr-review.md`
  - `(hint-path repo-root)` → String, `<repo-root>/.git/pr-review-hint`
  - `(read-hint! repo-root)` → String or nil, and deletes the file
  - `(build {:core String :repo-root String :ctx map :pr long :pass long :draft? boolean :prior-fingerprints [String]})` → String

- [ ] **Step 1: Write the failing test**

`test/pr_review/prompt_test.clj`:

```clojure
(ns pr-review.prompt-test
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [pr-review.prompt :as prompt]))

(defn- tmp-repo []
  (let [d (str (fs/create-temp-dir {:prefix "pr-review-prompt"}))]
    (fs/create-dirs (str d "/.git"))
    d))

(def ^:private ctx
  {:diff-path "/r/.git/pr-review-context/abc.diff"
   :changed-files ["src/a.clj" "test/b_test.clj"]
   :base "basesha" :sha "abc" :diff-bytes 1234})

(defn- base-args [repo]
  {:core "CORE_TEXT" :repo-root repo :ctx ctx :pr 370 :pass 1
   :draft? false :prior-fingerprints []})

(deftest core-is-always-included
  (is (str/includes? (prompt/build (base-args (tmp-repo))) "CORE_TEXT")))

(deftest prompt-names-the-diff-file-and-repo-root
  (let [r (tmp-repo)
        out (prompt/build (base-args r))]
    (is (str/includes? out (:diff-path ctx)))
    (is (str/includes? out r))
    (is (str/includes? out "src/a.clj"))))

(deftest first-pass-is-labelled-FIRST
  (let [out (prompt/build (base-args (tmp-repo)))]
    (is (str/includes? out "PASS: FIRST"))
    (is (not (str/includes? out "RE-REVIEW")))))

(deftest later-passes-are-labelled-RE-REVIEW
  (let [out (prompt/build (assoc (base-args (tmp-repo)) :pass 3))]
    (is (str/includes? out "PASS: RE-REVIEW"))
    (is (str/includes? out "pass 3")
        "the reviewer must know which pass it is to apply the severity asymmetry")))

(deftest overlay-is-included-when-present
  (let [r (tmp-repo)]
    (fs/create-dirs (str r "/.claude"))
    (spit (prompt/overlay-path r) "OVERLAY_TEXT")
    (is (str/includes? (prompt/build (base-args r)) "OVERLAY_TEXT"))))

(deftest missing-overlay-degrades-silently
  (let [out (prompt/build (base-args (tmp-repo)))]
    (is (not (str/includes? out "OVERLAY")))
    (is (str/includes? out "CORE_TEXT")
        "a repo with no .claude/pr-review.md must still get a working review")))

(deftest hint-is-included-and-consumed
  (let [r (tmp-repo)]
    (spit (prompt/hint-path r) "watch the retry path")
    (let [out (prompt/build (base-args r))]
      (is (str/includes? out "watch the retry path")))
    (is (not (fs/exists? (prompt/hint-path r)))
        "a hint is for one review; leaving it would silently apply to every later pass")))

(deftest draft-status-is-stated
  (is (str/includes? (prompt/build (assoc (base-args (tmp-repo)) :draft? true))
                     "draft")))

(deftest twice-raised-fingerprints-are-listed-as-do-not-re-raise
  (let [out (prompt/build (assoc (base-args (tmp-repo)) :pass 2
                                 :prior-fingerprints ["src/a.clj:12:correctness/followup"]))]
    (is (str/includes? out "src/a.clj:12:correctness/followup"))
    (is (str/includes? out "do not re-raise"))))

(deftest empty-diff-is-called-out
  (let [out (prompt/build (assoc-in (base-args (tmp-repo)) [:ctx :diff-bytes] 0))]
    (is (str/includes? out "empty")
        "an empty diff must be stated, or the reviewer invents findings")))
```

- [ ] **Step 2: Run the tests to verify they fail**

```bash
cd ~/.nixpkgs/claude-code-plugins && bb test
```
Expected: FAIL with `java.io.FileNotFoundException` naming `pr_review/prompt`, exit 1.

- [ ] **Step 3: Write the implementation**

`hooks/pr_review/prompt.clj`:

```clojure
(ns pr-review.prompt
  "Assemble the reviewer prompt: generic core, per-repo overlay, one-shot hint,
   and the pass state that drives the FIRST/RE-REVIEW severity asymmetry."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]))

(defn overlay-path
  "Per-repo enumeration recipe. Optional — the core alone is functional."
  [repo-root]
  (str repo-root "/.claude/pr-review.md"))

(defn hint-path
  "One-shot note from agent A to the reviewer."
  [repo-root]
  (str repo-root "/.git/pr-review-hint"))

(defn read-hint!
  "Read and delete the hint. A hint is scoped to one review; leaving it in
   place would silently steer every later pass on the PR."
  [repo-root]
  (let [p (hint-path repo-root)]
    (when (fs/exists? p)
      (let [s (str/trim (slurp p))]
        (fs/delete-if-exists p)
        (when-not (str/blank? s) s)))))

(defn- section
  [title body]
  (when-not (str/blank? (str body))
    (str "\n## " title "\n\n" body "\n")))

(defn build
  [{:keys [core repo-root ctx pr pass draft? prior-fingerprints]}]
  (let [overlay (when (fs/exists? (overlay-path repo-root))
                  (slurp (overlay-path repo-root)))
        hint    (read-hint! repo-root)
        first?  (= 1 pass)]
    (str
     core
     (section "This review"
              (str/join "\n"
                        (cond-> [(str "REPO ROOT: " repo-root)
                                 (str "PR NUMBER: " pr)
                                 (str "PASS: " (if first? "FIRST" "RE-REVIEW")
                                      " (pass " pass ")")
                                 (str "BASE: " (:base ctx))
                                 (str "HEAD: " (:sha ctx))]
                          draft? (conj "PR STATE: draft — review it as you would any other")
                          true   (conj (if (zero? (:diff-bytes ctx))
                                         "DIFF: empty — report that and stop; do not invent findings"
                                         (str "DIFF: " (:diff-bytes ctx) " bytes"))))))
     (section "How to read the change"
              (str "The complete, untruncated diff is on disk. Read it first:\n\n"
                   "    " (:diff-path ctx) "\n\n"
                   "Then read the surrounding source under the repo root for context.\n"
                   "You have Read, Grep and Glob. You have no shell and no test runner —\n"
                   "this review is a reading, not a run. Do not enumerate the tools you\n"
                   "lack; a list of absent capabilities is not a finding."))
     (section "Changed files"
              (if (seq (:changed-files ctx))
                (str/join "\n" (map #(str "- " %) (:changed-files ctx)))
                "(none)"))
     (when (seq prior-fingerprints)
       (section "Already reported twice — do not re-raise"
                (str "These findings have been raised on two previous passes. They stay\n"
                     "on the follow-up list; reporting them again gates verified fixes\n"
                     "from shipping.\n\n"
                     (str/join "\n" (map #(str "- " %) prior-fingerprints)))))
     (when overlay (section "Repository-specific review notes" overlay))
     (when hint (section "Note from the author for this review" hint)))))
```

- [ ] **Step 4: Run the tests to verify they pass**

```bash
cd ~/.nixpkgs/claude-code-plugins && bb test
```
Expected: PASS, `Ran 41 tests`.

- [ ] **Step 5: Commit**

```bash
cd ~/.nixpkgs/claude-code-plugins
git add hooks/pr_review/prompt.clj test/pr_review/prompt_test.clj
git commit -qm "feat: reviewer prompt assembly with per-repo overlay and one-shot hint"
```

---

### Task 7: Reviewer spawn and output parsing

**Files:**
- Create: `~/.nixpkgs/claude-code-plugins/hooks/pr_review/reviewer.clj`
- Test: `~/.nixpkgs/claude-code-plugins/test/pr_review/reviewer_test.clj`

**Interfaces:**
- Consumes: nothing
- Produces:
  - `(claude-argv)` → vector of String, the exact `claude -p` command line
  - `(run! prompt repo-root opts)` → `{:exit long :out String :err String}`; `opts` honours `:spawn-fn`
  - `(parse-output out)` → `{:verdict String :counts {String long} :fingerprints [String] :body String}`
  - `(mergeable? parsed)` → boolean

- [ ] **Step 1: Write the failing test**

`test/pr_review/reviewer_test.clj`:

```clojure
(ns pr-review.reviewer-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [pr-review.reviewer :as reviewer]))

(def ^:private good-output
  (str "VERDICT: NOT MERGEABLE — retry loop drops the last attempt\n"
       "\n"
       "  [correctness/blocking]  1 findings\n"
       "  [correctness/followup]  2 findings\n"
       "  [coverage]              none\n"
       "  [docs-accuracy]         none\n"
       "  [style]                 1 findings\n"
       "\n"
       "1. [correctness/blocking] src/retry.clj:42 — off-by-one drops attempt N\n"
       "2. [correctness/followup] src/retry.clj:88 — jitter unseeded\n"
       "3. [correctness/followup] src/pool.clj:12 — leak on 5xx\n"
       "4. [style] src/pool.clj:3 — naming\n"))

(deftest argv-pins-the-reviewer-to-read-only-tools
  (let [argv (reviewer/claude-argv)]
    (is (= "claude" (first argv)))
    (is (some #{"-p"} argv))
    (testing "no shell, no writes — this is what makes B provably non-mutating
              and immune to rtk's diff truncation"
      (let [i (.indexOf argv "--allowedTools")]
        (is (nat-int? i))
        (is (= "Read,Grep,Glob" (nth argv (inc i))))))
    (is (some #{"opus"} argv) "review quality is the point; do not downgrade the model")))

(deftest run-passes-the-prompt-and-cwd-to-the-spawner
  (let [seen (atom nil)
        spawn (fn [argv prompt dir] (reset! seen {:argv argv :prompt prompt :dir dir})
                {:exit 0 :out "VERDICT: MERGEABLE — 0 follow-ups to file" :err ""})
        res (reviewer/run! "PROMPT" "/repo" {:spawn-fn spawn})]
    (is (= 0 (:exit res)))
    (is (= "PROMPT" (:prompt @seen)))
    (is (= "/repo" (:dir @seen)) "the reviewer must run in the repo it is reviewing")))

(deftest parse-extracts-verdict-and-counts
  (let [p (reviewer/parse-output good-output)]
    (is (= "NOT MERGEABLE" (:verdict p)))
    (is (= 1 (get (:counts p) "correctness/blocking")))
    (is (= 2 (get (:counts p) "correctness/followup")))
    (is (= 0 (get (:counts p) "coverage")) "\"none\" must parse as 0, not nil")
    (is (= 1 (get (:counts p) "style")))))

(deftest parse-extracts-fingerprints-from-findings
  (let [p (reviewer/parse-output good-output)]
    (is (= ["src/retry.clj:42:correctness/blocking"
            "src/retry.clj:88:correctness/followup"
            "src/pool.clj:12:correctness/followup"
            "src/pool.clj:3:style"]
           (:fingerprints p))
        "fingerprints are file:line:category so the one-re-raise rule can key on them")))

(deftest mergeable-requires-no-blocking-and-no-coverage
  (is (true? (reviewer/mergeable?
              {:verdict "MERGEABLE" :counts {"correctness/blocking" 0 "coverage" 0}})))
  (is (false? (reviewer/mergeable?
               {:verdict "MERGEABLE" :counts {"correctness/blocking" 1 "coverage" 0}}))
      "the verdict line is the reviewer's claim; the counts are the evidence")
  (is (false? (reviewer/mergeable?
               {:verdict "MERGEABLE" :counts {"correctness/blocking" 0 "coverage" 2}}))))

(deftest output-with-no-verdict-is-MALFORMED-not-dropped
  (let [p (reviewer/parse-output "I could not find the diff file.")]
    (is (= "MALFORMED" (:verdict p)))
    (is (= "I could not find the diff file." (:body p))
        "a reviewer that fails must surface its own words, or the pass vanishes silently")
    (is (= [] (:fingerprints p)))))

(deftest body-is-preserved-verbatim
  (is (= good-output (:body (reviewer/parse-output good-output)))))
```

- [ ] **Step 2: Run the tests to verify they fail**

```bash
cd ~/.nixpkgs/claude-code-plugins && bb test
```
Expected: FAIL with `java.io.FileNotFoundException` naming `pr_review/reviewer`, exit 1.

- [ ] **Step 3: Write the implementation**

`hooks/pr_review/reviewer.clj`:

```clojure
(ns pr-review.reviewer
  "Spawn the independent reviewer and parse what it says.

   The reviewer is a separate `claude -p` process: fresh context window, none
   of agent A's conversation history, the same machine and working tree. Its
   tool list is Read,Grep,Glob — no Bash, so nothing it runs can be rewritten
   by rtk and nothing it does can touch the tree."
  (:require [babashka.process :as p]
            [clojure.string :as str]))

(def categories
  ["correctness/blocking" "correctness/followup" "coverage"
   "docs-accuracy" "style"])

(defn claude-argv
  []
  ["claude" "-p"
   "--model" "opus"
   "--allowedTools" "Read,Grep,Glob"])

(defn- default-spawn
  [argv prompt dir]
  (let [{:keys [exit out err]} (p/sh argv {:dir dir :in prompt})]
    {:exit exit :out (or out "") :err (or err "")}))

(defn run!
  "Run the reviewer with `prompt` on stdin, in `repo-root`.
   Never throws: a spawn failure becomes a non-zero exit with the message in
   :err, so the caller can still record a pass and tell the author."
  [prompt repo-root opts]
  (let [spawn (or (:spawn-fn opts) default-spawn)]
    (try (spawn (claude-argv) prompt repo-root)
         (catch Exception e {:exit 127 :out "" :err (str (ex-message e))}))))

(defn- parse-verdict
  [out]
  (when-let [[_ v] (re-find #"(?m)^\s*VERDICT:\s*(MERGEABLE|NOT MERGEABLE)" out)]
    v))

(defn- parse-counts
  "Read the per-category count block. \"none\" means 0 — a missing key and a
   zero count must not be confusable, or a clean pass reads as an unparsed one."
  [out]
  (into {}
        (map (fn [cat]
               (let [re (re-pattern (str "(?m)^\\s*\\[" cat "\\]\\s+(none|\\d+)"))
                     [_ n] (re-find re out)]
                 [cat (cond (nil? n) 0
                            (= "none" n) 0
                            :else (parse-long n))])))
        categories))

(defn- parse-fingerprints
  "Stable identity for a finding: file:line:category, taken from numbered
   finding lines of the form `N. [category] path:line — text`."
  [out]
  (->> (str/split-lines out)
       (keep (fn [line]
               (when-let [[_ cat path ln]
                          (re-find #"^\s*\d+\.\s*\[([a-z/-]+)\]\s+([^\s:]+):(\d+)" line)]
                 (str path ":" ln ":" cat))))
       distinct
       vec))

(defn parse-output
  [out]
  (let [out (or out "")]
    (if-let [v (parse-verdict out)]
      {:verdict v
       :counts (parse-counts out)
       :fingerprints (parse-fingerprints out)
       :body out}
      {:verdict "MALFORMED"
       :counts (zipmap categories (repeat 0))
       :fingerprints []
       :body (str/trim out)})))

(defn mergeable?
  "MERGEABLE means exactly: no correctness/blocking finding, and no coverage
   finding. The verdict line is the reviewer's claim; the counts are the
   evidence, and the evidence wins."
  [{:keys [verdict counts]}]
  (and (= "MERGEABLE" verdict)
       (zero? (get counts "correctness/blocking" 0))
       (zero? (get counts "coverage" 0))))
```

- [ ] **Step 4: Run the tests to verify they pass**

```bash
cd ~/.nixpkgs/claude-code-plugins && bb test
```
Expected: PASS, `Ran 48 tests`.

- [ ] **Step 5: Commit**

```bash
cd ~/.nixpkgs/claude-code-plugins
git add hooks/pr_review/reviewer.clj test/pr_review/reviewer_test.clj
git commit -qm "feat: read-only reviewer spawn and output parsing"
```

---

### Task 8: Trigger wiring and hooks.json

**Files:**
- Create: `~/.nixpkgs/claude-code-plugins/hooks/pr_review/trigger.clj`
- Create: `~/.nixpkgs/claude-code-plugins/hooks/hooks.json`
- Test: `~/.nixpkgs/claude-code-plugins/test/pr_review/trigger_test.clj`

**Interfaces:**
- Consumes: everything from Tasks 2–7
- Produces:
  - `(decide input opts)` → `{:action :silent|:review|:cap-reached :reason String …}` — pure decision, no side effects
  - `(-main & args)` → reads hook JSON on stdin, exits 0 or 2

- [ ] **Step 1: Write the failing test**

`test/pr_review/trigger_test.clj`:

```clojure
(ns pr-review.trigger-test
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [pr-review.ledger :as ledger]
            [pr-review.trigger :as trigger]))

(defn- tmp-repo []
  (let [d (str (fs/create-temp-dir {:prefix "pr-review-trigger"}))]
    (fs/create-dirs (str d "/.git"))
    d))

(defn- opts
  "Wire every collaborator to a stub so `decide` is exercised in isolation."
  [repo & {:keys [branch pr]}]
  {:repo-root-fn (constantly repo)
   :branch-fn    (constantly (or branch "feat/x"))
   :head-sha-fn  (constantly "headsha")
   :open-pr-fn   (constantly pr)})

(deftest no-repo-is-silent
  (let [d (trigger/decide {:cwd "/tmp"}
                          (assoc (opts nil) :repo-root-fn (constantly nil)))]
    (is (= :silent (:action d)))
    (is (str/includes? (:reason d) "not a git repo"))))

(deftest detached-head-is-silent
  (let [r (tmp-repo)
        d (trigger/decide {:cwd r} (opts r :branch nil))]
    (is (= :silent (:action d)))))

(deftest no-open-pr-is-silent
  (let [r (tmp-repo)
        d (trigger/decide {:cwd r} (opts r :pr nil))]
    (is (= :silent (:action d)))
    (is (str/includes? (:reason d) "no open PR")
        "a push to a branch with no PR is the human's normal workflow, not an error")))

(deftest open-pr-yields-a-review-with-pass-one
  (let [r (tmp-repo)
        d (trigger/decide {:cwd r}
                          (opts r :pr {:number 370 :isDraft false :baseRefName "main"}))]
    (is (= :review (:action d)))
    (is (= 370 (:pr d)))
    (is (= 1 (:pass d)))
    (is (= "headsha" (:sha d)))
    (is (= "main" (:base-ref d)))
    (is (false? (:draft? d)))))

(deftest drafts-are-reviewed
  (let [r (tmp-repo)
        d (trigger/decide {:cwd r}
                          (opts r :pr {:number 9 :isDraft true :baseRefName "main"}))]
    (is (= :review (:action d)))
    (is (true? (:draft? d)))))

(deftest pass-number-comes-from-the-ledger
  (let [r (tmp-repo)]
    (doseq [n [1 2]]
      (ledger/append-pass! r {:pr 370 :sha (str n) :pass n :verdict "NOT_MERGEABLE"
                              :blocking 1 :followup 0 :coverage 0 :fingerprints []}))
    (is (= 3 (:pass (trigger/decide
                     {:cwd r}
                     (opts r :pr {:number 370 :isDraft false :baseRefName "main"})))))))

(deftest cap-stops-the-loop
  (let [r (tmp-repo)]
    (doseq [n (range 1 (inc ledger/max-passes))]
      (ledger/append-pass! r {:pr 370 :sha (str n) :pass n :verdict "NOT_MERGEABLE"
                              :blocking 1 :followup 0 :coverage 0 :fingerprints []}))
    (let [d (trigger/decide {:cwd r}
                            (opts r :pr {:number 370 :isDraft false :baseRefName "main"}))]
      (is (= :cap-reached (:action d)))
      (is (str/includes? (:reason d) "10")))))

(deftest twice-raised-fingerprints-are-carried-into-the-decision
  (let [r (tmp-repo)
        fp "src/a.clj:1:correctness/followup"]
    (doseq [n [1 2]]
      (ledger/append-pass! r {:pr 370 :sha (str n) :pass n :verdict "MERGEABLE"
                              :blocking 0 :followup 1 :coverage 0 :fingerprints [fp]}))
    (is (= [fp] (:prior-fingerprints
                 (trigger/decide {:cwd r}
                                 (opts r :pr {:number 370 :isDraft false
                                              :baseRefName "main"})))))))

(deftest findings-message-is-self-describing
  (testing "the harness wrapper text is fixed and useless, so the first line
            must identify repo, PR and pass on its own"
    (let [msg (trigger/findings-message
               {:repo-root "/r" :pr 370 :pass 2}
               {:verdict "NOT MERGEABLE" :body "BODY"
                :counts {"correctness/blocking" 1}})]
      (is (str/starts-with? msg "pr-review-loop"))
      (is (str/includes? msg "PR #370"))
      (is (str/includes? msg "pass 2"))
      (is (str/includes? msg "BODY")))))
```

- [ ] **Step 2: Run the tests to verify they fail**

```bash
cd ~/.nixpkgs/claude-code-plugins && bb test
```
Expected: FAIL with `java.io.FileNotFoundException` naming `pr_review/trigger`, exit 1.

- [ ] **Step 3: Write the implementation**

`hooks/pr_review/trigger.clj`:

```clojure
(ns pr-review.trigger
  "PostToolUse hook entrypoint.

   Exit 0 means silence. Exit 2 wakes agent A with whatever this process wrote
   to stderr — and only stderr: the harness discards a hook's stdout even when
   stderr is empty. Never exit anything else; other codes produce a
   `Failed with non-blocking status code:` notice and the pass is lost."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [pr-review.context :as context]
            [pr-review.gh :as gh]
            [pr-review.ledger :as ledger]
            [pr-review.lock :as lock]
            [pr-review.prompt :as prompt]
            [pr-review.reviewer :as reviewer]))

(def ^:private context-keep 5)

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

(defn decide
  "Pure decision from the hook input. No side effects, no spawning."
  [{:keys [cwd]} opts]
  (let [repo-root ((or (:repo-root-fn opts) #(gh/repo-root cwd opts)))]
    (if-not repo-root
      {:action :silent :reason "not a git repo"}
      (let [branch ((or (:branch-fn opts) #(gh/current-branch repo-root opts)))]
        (if-not branch
          {:action :silent :reason "detached HEAD, no branch to match a PR"}
          (let [pr ((or (:open-pr-fn opts) #(gh/open-pr repo-root branch opts)))]
            (if-not pr
              {:action :silent :reason (str "no open PR for branch " branch)}
              (let [pr-num (:number pr)
                    sha ((or (:head-sha-fn opts) #(gh/head-sha repo-root opts)))]
                (if (ledger/cap-reached? repo-root pr-num)
                  {:action :cap-reached
                   :repo-root repo-root :pr pr-num
                   :reason (str "review cap of " ledger/max-passes
                                " passes reached for PR #" pr-num)}
                  {:action :review
                   :repo-root repo-root
                   :pr pr-num
                   :pass (ledger/next-pass-number repo-root pr-num)
                   :sha sha
                   :base-ref (:baseRefName pr)
                   :draft? (boolean (:isDraft pr))
                   :prior-fingerprints
                   (->> (ledger/read-passes repo-root pr-num)
                        (mapcat :fingerprints)
                        distinct
                        (filterv #(>= (ledger/raise-count repo-root pr-num %) 2)))})))))))))

(defn findings-message
  "The text agent A will see. The harness prefixes it with a fixed, unhelpful
   wrapper and ignores rewakeMessage for third-party plugins, so this string
   has to introduce itself."
  [{:keys [repo-root pr pass]} parsed]
  (str "pr-review-loop — " (fs/file-name repo-root)
       " PR #" pr ", pass " pass ": " (:verdict parsed) "\n\n"
       (:body parsed)
       "\n\nNext: use the pr-review-loop skill. Verify each"
       " [correctness/blocking] finding against the source before fixing it."))

(defn- review!
  [{:keys [repo-root pr pass sha base-ref draft? prior-fingerprints] :as d} opts]
  (case (:status (lock/acquire! repo-root {:pr pr :sha sha} opts))
    :duplicate {:exit 0 :message nil}
    (try
      (let [ctx    (context/build! repo-root {:pr pr :sha sha :base-ref base-ref} opts)
            core   (core-prompt)
            text   (prompt/build {:core core :repo-root repo-root :ctx ctx
                                  :pr pr :pass pass :draft? draft?
                                  :prior-fingerprints prior-fingerprints})
            res    (reviewer/run! text repo-root opts)
            parsed (reviewer/parse-output (:out res))]
        (ledger/append-pass!
         repo-root
         {:pr pr :sha sha :pass pass
          :verdict (:verdict parsed)
          :blocking (get (:counts parsed) "correctness/blocking" 0)
          :followup (get (:counts parsed) "correctness/followup" 0)
          :coverage (get (:counts parsed) "coverage" 0)
          :fingerprints (:fingerprints parsed)})
        (context/prune! repo-root context-keep)
        {:exit 2 :message (findings-message d parsed)})
      ;; Same `opts` acquire! was called with, not just repo-root: release!
      ;; now only deletes the record if its :pid still matches, and a test
      ;; that stubs :pid in opts to acquire! must have that same stub honoured
      ;; on release! or the two would disagree about who holds the lock.
      (finally (lock/release! repo-root opts)))))

(defn -main
  [& _]
  (let [input (try (json/parse-string (slurp *in*) true)
                   (catch Exception _ nil))
        d     (decide (or input {}) {})]
    (let [{:keys [exit message]}
          (case (:action d)
            :silent      {:exit 0 :message nil}
            :cap-reached {:exit 2 :message (str "pr-review-loop — " (:reason d)
                                                ". No further reviews will run"
                                                " on this PR. Decide manually.")}
            :review      (review! d {})
            {:exit 0 :message nil})]
      (when message
        (binding [*out* *err*] (println message) (flush)))
      (System/exit exit))))
```

- [ ] **Step 4: Run the tests to verify they pass**

```bash
cd ~/.nixpkgs/claude-code-plugins && bb test
```
Expected: PASS, `Ran 57 tests`.

- [ ] **Step 5: Write `hooks/hooks.json`**

Four `if` rules because rtk rewrites `git push` → `rtk git push` and `gh pr create` → `rtk gh pr create`, but passes the command through **unrewritten** when a Claude Code deny rule matches, or when `rtk`/`jq` is missing. They never double-fire: `Bash(git push:*)` does not match `rtk git push`.

```json
{
  "description": "pr-review-loop: on agent-A push or PR creation, review in the background and wake the session with findings.",
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "Bash",
        "hooks": [
          {
            "type": "command",
            "if": "Bash(rtk git push:*)",
            "command": "bb",
            "args": ["--config", "${CLAUDE_PLUGIN_ROOT}/bb.edn", "review-trigger"],
            "asyncRewake": true,
            "statusMessage": "Reviewing the pushed commits…"
          },
          {
            "type": "command",
            "if": "Bash(git push:*)",
            "command": "bb",
            "args": ["--config", "${CLAUDE_PLUGIN_ROOT}/bb.edn", "review-trigger"],
            "asyncRewake": true,
            "statusMessage": "Reviewing the pushed commits…"
          },
          {
            "type": "command",
            "if": "Bash(rtk gh pr create:*)",
            "command": "bb",
            "args": ["--config", "${CLAUDE_PLUGIN_ROOT}/bb.edn", "review-trigger"],
            "asyncRewake": true,
            "statusMessage": "Reviewing the new PR…"
          },
          {
            "type": "command",
            "if": "Bash(gh pr create:*)",
            "command": "bb",
            "args": ["--config", "${CLAUDE_PLUGIN_ROOT}/bb.edn", "review-trigger"],
            "asyncRewake": true,
            "statusMessage": "Reviewing the new PR…"
          }
        ]
      }
    ]
  }
}
```

No `timeout` key: it is unenforced for async hooks, and the sync default of 600 s sits above the worst observed review of 343 s.

- [ ] **Step 6: Verify the trigger is silent outside a PR branch**

```bash
cd /tmp && mkdir -p pr-review-smoke && cd pr-review-smoke && git init -q
echo '{"cwd":"/tmp/pr-review-smoke","tool_name":"Bash","tool_input":{"command":"rtk git push"}}' \
  | bb --config ~/.nixpkgs/claude-code-plugins/bb.edn review-trigger
echo "exit=$?"
```
Expected: no output, `exit=0`. A repo with no remote and no PR must be silent.

- [ ] **Step 6b: Verify a human push with no Claude session triggers nothing (R2)**

In a repo that **does** have an open PR, with no `claude` session running
anywhere against it:

```bash
cd <repo-with-open-pr>
wc -l .git/pr-review-ledger.jsonl 2>/dev/null || echo "0 (no ledger yet)"
git commit --allow-empty -qm "chore: human push, should not be reviewed"
git push
sleep 20
wc -l .git/pr-review-ledger.jsonl 2>/dev/null || echo "0 (no ledger yet)"
```
Expected: the line count is unchanged. Hooks live inside the Claude Code
process, so a terminal push has nothing to fire. This is R2 satisfied by
construction rather than by a check — the step exists to prove the construction
holds, and it is the one requirement with no code behind it.

- [ ] **Step 7: Commit**

```bash
cd ~/.nixpkgs/claude-code-plugins
git add . && git commit -qm "feat: trigger wiring and hook registration"
```

The live-PR end-to-end run is **not** part of this task. `trigger/review!`
slurps `${CLAUDE_PLUGIN_ROOT}/hooks/review_core.md`, which Task 9 creates — an
end-to-end attempt here would fail for a reason that has nothing to do with this
task's deliverable. It is Task 9 Step 3.

---

### Task 9: Generic review core prompt

**Files:**
- Create: `~/.nixpkgs/claude-code-plugins/hooks/review_core.md`

**Interfaces:**
- Consumes: nothing
- Produces: the file `pr-review.trigger/review!` slurps from `${CLAUDE_PLUGIN_ROOT}/hooks/review_core.md`

This is prose, not code, so it has no unit test. Its done-condition is Task 8 Step 7 producing a parseable `VERDICT:` line and correctly categorised findings.

- [ ] **Step 1: Write the core prompt**

`hooks/review_core.md` — write exactly this. It is derived from
`claude-code-http-proxy/.github/workflows/claude-code-review.yml` with every
repo-specific sentence removed (those go to the overlay in Task 11) and with
the inline-comment routing replaced by a single parseable list, because this
path has no GitHub API.

````markdown
You are reviewing a pull request created by an automated coding agent.

## Categorise every finding

Tag each finding with exactly one category:

  [correctness/blocking]  wrong behaviour reachable in ordinary use — or ANY
                          defect, however narrow its window, that mixes one
                          user's or session's data into another's, leaks a
                          secret or token, or corrupts stored state
  [correctness/followup]  wrong behaviour that is real but bounded: latent
                          behind a condition you could not verify, confined to
                          degraded paths (upstream 5xx, dropped stream, disk
                          full), or requiring a timing window narrower than one
                          network round trip
  [coverage]              a test that cannot fail for the reason it claims to
                          check — worst when it certifies a safety property
  [docs-accuracy]         a comment, docstring or commit-message claim that is false
  [style]                 naming, formatting, wording

Severity is a claim about reachability, so it inherits the evidence rules
below: say whether the trigger is traced through the source or hypothesised.

## Output format — this is parsed, so match it exactly

Begin with a verdict line, then a count block, then a single numbered list of
every finding.

    VERDICT: MERGEABLE — N follow-ups to file
    VERDICT: NOT MERGEABLE — <shortest statement of the blocking finding>

      [correctness/blocking]  N findings
      [correctness/followup]  N findings
      [coverage]              N findings
      [docs-accuracy]         N findings
      [style]                 N findings

Write a bare integer, or the word `none`. Every one of the five lines must be
present even when it is `none` — a missing line is indistinguishable from an
unparsed review.

Then the findings, one per line, in this shape:

    1. [correctness/blocking] src/retry.clj:42 — off-by-one drops the last attempt
    2. [correctness/followup] src/pool.clj:118 — connection leaks when upstream 5xxs

`path:line` is mandatory and must be repo-relative. The path and line become
this finding's identity across passes; without them the loop cannot tell a
re-raise from a new defect.

MERGEABLE means exactly: no [correctness/blocking] finding, and no [coverage]
finding in which a test certifies a safety property it does not check. Followup,
docs and style findings do not flip the verdict — that separation is the point of
the split. State it plainly either way, so a genuinely clean pass is
distinguishable from a quiet one.

## This review is a reading, not a run

You have `Read`, `Grep` and `Glob`. The complete, untruncated diff is on disk at
the path given below — read it first, then read the surrounding source for
context. You have no shell, no test runner and no REPL.

Do not try to run tests and do not try to read CI. Say once, in your output,
that the findings are a static reading — then stop. Do not enumerate the
commands you did not run or the capabilities you lack: a list of absent tools is
not a finding, and it displaces the review.

## Every finding carries its evidence

Cite the file:line that shows the behaviour before reporting a defect. A claim
inferred from a name, or from what a function looks like it ought to do, costs
the author a whole round trip to disprove — verify it against the source or drop
it.

Separate what you TRACED through the source from what you INFERRED from a name
or a shape, and say which is which. A hypothesis is welcome when it is labelled
as one; an inference presented as a trace costs the author a round trip.

Reachability is part of the finding, not a footnote — it decides the category. A
defect behind a flag that is off, a profile that is not selected, or a module
that is not enabled is still worth reporting as followup, with the trigger
condition in the same line.

## Sweep for siblings in the same pass

When you find a defect, sweep for its siblings in the SAME pass: the other call
sites of that function, the branch that mirrors it, the write path that
parallels the read path, the bound on the other side of the one that is guarded.
Report them together, as one finding at the severity of the worst sibling.

This is the highest-value instruction in this prompt. Root causes reported one
sibling per pass are, from the author's side, indistinguishable from an
unbounded queue.

Enumerating callers usually needs more than a naive grep. If a
"Repository-specific review notes" section appears below, it tells you how
callers are actually reached in this codebase — read it before claiming you have
enumerated them.

## Pass discipline

The "This review" section below tells you whether this is a FIRST pass or a
RE-REVIEW.

On a FIRST pass: when genuinely torn between blocking and followup, choose
**blocking**. Nothing has shipped yet.

On a RE-REVIEW, do these in order:

1. Verify closure: for each blocking finding from the previous pass, confirm the
   fix at its file:line, or say why it does not close it.
2. Sweep the new commits — fixes written to satisfy review comments have had
   less design thought than the original diff, so review them hard — but report
   at the split severities above. A regression in a fix is blocking only if it
   meets the blocking bar on its own terms.
3. Everything else you notice goes to the followup list.
4. End with the verdict.

On a RE-REVIEW, when genuinely torn, choose **followup**. That asymmetry is
deliberate. Holding the first-pass threshold constant across re-reviews means
every fix commit yields the next pass's findings, indefinitely: on a sibling
repo this setup produced twelve passes and roughly forty correctness findings on
ONE pull request without converging, about two thirds of them defects in fixes
written to satisfy the previous pass. The marginal latent finding on pass N is
paid for by delaying every already-fixed defect from shipping. If everything new
is followup-grade, the verdict is MERGEABLE and the loop is over.

Any finding listed under "Already reported twice — do not re-raise" stays on the
followup list. Do not report it again.
````

- [ ] **Step 2: Verify the format round-trips through the parser**

```bash
cd ~/.nixpkgs/claude-code-plugins
bb -e '(require (quote [pr-review.reviewer :as r]))
        (let [p (r/parse-output (slurp "/dev/stdin"))]
          (println "verdict:" (:verdict p))
          (println "counts:" (:counts p))
          (println "fingerprints:" (:fingerprints p)))' <<'EOF'
VERDICT: NOT MERGEABLE — retry loop drops the last attempt

  [correctness/blocking]  1 findings
  [correctness/followup]  none
  [coverage]              none
  [docs-accuracy]         none
  [style]                 none

1. [correctness/blocking] src/retry.clj:42 — off-by-one drops attempt N
EOF
```
Expected: `verdict: NOT MERGEABLE`, blocking 1, fingerprint `src/retry.clj:42:correctness/blocking`. If the example you wrote into `review_core.md` does not parse, the prompt and the parser disagree — fix the prompt, not the parser.

- [ ] **Step 3: End-to-end against a real PR**

Moved here from Task 8: `trigger/review!` needs `review_core.md`, so this is the
first point at which a full run is possible.

```bash
cd ~/.nixpkgs/claude-code-plugins
git add hooks/review_core.md && git commit -qm "feat: generic review core prompt"
sed -i '' 's/"0\.1\.0"/"0.2.0"/' .claude-plugin/plugin.json hooks/pr_review/version.clj
bb test
git add . && git commit -qm "chore: bump to 0.2.0"
claude plugin uninstall pr-review-loop@nixpkgs-plugins
claude plugin install pr-review-loop@nixpkgs-plugins
```

Then, in a **new** interactive `claude` session (plugin hooks do not hot-load)
inside a repo with an open PR: make a trivial commit, push, and let the turn end.

Expected: the turn completes immediately; 2–6 minutes later the session wakes on
its own with `pr-review-loop — <repo> PR #N, pass 1: …`;
`.git/pr-review-ledger.jsonl` has one line; and

```bash
diff <(cat .git/pr-review-context/<sha>.diff) <(git diff <base>...<sha>)
```

is empty — the reviewer read the real diff, not an `rtk`-truncated one. That
byte-for-byte check is the whole reason the diff is precomputed; a 195-byte
context file against an 80 KB diff means rtk got in the path.

- [ ] **Step 4: Commit**

```bash
cd ~/.nixpkgs/claude-code-plugins
git add . && git commit -qm "feat: end-to-end verified on a live PR"
```

---

### Task 10: Loop skill and manual command

**Files:**
- Create: `~/.nixpkgs/claude-code-plugins/skills/pr-review-loop/SKILL.md`
- Create: `~/.nixpkgs/claude-code-plugins/commands/review.md`

**Interfaces:**
- Consumes: `.git/pr-review-ledger.jsonl`, `.git/pr-review-hint`
- Produces: the behaviour agent A follows on waking

- [ ] **Step 1: Write the skill**

`skills/pr-review-loop/SKILL.md`:

```markdown
---
name: pr-review-loop
description: Use when a pr-review-loop finding arrives, or when the user asks to work the PR review loop. Drives verification of blocking findings, fixes, merge, and follow-up PRs.
---

# PR review loop

A background reviewer wakes you with findings. Your job is the loop, not the review.

## On waking with findings

1. **Verify before fixing.** Reproduce each `[correctness/blocking]` finding
   against the source — read the cited `file:line`. A finding you cannot
   reproduce gets a note in your reply, not a fix. Reviewers over- and
   under-grade; re-derive severity yourself.
2. **Fix the class, not the instances.** A finding listing N bad inputs is
   evidence of a class. Name the invariant, derive the full violating input
   class, whitelist the valid class. Then grep for the *shape* of the defect,
   not the symbol name.
3. **Push the fixes to the same PR.** The push re-triggers the reviewer;
   pass N+1 verifies closure. Never open a new PR for review fixes — a new PR
   resets the pass counter and the loop never converges.
4. Ignore `[docs-accuracy]` and `[style]` until the PR is otherwise mergeable.

## When the verdict is MERGEABLE

1. Check CI. If Buildkite or another gate is configured and red, fix that first.
2. Merge.
3. Post one summary comment on the PR before or immediately after merging:
   passes run, findings by category, what was fixed, what was deferred. Read
   the pass history from `.git/pr-review-ledger.jsonl`.
4. If there are `[correctness/followup]` findings, open a **new** PR for them
   and let the loop run there. Verify each one before fixing it, same as above.
5. If there are none, the job is done. Say so.

## When the cap is reached

Ten passes have run on this PR. Do not push again expecting another review.
Summarise what is unresolved and hand the decision to the user.

## Hinting the reviewer

To tell the reviewer something before it runs, write it to
`<repo>/.git/pr-review-hint` before pushing. It is included in the next
review's prompt and consumed — it applies to exactly one pass.

## What the reviewer cannot do

It has `Read`, `Grep`, `Glob` and the full diff on disk. No shell, no tests,
no REPL. If a finding depends on runtime behaviour it will say so, and
verifying that is your job.
```

- [ ] **Step 2: Write the manual command**

`commands/review.md`:

```markdown
---
description: Run the PR reviewer on the current branch now, without waiting for a push.
---

Run the pr-review-loop reviewer against the current branch immediately.

1. Confirm the branch has an open PR:
   `gh pr list --head "$(git rev-parse --abbrev-ref HEAD)" --state open --json number,isDraft`
   If there is none, stop and say so.

2. If the user gave a note for the reviewer in their request, write it to
   `.git/pr-review-hint` first.

3. Invoke the trigger directly, feeding it the same JSON shape the hook feeds it.
   Note the literal path: `$CLAUDE_PLUGIN_ROOT` is set only for hooks declared in
   a plugin's `hooks/hooks.json`, so it is empty here.

   ```bash
   echo "{\"cwd\":\"$PWD\",\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"git push\"}}" \
     | bb --config ~/.nixpkgs/claude-code-plugins/bb.edn review-trigger
   ```

   It writes findings to stderr and exits 2. Read them from the command output
   rather than waiting for a wake — this path is synchronous.

4. Then follow the pr-review-loop skill from step 1 of "On waking with findings".
```

- [ ] **Step 3: Verify both are registered**

```bash
cd ~/.nixpkgs/claude-code-plugins
sed -i '' 's/"0\.2\.0"/"0.3.0"/' .claude-plugin/plugin.json hooks/pr_review/version.clj
bb test
git add . && git commit -qm "feat: loop skill and manual review command"
claude plugin uninstall pr-review-loop@nixpkgs-plugins
claude plugin install pr-review-loop@nixpkgs-plugins
```
Then in a new session, `/pr-review-loop:review` should appear in the command list and the skill in the skills list.

- [ ] **Step 4: Commit**

```bash
cd ~/.nixpkgs/claude-code-plugins
git add . && git commit -qm "chore: bump to 0.3.0"
```

---

### Task 11: cchp overlay, README, rollout notes

**Files:**
- Create: `~/workspace/home/claude-code-http-proxy/.claude/pr-review.md`
- Create: `~/.nixpkgs/claude-code-plugins/README.md`
- Modify: `~/.nixpkgs/docs/superpowers/specs/2026-09-08-pr-review-loop.md` (status line)

**Interfaces:**
- Consumes: `pr-review.prompt/overlay-path`
- Produces: the first real overlay, proving the split works

- [ ] **Step 1: Write the cchp overlay**

`~/workspace/home/claude-code-http-proxy/.claude/pr-review.md` — the enumeration recipe that was deleted from the core in Task 9:

```markdown
# Review notes for claude-code-http-proxy

## Enumerating callers needs more than grep

This system is wired by `ig/init-key` defmethods — 168 of them — whose only
caller is a keyword in `resources/system.edn`. The definition and its use share
no symbol at all. **Search the keyword, not the function name.**

A `defn` reached through `requiring-resolve`, or a var passed as a value, will
also not turn up in a naive symbol grep. When a finding is "this call site was
missed", enumerate properly before claiming completeness.

## Where defects have clustered

Three quarters of past review findings here landed on a file an earlier finding
had already flagged. That is not a repo full of independent bugs — it is root
causes reported one sibling per pass. Sweep hard.

## Tests are gated elsewhere

`.buildkite/pipeline.yml` runs `make test-unit-ci` on all branches, and the
author reads that result. Do not report "tests were not run" as a finding.
```

- [ ] **Step 2: Verify the overlay reaches the prompt**

```bash
cd ~/workspace/home/claude-code-http-proxy
bb --config ~/.nixpkgs/claude-code-plugins/bb.edn \
   -e '(require (quote [pr-review.prompt :as p]))
        (println (clojure.string/includes?
                  (p/build {:core "CORE" :repo-root (System/getProperty "user.dir")
                            :ctx {:diff-path "/x" :changed-files [] :base "b" :sha "s" :diff-bytes 1}
                            :pr 1 :pass 1 :draft? false :prior-fingerprints []})
                  "Search the keyword, not the function name"))'
```
Expected: `true`.

- [ ] **Step 3: Write the plugin README**

`~/.nixpkgs/claude-code-plugins/README.md`:

```markdown
# claude-code-plugins

Claude Code plugins maintained in `~/.nixpkgs`. This directory doubles as a
local plugin marketplace. It is **not** a separate git repo — it is tracked
content of `~/.nixpkgs`, and a local-directory marketplace does not require a
repo of its own.

## Install

```bash
claude plugin marketplace add ~/.nixpkgs/claude-code-plugins
claude plugin install pr-review-loop@nixpkgs-plugins
```

Then start a **new** `claude` session — plugin hooks do not hot-load.

## Updating

`claude plugin update` is a no-op unless the version string changes. Bump
`version` in `.claude-plugin/plugin.json` **and** `plugin-version` in
`hooks/pr_review/version.clj` (a test enforces they agree), commit, then:

```bash
claude plugin uninstall pr-review-loop@nixpkgs-plugins
claude plugin install pr-review-loop@nixpkgs-plugins
```

Uninstall leaves `~/.claude/plugins/cache/nixpkgs-plugins` behind; remove it
by hand if you want it gone.

## Tests

```bash
cd ~/.nixpkgs/claude-code-plugins && bb test
```

## pr-review-loop

Reviews a PR in the background whenever this machine's Claude session pushes to
it, and wakes the session with the findings. See
`~/.nixpkgs/docs/superpowers/specs/2026-09-08-pr-review-loop.md`.

Per-repo state, all under `.git/` and safe to delete:

| Path | Purpose |
|---|---|
| `.git/pr-review-ledger.jsonl` | pass history, drives the 10-pass cap and the one-re-raise rule |
| `.git/pr-review.lock` | at most one live reviewer per clone |
| `.git/pr-review-context/<sha>.diff` | the untruncated diff the reviewer reads |
| `.git/pr-review-hint` | one-shot note to the next review; consumed on read |

Optional per-repo prompt overlay: `<repo>/.claude/pr-review.md`.

It only runs in **interactive** sessions. In `-p` mode Claude Code kills async
hooks at teardown, so nothing is reviewed and nothing warns you.
```

- [ ] **Step 4: Flip the spec status**

Change the spec's `**Status:**` line from `accepted, not yet implemented` to
`implemented 2026-09-08` and commit.

- [ ] **Step 5: Commit**

```bash
cd ~/workspace/home/claude-code-http-proxy
git add .claude/pr-review.md
git commit -qm "docs: pr-review-loop overlay for this repo"

cd ~/.nixpkgs
git add claude-code-plugins docs/superpowers
git commit -qm "feat: pr-review-loop plugin and spec"
```

- [ ] **Step 6: Verify the plugin survives a nix rebuild (R15)**

`rtk init --global --auto-patch` runs on every home-manager activation. It was
measured to splice rather than clobber, preserving `enabledPlugins`,
`extraKnownMarketplaces`, existing hooks and `permissions` — but verify it on
the real settings file rather than trusting the sandboxed result:

```bash
bb -e '(require (quote [cheshire.core :as json]))
        (let [m (json/parse-string (slurp (str (System/getenv "HOME") "/.claude/settings.json")) true)]
          (println "plugin enabled:" (get-in m [:enabledPlugins (keyword "pr-review-loop@nixpkgs-plugins")]))
          (println "marketplace known:" (some? (get-in m [:extraKnownMarketplaces :nixpkgs-plugins])))
          (println "rtk hook entries:" (count (filter #(re-find #"rtk-rewrite" (str %))
                                                      (mapcat :hooks (get-in m [:hooks :PreToolUse]))))))'
```
Record the three values. Then run your normal nix rebuild, run the same command
again, and confirm all three are unchanged — in particular that `rtk hook
entries` is still `1`, proving no duplicate accumulated.

- [ ] **Step 7: Decide the GitHub Action's fate**

Leave `claude-code-review.yml` in place until the local loop has run on several
real PRs. Removing it is a separate, reversible commit, and it is the only
thing still paying Actions minutes. Do not delete it in this plan.

---

## Deferred

Carried from the spec, not implemented here, each with the test that would
force it into scope:

| # | Item | Forces it in |
|---|---|---|
| D1 | Mid-turn rewake behaviour (spec F1) | A wake arriving mid-edit derails A |
| D2 | `RTK_HOOK_AUDIT=1` reconciliation against the ledger (spec F2) | Ledger passes fewer than observed pushes to PR branches |
| D3 | Cross-machine ledger | A PR reviewed on two machines restarts at pass 1 |
| D4 | Repo-local `settings.local.json` rule flipping the command shape (spec F5) | A pass is missed after adding a project `ask` rule for `Bash(git push:*)` |
| D5 | Posting per-pass comments to the PR | The single pre-merge summary proves too coarse to audit |
| D6 | nREPL access for the reviewer | A finding class is repeatedly missed because it needs runtime inspection |
