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
- B (the reviewer) is confined by `--disallowedTools` plus `--strict-mcp-config`, **not** by `--allowedTools`. `--allowedTools` is a pre-approval allowlist: with `permissions.defaultMode: "bypassPermissions"` and no allow/deny/ask rules, every tool is auto-approved and B had Bash, Write and Edit. Only the deny list removes them from B's function list. It is a deny list, so it is not airtight — a tool added in a future Claude Code version is granted by default and `pr-review.reviewer/denied-tools` needs re-probing on every upgrade.
- Never run `git diff` from inside B. `rtk git diff HEAD~1` returns 195 bytes where plain `git diff` returns 80 162.
- The trigger writes only under `<git-common-dir>/pr-review*` — `git rev-parse --git-common-dir`, not a literal `<repo>/.git`, which is a *file* in a linked worktree. It never touches the work tree, the index, or any ref.
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
  - `(ledger-path git-dir)` → String, `<git-dir>/pr-review-ledger.jsonl`. Every
    fn here takes the clone's **shared git directory** (Task 4's
    `gh/git-common-dir`), never a repo root: `<repo-root>/.git` is a *file* in a
    linked worktree, so a repo-root-relative ledger is unwritable there and
    invisible to the clone's other worktrees
  - `(read-passes git-dir pr-number)` → vector of entry maps, oldest first. The
    only fn here that touches the filesystem — read once, then use the pure
    predicates below, so one decision costs one file read
  - `(append-pass! git-dir entry)` → the entry as written
  - `(next-pass-number passes)` → long, 1 when none
  - `(cap-reached? passes)` → boolean
  - `(reviewed-sha? passes sha)` → boolean — SHA idempotence; a repeat push of
    one commit must not spend a cap slot re-reviewing it
  - `(fingerprint-category fingerprint)` → String or nil, read after the LAST
    colon (a path may contain one)
  - `(suppressible? fingerprint)` → boolean — `suppressible-categories` is
    `#{"correctness/followup" "docs-accuracy" "style"}` and nothing else: the
    spec authorises the one-re-raise rule for follow-up grade only, so
    suppressing a blocking or coverage finding is a false-clean path
  - `(suppressed-fingerprints passes)` → [String], first-appearance order —
    reported on two or more of `passes` AND suppressible
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

(defn- tmp-git-dir
  "A stand-in for the clone's shared git directory. Every ledger function
   takes this, never a repo root: `<repo-root>/.git` is a file in a linked
   worktree."
  []
  (let [d (str (fs/create-temp-dir {:prefix "pr-review-ledger"}) "/.git")]
    (fs/create-dirs d)
    d))

(defn- pass
  [pr sha n & {:keys [fingerprints verdict]}]
  {:pr pr :sha sha :pass n :verdict (or verdict "NOT_MERGEABLE")
   :blocking 1 :followup 0 :coverage 0 :fingerprints (vec fingerprints)})

(deftest ledger-path-is-under-the-git-dir
  (is (= "/r/.git/pr-review-ledger.jsonl" (ledger/ledger-path "/r/.git")))
  (is (= "/main/.git/pr-review-ledger.jsonl"
         (ledger/ledger-path "/main/.git"))
      "given the shared git dir of a worktree, the ledger lands in the main
       clone — one ledger per repository is what makes the cap mean anything
       across worktrees"))

(deftest empty-ledger-starts-at-pass-one
  (let [g (tmp-git-dir)]
    (is (= [] (ledger/read-passes g 370)))
    (is (= 1 (ledger/next-pass-number (ledger/read-passes g 370))))
    (is (false? (ledger/cap-reached? (ledger/read-passes g 370))))))

(deftest passes-are-per-pr-and-monotonic
  (let [g (tmp-git-dir)]
    (ledger/append-pass! g (pass 370 "aaa" 1))
    (ledger/append-pass! g (pass 371 "bbb" 1))
    (ledger/append-pass! g (pass 370 "ccc" 2))
    (testing "reads are filtered by PR"
      (is (= ["aaa" "ccc"] (mapv :sha (ledger/read-passes g 370))))
      (is (= ["bbb"] (mapv :sha (ledger/read-passes g 371)))))
    (testing "next pass number counts only that PR"
      (is (= 3 (ledger/next-pass-number (ledger/read-passes g 370))))
      (is (= 2 (ledger/next-pass-number (ledger/read-passes g 371)))))))

(deftest append-stamps-a-timestamp
  (let [g (tmp-git-dir)
        e (ledger/append-pass! g (pass 1 "x" 1))]
    (is (pos? (:ts e)) "append-pass! must stamp :ts so the cap can be reasoned about over time")))

(deftest cap-blocks-at-max-passes
  (let [g (tmp-git-dir)]
    (doseq [n (range 1 (inc ledger/max-passes))]
      (ledger/append-pass! g (pass 9 (str n) n)))
    (is (true? (ledger/cap-reached? (ledger/read-passes g 9)))
        "at max-passes the trigger must refuse to spawn another reviewer")
    (is (false? (ledger/cap-reached? (ledger/read-passes g 10)))
        "the cap is per PR, not global")))

(deftest reviewed-sha-is-recognised-per-pr
  (let [g (tmp-git-dir)]
    (ledger/append-pass! g (pass 370 "deadbeef" 1))
    (is (true? (ledger/reviewed-sha? (ledger/read-passes g 370) "deadbeef"))
        "a repeat push of one commit must be recognisable, or the same SHA is
         reviewed again on every `git push --tags` and spends a cap slot each time")
    (is (false? (ledger/reviewed-sha? (ledger/read-passes g 370) "newsha")))
    (is (false? (ledger/reviewed-sha? (ledger/read-passes g 371) "deadbeef"))
        "SHA identity is scoped to the PR")
    (is (false? (ledger/reviewed-sha? (ledger/read-passes g 370) nil))
        "an unresolvable HEAD must not match every recorded pass")))

(deftest fingerprint-category-reads-after-the-last-colon
  (is (= "style" (ledger/fingerprint-category "src/a.clj:1:style")))
  (is (= "correctness/blocking"
         (ledger/fingerprint-category "src/a.clj:1:correctness/blocking")))
  (is (= "style" (ledger/fingerprint-category "src/pool:v2/file.clj:34:style"))
      "a path may contain a colon; splitting on colons would read \"v2\" as the
       category and silently mis-classify the finding")
  (is (nil? (ledger/fingerprint-category "nocolons"))))

(deftest only-followup-grade-categories-are-suppressible
  (testing "the spec authorises the one-re-raise rule for follow-up grade only"
    (is (true? (ledger/suppressible? "src/a.clj:1:correctness/followup")))
    (is (true? (ledger/suppressible? "src/a.clj:1:docs-accuracy")))
    (is (true? (ledger/suppressible? "src/a.clj:1:style"))))
  (testing "a blocking or coverage finding that survived two passes is not
            fixed; suppressing it makes the next pass report MERGEABLE with
            the defect still in the tree"
    (is (false? (ledger/suppressible? "src/a.clj:1:correctness/blocking")))
    (is (false? (ledger/suppressible? "src/a.clj:1:coverage")))))

(deftest suppressed-fingerprints-needs-two-raises-and-a-suppressible-category
  (let [blocking "src/a.clj:1:correctness/blocking"
        coverage "src/a.clj:2:coverage"
        followup "src/a.clj:3:correctness/followup"
        once     "src/a.clj:4:style"
        passes [(pass 1 "a" 1 :fingerprints [blocking coverage followup])
                (pass 1 "b" 2 :fingerprints [blocking coverage followup once])]]
    (is (= [followup] (ledger/suppressed-fingerprints passes))
        "a blocking finding reported on two passes must still be reported on
         the third: filtering only on the raise count is a false-clean path,
         not a convergence aid")
    (is (= [] (ledger/suppressed-fingerprints [(first passes)]))
        "one sighting is not a re-raise")))

(deftest suppressed-fingerprints-counts-a-pass-once
  (let [fp "src/a.clj:9:style"
        passes [(pass 1 "a" 1 :fingerprints [fp fp])]]
    (is (= [] (ledger/suppressed-fingerprints passes))
        "one pass reporting the same finding twice is still one raise")))

(deftest suppressed-fingerprints-order-is-first-appearance
  (let [a "z/a.clj:1:style" b "a/b.clj:2:style" c "m/c.clj:3:docs-accuracy"
        passes [(pass 1 "1" 1 :fingerprints [a b c])
                (pass 1 "2" 2 :fingerprints [a b c])]]
    (is (= [a b c] (ledger/suppressed-fingerprints passes))
        "the do-not-re-raise list is rendered into the prompt; a hash-order
         list would churn the prompt between passes for no reason")))

(deftest suppressed-fingerprints-reads-the-ledger-once
  (testing "the filter used to call a per-fingerprint helper that re-slurped
            the whole ledger: 138 full file reads for one decision at nine
            passes and fifteen findings"
    (let [g (tmp-git-dir)
          fp "src/a.clj:1:style"]
      (doseq [n [1 2]]
        (ledger/append-pass! g (pass 370 (str n) n :fingerprints [fp])))
      (let [reads (atom 0)
            orig  slurp]
        (with-redefs [slurp (fn [& args] (swap! reads inc) (apply orig args))]
          (let [passes (ledger/read-passes g 370)]
            (is (= [fp] (ledger/suppressed-fingerprints passes)))))
        (is (= 1 @reads)
            "one decision must cost exactly one read of the ledger file")))))

(deftest corrupt-lines-are-skipped-not-fatal
  (let [g (tmp-git-dir)]
    (ledger/append-pass! g (pass 2 "ok" 1))
    (spit (ledger/ledger-path g) "{not json\n" :append true)
    (is (= ["ok"] (mapv :sha (ledger/read-passes g 2)))
        "a truncated write from a killed reviewer must not break every later read")))

(deftest interrupted-write-does-not-lose-prior-passes
  (let [g (tmp-git-dir)]
    (ledger/append-pass! g (pass 42 "first" 1))
    ;; Simulate a reviewer killed after the new pass is durably written to the
    ;; temp file but before it is published.
    (with-redefs [ledger/atomic-replace! (fn [_ _] (throw (ex-info "simulated crash before publish" {})))]
      (is (thrown? Exception (ledger/append-pass! g (pass 42 "second" 2)))))
    (is (= ["first"] (mapv :sha (ledger/read-passes g 42)))
        "a crash between the temp write and the atomic rename must leave every
         previously recorded pass intact, not zero the ledger")))

(deftest append-pass-flocks-a-guard-file-not-the-ledger-path
  (let [g (tmp-git-dir)
        seen (atom [])]
    (with-redefs [flock/with-file-lock (fn [path f] (swap! seen conj path) (f))]
      (ledger/append-pass! g (pass 77 "one" 1))
      (ledger/append-pass! g (pass 77 "two" 2)))
    (testing "both sequential appends still land"
      (is (= ["one" "two"] (mapv :sha (ledger/read-passes g 77)))))
    (testing "the flock target is a sibling guard file, never the ledger path append-pass! renames over"
      (is (= [(flock/guard-path (ledger/ledger-path g)) (flock/guard-path (ledger/ledger-path g))]
             @seen))
      (is (not-any? #{(ledger/ledger-path g)} @seen)
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
  "Append-only, per-clone record of review passes, and the termination rules
   read off it.

   Lives under the clone's shared git directory so it survives session
   restarts, context compaction and `claude` upgrades, needs no network, and
   works before a PR exists. Same precedent as Claude Code's own
   .git/claude-trailers.

   Every function takes `git-dir` — what pr-review.gh/git-common-dir
   resolved — never a repo root. In a linked worktree `<root>/.git` is a
   file, so a repo-root-relative ledger is unwritable there and invisible to
   every other worktree of the same clone.

   The policy predicates are pure over an already-read `passes` collection so
   one decision costs one file read. They used to each read the file
   themselves, and the one-re-raise filter called a per-fingerprint helper
   that re-slurped the whole ledger — 138 full reads for one decision at nine
   passes and fifteen findings."
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

(def suppressible-categories
  "The only finding categories the one-re-raise rule may ever silence.

   The spec authorises it for follow-up grade only — \"A follow-up-grade
   finding may be re-raised once, then it stays on the list.\" A
   correctness/blocking or coverage finding still present after two passes
   has not been fixed; silencing it makes the next pass report MERGEABLE with
   the defect still in the tree, and the loop then merges broken code."
  #{"correctness/followup" "docs-accuracy" "style"})

(def ^:private max-lines
  "Ledger is trimmed to this many lines under the write lock. At ~200 bytes a
   line this bounds the file at ~100KB."
  500)

(defn ledger-path
  [git-dir]
  (str git-dir "/pr-review-ledger.jsonl"))

(defn- parse-line
  [line]
  (try (json/parse-string line true)
       (catch Exception _ nil)))

(defn- read-all
  [git-dir]
  (let [p (ledger-path git-dir)]
    (if-not (fs/exists? p)
      []
      (into [] (keep parse-line) (str/split-lines (slurp p))))))

(defn read-passes
  "Every recorded pass for `pr-number`, oldest first. Unparseable lines are
   skipped: a reviewer killed mid-write must not break all later reads.

   This is the only function here that touches the filesystem. Read once,
   then hand the result to the pure predicates below."
  [git-dir pr-number]
  (filterv #(= pr-number (:pr %)) (read-all git-dir)))

(defn next-pass-number
  [passes]
  (inc (count passes)))

(defn cap-reached?
  [passes]
  (>= (count passes) max-passes))

(defn reviewed-sha?
  "True when `passes` already records a completed pass at `sha`.

   Repeat pushes of one commit are ordinary: `git push` twice, `git push
   --tags`, `--dry-run` and `--delete` all match `Bash(git push:*)`. Without
   this the same commit is reviewed again on every one of them, spending a
   cap slot each time and telling agent A nothing it has not already been
   told."
  [passes sha]
  (boolean (and sha (some #(= sha (:sha %)) passes))))

(defn fingerprint-category
  "The category segment of a `<file>:<line>:<category>` fingerprint.

   Read after the LAST colon rather than by splitting on colons: a path may
   contain one (`src/pool:v2/file.clj:34:style`), a category never does."
  [fingerprint]
  (let [s (str fingerprint)]
    (when-let [i (str/last-index-of s ":")]
      (subs s (inc i)))))

(defn suppressible?
  "Whether the one-re-raise rule is allowed to silence this fingerprint at
   all — see `suppressible-categories`."
  [fingerprint]
  (contains? suppressible-categories (fingerprint-category fingerprint)))

(defn suppressed-fingerprints
  "Fingerprints the next reviewer must not raise again: reported on two or
   more of `passes` AND of a suppressible category.

   Both conditions are load-bearing. Without the count the rule fires on a
   first sighting; without the category filter it fires on blocking and
   coverage findings, which is a false-clean path, not a convergence aid.

   Order is first-appearance so the prompt's do-not-re-raise list is stable
   between passes."
  [passes]
  (let [per-pass (mapv (comp distinct :fingerprints) passes)
        all      (vec (apply concat per-pass))
        freq     (frequencies all)]
    (->> all
         distinct
         (filterv #(and (>= (get freq % 0) 2) (suppressible? %))))))

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
  [git-dir entry]
  (let [entry (assoc entry :ts (System/currentTimeMillis))
        p (ledger-path git-dir)]
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
  - `(lock-path git-dir)` → String, `<git-dir>/pr-review.lock`. `git-dir` is the
    clone's shared git directory (Task 4's `gh/git-common-dir`), so all
    worktrees of one repository contend for one lock
  - `(read-lock git-dir)` → nil or `{:pid long :pr long :sha String :started long}`
  - `(alive? pid)` → boolean
  - `(kill-reviewers! pid)` → long or nil. SIGTERMs the process subtree **below**
    `pid` and not `pid` itself: `pid` is the bb trigger and the reviewer is its
    `claude -p` child, so killing `pid` left the reviewer running to completion
    (R14 unmet, two concurrent reviewers) and made the loser exit 143
  - `(acquire! git-dir {:pr long :sha String} opts)` → `{:status :acquired}` | `{:status :duplicate}` | `{:status :superseded :killed-pid long}`
  - `(superseded? git-dir opts)` → boolean. True when the record no longer names
    `opts`'s `:pid` — how a loser learns it lost, so it can exit 0 quietly
    instead of recording a truncated review. A missing record counts as
    superseded
  - `(release! git-dir)` / `(release! git-dir opts)` → nil. Deletes the lock
    record only if it is still held by `opts`'s `:pid` (default this process),
    under the same guard flock `acquire!` uses
  - `opts` accepts `:kill-fn` (default `kill-reviewers!`) and `:pid` (default this process)

- [ ] **Step 1: Write the failing test**

`test/pr_review/lock_test.clj`:

```clojure
(ns pr-review.lock-test
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
            [pr-review.flock :as flock]
            [pr-review.lock :as lock]))

(defn- tmp-git-dir
  "A stand-in for the clone's shared git directory — what
   pr-review.gh/git-common-dir resolves. Never a repo root: `<root>/.git` is a
   file in a linked worktree, so a repo-root-relative lock is unwritable there
   and invisible to the clone's other worktrees."
  []
  (let [d (str (fs/create-temp-dir {:prefix "pr-review-lock"}) "/.git")]
    (fs/create-dirs d)
    d))

(defn- write-lock! [git-dir m]
  (spit (lock/lock-path git-dir) (json/generate-string m)))

(deftest lock-path-is-under-the-git-dir
  (is (= "/r/.git/pr-review.lock" (lock/lock-path "/r/.git"))))

(deftest alive-tracks-real-processes
  (is (true? (lock/alive? (.pid (java.lang.ProcessHandle/current)))))
  (is (false? (lock/alive? 999999)) "an absent PID must read as dead, not as held"))

(deftest acquire-on-free-repo-succeeds-and-records-identity
  (let [r (tmp-git-dir)
        res (lock/acquire! r {:pr 370 :sha "abc"} {:pid 4242})]
    (is (= :acquired (:status res)))
    (is (= {:pid 4242 :pr 370 :sha "abc"}
           (select-keys (lock/read-lock r) [:pid :pr :sha])))
    (is (pos? (:started (lock/read-lock r))))))

(deftest same-sha-in-flight-is-a-duplicate
  (let [r (tmp-git-dir)
        self (.pid (java.lang.ProcessHandle/current))]
    (write-lock! r {:pid self :pr 370 :sha "abc" :started 1})
    (is (= :duplicate (:status (lock/acquire! r {:pr 370 :sha "abc"} {:pid 1})))
        "two hooks for one push must not run two reviewers")
    (is (= self (:pid (lock/read-lock r))) "the incumbent keeps the lock")))

(deftest newer-sha-supersedes-and-kills-the-incumbent
  (let [r (tmp-git-dir)
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
  (let [r (tmp-git-dir)
        killed (atom nil)]
    (write-lock! r {:pid 999999 :pr 370 :sha "old" :started 1})
    (let [res (lock/acquire! r {:pr 370 :sha "new"}
                             {:pid 5 :kill-fn #(reset! killed %)})]
      (is (= :acquired (:status res))
          "a crashed reviewer must not block every future review")
      (is (nil? @killed) "nothing to kill when the holder is already gone"))))

(deftest corrupt-lock-file-is-treated-as-free
  (let [r (tmp-git-dir)]
    (spit (lock/lock-path r) "{not json")
    (is (= :acquired (:status (lock/acquire! r {:pr 1 :sha "s"} {:pid 6}))))))

(deftest release-removes-the-lock
  (let [r (tmp-git-dir)]
    (lock/acquire! r {:pr 1 :sha "s"} {:pid 7})
    (lock/release! r {:pid 7})
    (is (nil? (lock/read-lock r)))))

(deftest acquire-runs-under-the-shared-flock-on-the-guard-path
  (let [r (tmp-git-dir)
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
  (let [r (tmp-git-dir)]
    (spit (lock/lock-path r) "{}")
    (is (= :acquired (:status (lock/acquire! r {:pr 1 :sha "s"} {:pid 10})))
        "a lock record that is valid JSON but missing :pid must read as free, not
         throw: read-lock returning {} truthy would send (alive? nil) into
         (long nil), an NPE")))

(deftest release-does-not-delete-a-record-owned-by-a-different-pid
  (let [r (tmp-git-dir)]
    (write-lock! r {:pid 4242 :pr 370 :sha "new" :started 1})
    (lock/release! r {:pid 999})
    (is (= {:pid 4242 :pr 370 :sha "new"}
           (select-keys (lock/read-lock r) [:pid :pr :sha]))
        "release! must never delete a record that belongs to a different holder:
         a reviewer finishing normally must not be able to delete the record a
         concurrent acquire! just wrote for the process that superseded it")))

(deftest release-deletes-its-own-record
  (let [r (tmp-git-dir)]
    (write-lock! r {:pid 4242 :pr 370 :sha "new" :started 1})
    (lock/release! r {:pid 4242})
    (is (nil? (lock/read-lock r))
        "the ownership check must not be so strict it turns release! into a
         no-op for its own record")))

(deftest release-runs-under-the-shared-flock-on-the-guard-path
  (let [r (tmp-git-dir)
        seen (atom nil)]
    (write-lock! r {:pid 4242 :pr 370 :sha "new" :started 1})
    (with-redefs [flock/with-file-lock (fn [path f] (reset! seen path) (f))]
      (lock/release! r {:pid 4242}))
    (is (some? @seen)
        "release! must run under pr-review.flock/with-file-lock so it cannot
         interleave with an in-flight acquire!")
    (is (= (flock/guard-path (lock/lock-path r)) @seen)
        "release! must flock the same sibling guard path acquire! uses")
    (is (not= (lock/lock-path r) @seen)
        "release! must never flock the lock record path itself")))

(deftest kill-targets-the-reviewer-subtree-not-the-recorded-process
  (testing "the recorded pid is the bb trigger; the reviewer is its child.
            SIGTERMing the trigger — what this used to do — left the `claude
            -p` child running to completion, so a supersede gave two
            concurrent reviewers for the 2-6 minutes a review takes, and made
            the losing trigger exit 143"
    ;; A babashka parent that spawns a child and then sleeps in-process, not
    ;; a shell: the same shape as the real trigger, which spawns `claude -p`
    ;; and blocks on it. A `sh -c` parent would exit the moment its
    ;; foreground child died and prove nothing about who was killed.
    (let [proc (p/process ["bb" "-e" "(require '[babashka.process :as p]) (p/process [\"sleep\" \"30\"]) (Thread/sleep 20000)"]
                          {:out :string :err :string})
          pid  (.pid (:proc proc))]
      (try
        (Thread/sleep 2000)
        (let [h    (.get (java.lang.ProcessHandle/of (long pid)))
              kids (vec (iterator-seq (.iterator (.descendants h))))]
          (is (seq kids)
              "fixture precondition: the recorded process must actually have a
               child to stand in for the reviewer")
          (lock/kill-reviewers! pid)
          (Thread/sleep 800)
          (is (every? #(not (.isAlive %)) kids)
              "the reviewer child must be dead")
          (is (true? (lock/alive? pid))
              "the recorded trigger must survive, so it can reach its own
               System/exit 0 instead of dying with 143"))
        (finally (p/destroy-tree proc))))))

(deftest kill-on-a-dead-pid-is-not-an-error
  (is (nil? (lock/kill-reviewers! 999999))
      "a supersede must never throw out of acquire!: the trigger's only legal
       exit codes are 0 and 2"))

(deftest superseded-is-false-while-this-process-still-holds-the-lock
  (let [g (tmp-git-dir)]
    (lock/acquire! g {:pr 1 :sha "s"} {:pid 4242})
    (is (false? (lock/superseded? g {:pid 4242})))))

(deftest superseded-is-true-once-another-trigger-takes-the-slot
  (let [g (tmp-git-dir)]
    (lock/acquire! g {:pr 1 :sha "old"} {:pid 4242})
    (write-lock! g {:pid 777 :pr 1 :sha "new" :started 1})
    (is (true? (lock/superseded? g {:pid 4242}))
        "this is how a loser learns it lost: its reviewer was killed
         mid-answer, and recording that truncated output would spend a cap
         slot and wake agent A with findings for a stale SHA")))

(deftest a-missing-lock-record-reads-as-superseded
  (let [g (tmp-git-dir)]
    (is (true? (lock/superseded? g {:pid 4242}))
        "no record can only mean this trigger was superseded and the winner
         has since released, or that something outside the loop deleted it;
         publishing a pass whose lock is gone is the riskier of the two")))
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
   already stale — the newer push kills the older reviewer and takes over.

   `git-dir` throughout is the clone's shared git directory (see
   pr-review.gh/git-common-dir), so every worktree of one repository
   contends for the same lock. A worktree-local lock would let one push per
   worktree run a reviewer concurrently, which is the thing this namespace
   exists to prevent.

   Nothing here may make the trigger exit anything but 0 or 2: any other code
   makes Claude Code print `Failed with non-blocking status code:` and the
   pass is silently lost. That is why the kill targets the superseded
   reviewer rather than the superseded trigger — a SIGTERMed babashka exits
   143."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [pr-review.flock :as flock]))

(defn lock-path
  [git-dir]
  (str git-dir "/pr-review.lock"))

(defn read-lock
  "Current lock record, or nil when absent, unparseable, or missing a
   usable :pid. A corrupt or incomplete lock reads as free: a half-written
   file must not wedge the loop forever."
  [git-dir]
  (let [p (lock-path git-dir)]
    (when (fs/exists? p)
      (let [parsed (try (json/parse-string (slurp p) true)
                        (catch Exception _ nil))]
        (when (:pid parsed)
          parsed)))))

(defn alive?
  [pid]
  (let [h (java.lang.ProcessHandle/of (long pid))]
    (and (.isPresent h) (.isAlive (.get h)))))

(defn kill-reviewers!
  "SIGTERM the whole process subtree *below* `pid`, and not `pid` itself.

   `pid` is the recorded trigger, and the reviewer is its `claude -p` child.
   Killing the trigger — what this used to do — left that child running to
   completion, so a supersede produced two concurrent reviewers for the two
   to six minutes a review takes: R14 unmet. It also made the losing trigger
   exit 143, a third exit code this module's contract forbids by name.

   Killing downward instead fixes both. The reviewer dies, the loser stays
   alive to reach its own `System/exit 0`, and it learns it lost by finding
   the lock record no longer names it (see `superseded?`).

   The descendant set is snapshotted before any destroy so a dying
   intermediate process cannot orphan a grandchild out of the walk."
  [pid]
  (try
    (let [opt (java.lang.ProcessHandle/of (long pid))]
      (when (.isPresent opt)
        (let [kids (vec (iterator-seq (.iterator (.descendants (.get opt)))))]
          (doseq [k kids] (.destroy k))
          (count kids))))
    (catch Exception _ nil)))

(defn- write-lock!
  [git-dir {:keys [pid pr sha]}]
  (fs/create-dirs (fs/parent (lock-path git-dir)))
  (spit (lock-path git-dir)
        (json/generate-string {:pid pid :pr pr :sha sha
                               :started (System/currentTimeMillis)})))

(defn acquire!
  "Take the reviewer lock for (`pr`, `sha`).

   :duplicate  — a live reviewer already holds this exact SHA. Caller exits 0.
   :superseded — a live reviewer held an older SHA; its reviewer was killed.
   :acquired   — the lock was free, corrupt, or held by a dead process.

   The whole read-check-write runs under a shared flock on
   `(flock/guard-path (lock-path git-dir))`, never on the lock path
   itself: acquire! rewrites (and release! deletes) that path, so a lock
   held on it would stop protecting anything the instant it's rewritten.
   A guard file that acquire! never touches keeps the flock's identity
   independent of the record's lifecycle, so two concurrent triggers in
   the same clone cannot both observe a free or dead lock and both
   proceed."
  [git-dir {:keys [pr sha]} {:keys [pid kill-fn]}]
  (flock/with-file-lock
    (flock/guard-path (lock-path git-dir))
    (fn []
      (let [pid (or pid (.pid (java.lang.ProcessHandle/current)))
            kill-fn (or kill-fn kill-reviewers!)
            held (read-lock git-dir)]
        (cond
          (and held (alive? (:pid held)) (= sha (:sha held)))
          {:status :duplicate}

          (and held (alive? (:pid held)))
          (do (kill-fn (:pid held))
              (write-lock! git-dir {:pid pid :pr pr :sha sha})
              {:status :superseded :killed-pid (:pid held)})

          :else
          (do (write-lock! git-dir {:pid pid :pr pr :sha sha})
              {:status :acquired}))))))

(defn superseded?
  "True when the lock record no longer names `pid` — another trigger took the
   reviewer slot while this one was working.

   This is how a loser learns it lost. `kill-reviewers!` kills the reviewer
   child, not the trigger, so the trigger returns from a reviewer that was
   SIGTERMed mid-answer; recording that as a pass would spend a cap slot on a
   truncated review and wake agent A with findings for a SHA that is already
   stale.

   A missing record counts as superseded too. It can only mean this trigger
   was superseded and the winner has since released, or that something
   outside the loop deleted the record; in both cases the conservative move
   is to stay quiet rather than publish a pass whose lock is gone."
  [git-dir {:keys [pid]}]
  (let [pid (or pid (.pid (java.lang.ProcessHandle/current)))]
    (not= pid (:pid (read-lock git-dir)))))

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
  ([git-dir] (release! git-dir {}))
  ([git-dir {:keys [pid]}]
   (let [pid (or pid (.pid (java.lang.ProcessHandle/current)))]
     (flock/with-file-lock
       (flock/guard-path (lock-path git-dir))
       (fn []
         (let [held (read-lock git-dir)]
           (when (= pid (:pid held))
             (fs/delete-if-exists (lock-path git-dir))))))
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

> **Shipped code is authoritative for this task.** A fix round hardened it after
> the blocks below were written: `gh/diff` now bypasses `ok-out` so the diff is
> returned byte-untouched, and `:diff-bytes` is read from the file with
> `fs/size` rather than counting UTF-16 code units. The shipped
> `context_test.clj` also carries a seventh test,
> `build-through-real-defaults-keeps-real-diff-bytes`, that the Step 1 block
> below does not — it is the only test exercising `build!`'s real wiring into
> `gh.clj`, and it is what guards this exact defect class. Read
> `claude-code-plugins/hooks/pr_review/{gh,context}.clj` and
> `claude-code-plugins/test/pr_review/{gh,context}_test.clj` as built, and see
> commit `d5e7d7d81`.

**Files:**
- Create: `~/.nixpkgs/claude-code-plugins/hooks/pr_review/gh.clj`
- Test: `~/.nixpkgs/claude-code-plugins/test/pr_review/gh_test.clj`

**Interfaces:**
- Consumes: nothing
- Produces (every fn takes a trailing `opts` map honouring `:sh`):
  - `(default-sh args dir)` → `{:exit long :out String :err String}`
  - `(repo-root cwd opts)` → String or nil
  - `(current-branch repo-root opts)` → String or nil
  - `(git-common-dir repo-root opts)` → String or nil — `git rev-parse
    --git-common-dir` resolved against `repo-root`. Relative (`.git`) in an
    ordinary clone, absolute inside a linked worktree, where `<repo-root>/.git`
    is a *file*. Every state module takes this instead of assuming
    `<repo-root>/.git`; callers fall back to `<repo-root>/.git` on nil
  - `(head-sha repo-root opts)` → String or nil
  - `(open-pr repo-root branch opts)` → nil or `{:number long :isDraft boolean :baseRefName String}`
  - `(merge-base repo-root base-ref opts)` → String or nil
  - `(diff repo-root base sha opts)` → String on success (`""` when the range is genuinely empty), or **nil when the diff command itself failed**. The two must stay distinguishable: collapsing them let an unresolvable base ref produce a 0-byte diff that a reviewer then rubber-stamped as MERGEABLE.

- [ ] **Step 1: Write the failing test**

`test/pr_review/gh_test.clj`:

```clojure
(ns pr-review.gh-test
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.test :refer [deftest is testing]]
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

(deftest diff-distinguishes-a-failure-from-a-real-empty-diff
  (is (nil? (gh/diff "/repo" "origin/main" "headsha"
                      {:sh (fn [_ _] {:exit 1 :out ""
                                      :err "fatal: bad revision 'origin/main'"})}))
      "a non-zero exit — e.g. an unresolved base ref — must come back as nil,
       never as \"\", or a failed diff looks exactly like a real empty one")
  (is (= "" (gh/diff "/repo" "origin/main" "headsha"
                      {:sh (fn [_ _] {:exit 0 :out "" :err ""})}))
      "a zero exit with no output is a genuinely empty diff and must still
       come back as \"\", not nil"))

(deftest git-common-dir-resolves-a-relative-answer-against-the-repo-root
  (let [calls (atom [])
        sh (stub {["git" "rev-parse"] {:exit 0 :out ".git\n" :err ""}} calls)]
    (is (= "/repo/.git" (gh/git-common-dir "/repo" {:sh sh}))
        "git answers relatively in an ordinary clone, and a relative path
         would be resolved against the hook's cwd, not the repo")
    (is (= ["git" "rev-parse" "--git-common-dir"] (first @calls))
        "--git-common-dir, not --git-dir: all worktrees of one clone must
         share one ledger and one lock, and --git-dir gives each worktree its
         own private directory")))

(deftest git-common-dir-keeps-an-absolute-answer
  (let [sh (stub {["git" "rev-parse"] {:exit 0 :out "/main/.git\n" :err ""}} (atom []))]
    (is (= "/main/.git" (gh/git-common-dir "/wt" {:sh sh}))
        "inside a worktree git answers with the main clone's .git; joining
         that onto the worktree root would invent a path that does not exist")))

(deftest git-common-dir-is-nil-when-git-fails
  (is (nil? (gh/git-common-dir "/repo" {:sh (fn [_ _] {:exit 128 :out "" :err "no"})}))
      "callers fall back to <repo-root>/.git, so a failure must be nil rather
       than a throw out of the hook"))

(deftest git-common-dir-in-a-real-worktree-points-at-the-main-clone
  (testing "the fixture no test had: a repo root whose .git is a FILE.
            fs/create-dirs on it throws FileAlreadyExistsException, which is
            what made every push from a worktree exit 1"
    (let [tmp  (str (fs/create-temp-dir {:prefix "pr-review-gh-wt"}))
          main (str tmp "/main")
          wt   (str tmp "/wt")
          git! (fn [dir & args]
                 (let [{:keys [exit err]} (p/sh (into ["git"] args) {:dir dir})]
                   (when-not (zero? exit)
                     (throw (ex-info (str "fixture git failed: " args " " err) {})))))]
      (fs/create-dirs main)
      (git! main "init" "-q")
      (git! main "config" "user.email" "t@t.t")
      (git! main "config" "user.name" "t")
      (spit (str main "/f") "hi")
      (git! main "add" "f")
      (git! main "commit" "-qm" "init")
      (git! main "worktree" "add" "-q" wt "-b" "feat")
      (is (fs/regular-file? (str wt "/.git"))
          "fixture precondition: a linked worktree's .git is a file, not a
           directory")
      (is (fs/directory? (gh/git-common-dir wt {}))
          "the resolved git dir must be a real directory, or every ledger and
           lock write under it fails")
      (is (= (str (fs/real-path (str main "/.git")))
             (str (fs/real-path (gh/git-common-dir wt {}))))
          "a worktree must resolve to the main clone's .git so both share one
           ledger, one lock and one context directory"))))
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
  (let [{:keys [exit out]} (run opts ["git" "diff" (str base "..." sha)] repo-root)]
    (when (zero? exit) out)))

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

> **Shipped code is authoritative for this task.** A fix round hardened it after
> the blocks below were written: `gh/diff` now bypasses `ok-out` so the diff is
> returned byte-untouched, and `:diff-bytes` is read from the file with
> `fs/size` rather than counting UTF-16 code units. The shipped
> `context_test.clj` also carries a seventh test,
> `build-through-real-defaults-keeps-real-diff-bytes`, that the Step 1 block
> below does not — it is the only test exercising `build!`'s real wiring into
> `gh.clj`, and it is what guards this exact defect class. Read
> `claude-code-plugins/hooks/pr_review/{gh,context}.clj` and
> `claude-code-plugins/test/pr_review/{gh,context}_test.clj` as built, and see
> commit `d5e7d7d81`.

**Files:**
- Create: `~/.nixpkgs/claude-code-plugins/hooks/pr_review/context.clj`
- Test: `~/.nixpkgs/claude-code-plugins/test/pr_review/context_test.clj`

**Interfaces:**
- Consumes: `pr-review.gh/{merge-base,diff}` (head SHA is supplied by the caller, not read here)
- Produces:
  - `(context-dir git-dir)` → String, `<git-dir>/pr-review-context`
  - `(build! repo-root git-dir {:pr long :sha String :base-ref String} opts)` → `{:diff-path String :changed-files [String] :base String :sha String :diff-bytes long :diff-failed? boolean}` — `:diff-failed?` is true only when the diff command itself failed (e.g. an unresolved base ref), never for a genuinely empty diff. `repo-root` is where git runs; `git-dir` is where the result is written, and the two differ in a linked worktree
  - `(prune! git-dir keep)` → long, number of files deleted

- [ ] **Step 1: Write the failing test**

`test/pr_review/context_test.clj`:

```clojure
(ns pr-review.context-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [pr-review.context :as context]))

(defn- tmp-repo
  "Returns [repo-root git-dir]. They are separate arguments to `build!` on
   purpose: git runs in the work tree, the context is written under the
   clone's shared git directory, and in a linked worktree those differ."
  []
  (let [d (str (fs/create-temp-dir {:prefix "pr-review-ctx"}))
        g (str d "/.git")]
    (fs/create-dirs g)
    [d g]))

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
  (let [[r g] (tmp-repo)
        res (context/build! r g {:pr 370 :sha "headsha" :base-ref "main"}
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
  (let [[r g] (tmp-repo)
        res (context/build! r g {:pr 370 :sha "abc123" :base-ref "main"}
                            {:merge-base-fn (constantly "b") :diff-fn (constantly "d")
                             :sh stub-sh})]
    (is (= (str (context/context-dir g) "/abc123.diff") (:diff-path res))
        "one file per SHA so a superseded reviewer's diff is never overwritten
         under it mid-read")))

(deftest changed-files-are-extracted-from-the-diff
  (let [[r g] (tmp-repo)
        res (context/build! r g {:pr 1 :sha "s" :base-ref "main"}
                            {:merge-base-fn (constantly "b")
                             :diff-fn (constantly sample-diff)
                             :sh stub-sh})]
    (is (= ["src/a.clj" "test/b_test.clj"] (:changed-files res)))))

(deftest missing-merge-base-falls-back-to-the-base-ref
  (let [[r g] (tmp-repo)
        res (context/build! r g {:pr 1 :sha "s" :base-ref "main"}
                            {:merge-base-fn (constantly nil)
                             :diff-fn (constantly "d") :sh stub-sh})]
    (is (= "origin/main" (:base res))
        "an unfetched base must still produce a reviewable range, not nil")))

(deftest empty-diff-is-reported-not-hidden
  (let [[r g] (tmp-repo)
        res (context/build! r g {:pr 1 :sha "s" :base-ref "main"}
                            {:merge-base-fn (constantly "b")
                             :diff-fn (constantly "") :sh stub-sh})]
    (is (= 0 (:diff-bytes res)))
    (is (= [] (:changed-files res)))
    (testing "the file still exists so the prompt never names a missing path"
      (is (fs/exists? (:diff-path res))))))

(deftest prune-keeps-the-newest-contexts
  (let [[r g] (tmp-repo)]
    (doseq [n ["a" "b" "c" "d"]]
      (context/build! r g {:pr 1 :sha n :base-ref "main"}
                      {:merge-base-fn (constantly "b") :diff-fn (constantly "d")
                       :sh stub-sh})
      (Thread/sleep 5))
    (is (= 2 (context/prune! g 2)))
    (is (= 2 (count (fs/glob (context/context-dir g) "*.diff"))))))

(deftest build-through-real-defaults-keeps-real-diff-bytes
  (let [[r g] (tmp-repo)
        payload "diff --git a/café.clj b/café.clj\n@@ -1 +1 @@\n-(def café 1)\n+(def café 2)\n"
        responses {["git" "merge-base"] {:exit 0 :out "basesha\n" :err ""}
                   ["git" "diff"]       {:exit 0 :out payload :err ""}}
        sh (fn [args _dir]
             (get responses (vec (take 2 args)) {:exit 1 :out "" :err "unstubbed"}))
        res (context/build! r g {:pr 1 :sha "s" :base-ref "main"} {:sh sh})
        on-disk (fs/size (:diff-path res))]
    (testing "no :diff-fn or :merge-base-fn override, so build! runs through
              gh/merge-base and gh/diff's real default wiring"
      (is (= payload (slurp (:diff-path res)))
          "the file on disk must be byte-identical to git's raw output,
           trailing newline included — a :diff-fn stub would have hidden
           ok-out silently trimming it")
      (is (= on-disk (:diff-bytes res)))
      (is (> on-disk (count payload))
          ":diff-bytes must count UTF-8 bytes on disk, not UTF-16 code units,
           or a non-ASCII diff would under-report its own size"))))

(deftest build-flags-a-failed-diff
  (let [[r g] (tmp-repo)
        res (context/build! r g {:pr 1 :sha "s" :base-ref "main"}
                            {:merge-base-fn (constantly nil)
                             :diff-fn (constantly nil)
                             :sh stub-sh})]
    (is (true? (:diff-failed? res))
        "a nil from diff-fn means the diff command itself failed — an
         unresolved base ref, most often — and must be flagged, not
         silently written as an empty file that reads as a clean pass")
    (is (fs/exists? (:diff-path res))
        "the file is still written even on failure, so nothing downstream
         ever names a missing path")
    (is (= 0 (:diff-bytes res)))))

(deftest build-does-not-flag-a-legitimately-empty-diff
  (let [[r g] (tmp-repo)
        res (context/build! r g {:pr 1 :sha "s" :base-ref "main"}
                            {:merge-base-fn (constantly "b")
                             :diff-fn (constantly "")
                             :sh stub-sh})]
    (is (false? (:diff-failed? res))
        "a real empty diff (exit 0, no output) is not a failure — flagging
         it too would make the flag meaningless")))
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
  "Under the clone's shared git directory (pr-review.gh/git-common-dir), not
   `<repo-root>/.git`: that path is a file in a linked worktree, so
   `fs/create-dirs` on it throws and every push from a worktree died."
  [git-dir]
  (str git-dir "/pr-review-context"))

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

   :diff-failed? is true when the diff command itself could not be produced
   (e.g. an unresolved base ref) — never conflated with a genuinely empty
   diff, which reports false. The file is still written (empty, in that
   case) either way, so nothing downstream ever names a missing path.

   `repo-root` is where git runs; `git-dir` is where the result is written.
   The two differ in a linked worktree and must not be conflated.

   opts may override :merge-base-fn and :diff-fn for testing; both default to
   the real git calls in pr-review.gh."
  [repo-root git-dir {:keys [sha base-ref]} opts]
  (let [merge-base-fn (or (:merge-base-fn opts)
                          #(gh/merge-base repo-root base-ref opts))
        diff-fn       (or (:diff-fn opts)
                          #(gh/diff repo-root %1 %2 opts))
        base          (or (merge-base-fn) (str "origin/" base-ref))
        diff-result   (diff-fn base sha)
        diff-failed?  (nil? diff-result)
        diff-text     (or diff-result "")
        dir           (context-dir git-dir)
        diff-path     (str dir "/" sha ".diff")]
    (fs/create-dirs dir)
    (spit diff-path diff-text)
    {:diff-path     diff-path
     :changed-files (changed-files diff-text)
     :base          base
     :sha           sha
     :diff-bytes    (fs/size diff-path)
     :diff-failed?  diff-failed?}))

(defn prune!
  "Delete all but the `keep` newest .diff files. Returns how many were removed."
  [git-dir keep]
  (let [files (->> (fs/glob (context-dir git-dir) "*.diff")
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

> **Shipped code is authoritative for this task.** A fix round hardened it after
> the blocks below were written: `parse-verdict` now anchors at column 0 so an
> echoed, indented copy of the prompt's own format example can no longer parse as
> a clean pass (it previously yielded `MERGEABLE` with all-zero counts from a
> reply containing no review at all), and `parse-fingerprints` now captures paths
> containing spaces or colons whole. The shipped test files carry four assertions
> the blocks below lack. Read
> `claude-code-plugins/hooks/pr_review/{prompt,reviewer}.clj` and
> `claude-code-plugins/test/pr_review/{prompt,reviewer}_test.clj` as built, and
> see commit `f024dcf91`.

**Files:**
- Create: `~/.nixpkgs/claude-code-plugins/hooks/pr_review/prompt.clj`
- Test: `~/.nixpkgs/claude-code-plugins/test/pr_review/prompt_test.clj`

**Interfaces:**
- Consumes: nothing. Pass number and already-twice-raised fingerprints arrive as arguments — Task 8's trigger reads them from the ledger and passes them in, so this namespace stays pure apart from consuming the hint file.
- Produces:
  - `(overlay-path repo-root)` → String, `<repo-root>/.claude/pr-review.md`
  - `(hint-path git-dir)` → String, `<git-dir>/pr-review-hint` — under the shared
    git directory, so the hint is reachable from a linked worktree
  - `(read-hint! git-dir)` → String or nil, and deletes the file
  - `(build {:core String :repo-root String :git-dir String :ctx map :pr long :pass long :draft? boolean :prior-fingerprints [String]})` → String

- [ ] **Step 1: Write the failing test**

`test/pr_review/prompt_test.clj`:

```clojure
(ns pr-review.prompt-test
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [pr-review.prompt :as prompt]))

(defn- tmp-repo
  "Returns [repo-root git-dir]. The overlay is repo-root-relative
   (`.claude/pr-review.md`, a tracked file); the hint is git-dir-relative, so
   it is reachable from a linked worktree where `<repo-root>/.git` is a file."
  []
  (let [d (str (fs/create-temp-dir {:prefix "pr-review-prompt"}))
        g (str d "/.git")]
    (fs/create-dirs g)
    [d g]))

(def ^:private ctx
  {:diff-path "/r/.git/pr-review-context/abc.diff"
   :changed-files ["src/a.clj" "test/b_test.clj"]
   :base "basesha" :sha "abc" :diff-bytes 1234})

(defn- base-args [[repo git-dir]]
  {:core "CORE_TEXT" :repo-root repo :git-dir git-dir :ctx ctx :pr 370 :pass 1
   :draft? false :prior-fingerprints []})

(deftest core-is-always-included
  (is (str/includes? (prompt/build (base-args (tmp-repo))) "CORE_TEXT")))

(deftest prompt-names-the-diff-file-and-repo-root
  (let [[r g :as repo] (tmp-repo)
        out (prompt/build (base-args repo))]
    (is (str/includes? out (:diff-path ctx)))
    (is (str/includes? out r))
    (is (str/includes? out "src/a.clj"))
    (is (str/includes? out "PR NUMBER: 370"))
    (is (str/includes? out "BASE: basesha"))
    (is (str/includes? out "HEAD: abc"))))

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
  (let [[r g :as repo] (tmp-repo)]
    (fs/create-dirs (str r "/.claude"))
    (spit (prompt/overlay-path r) "OVERLAY_TEXT")
    (is (str/includes? (prompt/build (base-args repo)) "OVERLAY_TEXT"))))

(deftest missing-overlay-degrades-silently
  (let [out (prompt/build (base-args (tmp-repo)))]
    (is (not (str/includes? out "OVERLAY")))
    (is (str/includes? out "CORE_TEXT")
        "a repo with no .claude/pr-review.md must still get a working review")))

(deftest hint-is-included-and-consumed
  (let [[r g :as repo] (tmp-repo)]
    (spit (prompt/hint-path g) "watch the retry path")
    (let [out (prompt/build (base-args repo))]
      (is (str/includes? out "watch the retry path")))
    (is (not (fs/exists? (prompt/hint-path g)))
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
  "One-shot note from agent A to the reviewer.

   Under the clone's shared git directory (pr-review.gh/git-common-dir), so
   the hint is readable from a linked worktree — where `<repo-root>/.git` is
   a file and nothing can live under it."
  [git-dir]
  (str git-dir "/pr-review-hint"))

(defn read-hint!
  "Read and delete the hint. A hint is scoped to one review; leaving it in
   place would silently steer every later pass on the PR."
  [git-dir]
  (let [p (hint-path git-dir)]
    (when (fs/exists? p)
      (let [s (str/trim (slurp p))]
        (fs/delete-if-exists p)
        (when-not (str/blank? s) s)))))

(defn- section
  [title body]
  (when-not (str/blank? (str body))
    (str "\n## " title "\n\n" body "\n")))

(defn build
  [{:keys [core repo-root git-dir ctx pr pass draft? prior-fingerprints]}]
  (let [overlay (when (fs/exists? (overlay-path repo-root))
                  (slurp (overlay-path repo-root)))
        hint    (read-hint! git-dir)
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

> **Shipped code is authoritative for this task.** A fix round hardened it after
> the blocks below were written: `parse-verdict` now anchors at column 0 so an
> echoed, indented copy of the prompt's own format example can no longer parse as
> a clean pass (it previously yielded `MERGEABLE` with all-zero counts from a
> reply containing no review at all), and `parse-fingerprints` now captures paths
> containing spaces or colons whole. The shipped test files carry four assertions
> the blocks below lack. Read
> `claude-code-plugins/hooks/pr_review/{prompt,reviewer}.clj` and
> `claude-code-plugins/test/pr_review/{prompt,reviewer}_test.clj` as built, and
> see commit `f024dcf91`.

**Files:**
- Create: `~/.nixpkgs/claude-code-plugins/hooks/pr_review/reviewer.clj`
- Test: `~/.nixpkgs/claude-code-plugins/test/pr_review/reviewer_test.clj`

**Interfaces:**
- Consumes: nothing
- Produces:
  - `denied-tools` → vector of String. **The sandbox.** `--allowedTools` is a
    pre-approval allowlist and restricts nothing — with
    `permissions.defaultMode: "bypassPermissions"` every tool is auto-approved,
    and the reviewer had Bash and wrote a file outside every repo.
    `--disallowedTools` does remove them. It is a deny list, so a tool added to
    Claude Code in future is granted by default and this list needs re-probing
    on every upgrade
  - `(claude-argv)` → vector of String, the exact `claude -p` command line:
    `--disallowedTools`, `--strict-mcp-config`, and `--allowedTools` kept for
    intent only
  - `(run! prompt repo-root opts)` → `{:exit long :out String :err String}`; `opts` honours `:spawn-fn`
  - `(parse-output out)` → `{:verdict String :counts {String long} :fingerprints [String] :body String}`. Verdict is the **last** column-0 `VERDICT:` line (`re-find` returns the first, so a reviewer restating the format unindented had that parsed as its answer); counts and finding lines are read from emphasis-stripped lines, case-insensitively
  - `(mergeable? parsed)` → boolean — verdict line MERGEABLE **and** a zero
    `[correctness/blocking]` count. No coverage clause: the spec's coverage
    condition is a judgement about one finding's content that a bare count
    cannot express
  - `(reconcile parsed)` → parsed with `:verdict` replaced by what its own counts
    support, and the contradiction stated in `:body`. This is `mergeable?` wired
    in; it had zero call sites
  - `(parse-warnings parsed)` → [String] — non-zero counts with no parseable
    `path:line` is a parse failure, not a quiet review

- [ ] **Step 1: Write the failing test**

`test/pr_review/reviewer_test.clj`:

```clojure
(ns pr-review.reviewer-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
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

(defn- counted
  "A minimal well-formed reply with `n` blocking findings claimed in the count
   block, and `verdict` on the verdict line."
  [verdict n findings]
  (str "VERDICT: " verdict " — whatever\n\n"
       "  [correctness/blocking]  " n "\n"
       "  [correctness/followup]  none\n"
       "  [coverage]              none\n"
       "  [docs-accuracy]         none\n"
       "  [style]                 none\n\n"
       findings))

;; ---------------------------------------------------------------- sandbox

(deftest argv-sandboxes-the-reviewer-with-a-deny-list
  (let [argv (reviewer/claude-argv)]
    (is (= "claude" (first argv)))
    (is (some #{"-p"} argv))
    (is (some #{"opus"} argv) "review quality is the point; do not downgrade the model")
    (testing "--disallowedTools is the only mechanism that actually removes a
              tool. --allowedTools is a pre-approval allowlist, and with
              permissions.defaultMode bypassPermissions every tool is
              auto-approved regardless — measured: that invocation had Bash
              and wrote a file outside every repo"
      (let [i (.indexOf argv "--disallowedTools")]
        (is (nat-int? i))
        (let [denied (set (str/split (nth argv (inc i)) #","))]
          (testing "a shell or a writer"
            (is (every? denied ["Bash" "Write" "Edit" "MultiEdit" "NotebookEdit"])))
          (testing "another agent to do it instead"
            (is (every? denied ["Agent" "Task" "SendMessage"])))
          (testing "a route off this machine — Artifact publishes to the web"
            (is (every? denied ["WebFetch" "WebSearch" "Artifact"
                                "PushNotification" "RemoteTrigger"
                                "ShareOnboardingGuide"])))
          (testing "a way to make work happen later"
            (is (every? denied ["CronCreate" "ScheduleWakeup" "Workflow" "Skill"])))
          (testing "a way to reach a tool that is not on this list at all"
            (is (denied "ToolSearch")))
          (testing "Read, Grep and Glob are the whole review"
            (is (not-any? denied ["Read" "Grep" "Glob"]))))))
    (testing "no MCP server the user happens to have configured: several write
              files and reach the network, and their names are
              per-installation so no deny list can enumerate them"
      (is (some #{"--strict-mcp-config"} argv)))
    (testing "--allowedTools is kept for intent; it restricts nothing"
      (let [i (.indexOf argv "--allowedTools")]
        (is (nat-int? i))
        (is (= "Read,Grep,Glob" (nth argv (inc i))))))))

(deftest run-passes-the-prompt-and-cwd-to-the-spawner
  (let [seen (atom nil)
        spawn (fn [argv prompt dir] (reset! seen {:argv argv :prompt prompt :dir dir})
                {:exit 0 :out "VERDICT: MERGEABLE — 0 follow-ups to file" :err ""})
        res (reviewer/run! "PROMPT" "/repo" {:spawn-fn spawn})]
    (is (= 0 (:exit res)))
    (is (= "PROMPT" (:prompt @seen)))
    (is (= "/repo" (:dir @seen)) "the reviewer must run in the repo it is reviewing")))

(deftest run-never-throws-even-if-the-spawner-does
  (let [spawn (fn [_ _ _] (throw (ex-info "boom" {})))
        res (reviewer/run! "PROMPT" "/repo" {:spawn-fn spawn})]
    (is (not (zero? (:exit res)))
        "a spawn failure must surface as a result, never propagate as an exception")
    (is (= "boom" (:err res)))))

;; ---------------------------------------------------------------- parsing

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

(deftest output-with-no-verdict-is-MALFORMED-not-dropped
  (let [p (reviewer/parse-output "I could not find the diff file.")]
    (is (= "MALFORMED" (:verdict p)))
    (is (= "I could not find the diff file." (:body p))
        "a reviewer that fails must surface its own words, or the pass vanishes silently")
    (is (= [] (:fingerprints p)))))

(deftest body-is-preserved-verbatim
  (is (= good-output (:body (reviewer/parse-output good-output)))
      "emphasis is stripped for parsing only; agent A must read exactly what
       the reviewer wrote"))

(deftest echoed-template-is-MALFORMED-not-a-clean-pass
  (let [echoed (->> (str/split-lines good-output)
                    (map #(str "    " %))
                    (str/join "\n"))
        p (reviewer/parse-output echoed)]
    (is (= "MALFORMED" (:verdict p))
        "an indented, echoed format example must never parse as a real verdict")
    (is (false? (reviewer/mergeable? p)))))

(deftest the-last-column-zero-verdict-wins
  (let [out (str "VERDICT: MERGEABLE — this is me restating the required format\n"
                 "\n"
                 "Now the actual review.\n"
                 "\n"
                 (counted "NOT MERGEABLE" 1
                          "1. [correctness/blocking] src/a.clj:7 — boom\n"))]
    (is (= "NOT MERGEABLE" (:verdict (reviewer/parse-output out)))
        "re-find returns the FIRST match, so a reviewer that restated the
         format unindented before reviewing had that restatement parsed as its
         answer — a third false-clean path")))

(deftest a-bolded-verdict-at-column-zero-still-counts
  (is (= "MERGEABLE"
         (:verdict (reviewer/parse-output "**VERDICT: MERGEABLE — nothing to fix**\n")))
      "emphasis is stripped before the column-0 anchor is applied, so bold
       markup does not turn a real verdict into MALFORMED"))

(deftest counts-survive-emphasis-and-capitals
  (testing "mergeable? reads the count block as the evidence that overrides
            the verdict line, so a count block that fails to parse is a false
            clean — the same defect class as an unparseable finding line"
    (let [out (str "VERDICT: MERGEABLE — looks fine\n\n"
                   "  **[Correctness/Blocking]**  2\n"
                   "  `[correctness/followup]`    None\n"
                   "  [coverage]                  none\n"
                   "  [docs-accuracy]             none\n"
                   "  [style]                     none\n")
          p (reviewer/parse-output out)]
      (is (= 2 (get (:counts p) "correctness/blocking")))
      (is (= 0 (get (:counts p) "correctness/followup")))
      (is (false? (reviewer/mergeable? p))))))

(deftest fingerprints-are-parsed-from-every-realistic-line-shape
  (testing "each of these used to yield a counted finding with an EMPTY
            fingerprint, so the one-re-raise rule could never fire for it and
            it was re-reported on every pass straight into the 10-pass cap.
            The invariant is `[category] path:line`, not the canonical
            `N. [category] path:line — text`"
    (doseq [[label line expected]
            [["canonical"
              "1. [correctness/blocking] src/retry.clj:42 — off-by-one"
              "src/retry.clj:42:correctness/blocking"]
             ["line range collapses to its first line"
              "2. [correctness/blocking] src/retry.clj:42-45 — off-by-one"
              "src/retry.clj:42:correctness/blocking"]
             ["colon instead of the em-dash"
              "3. [correctness/blocking] src/retry.clj:42: off-by-one"
              "src/retry.clj:42:correctness/blocking"]
             ["backticked path — the likeliest LLM shape"
              "4. [correctness/blocking] `src/retry.clj:42` — off-by-one"
              "src/retry.clj:42:correctness/blocking"]
             ["bolded path"
              "5. [correctness/blocking] **src/retry.clj:42** — off-by-one"
              "src/retry.clj:42:correctness/blocking"]
             ["L-prefixed line number"
              "6. [correctness/blocking] src/retry.clj:L42 — off-by-one"
              "src/retry.clj:42:correctness/blocking"]
             ["capitalised category"
              "7. [Correctness/Blocking] src/retry.clj:42 — off-by-one"
              "src/retry.clj:42:correctness/blocking"]
             ["dash bullet instead of N."
              "- [correctness/blocking] src/retry.clj:42 — off-by-one"
              "src/retry.clj:42:correctness/blocking"]
             ["asterisk bullet"
              "* [correctness/blocking] src/retry.clj:42 — off-by-one"
              "src/retry.clj:42:correctness/blocking"]
             ["paren bullet"
              "8) [correctness/blocking] src/retry.clj:42 — off-by-one"
              "src/retry.clj:42:correctness/blocking"]
             ["no bullet at all"
              "[correctness/blocking] src/retry.clj:42 — off-by-one"
              "src/retry.clj:42:correctness/blocking"]
             ["path containing a space"
              "9. [docs-accuracy] docs/My Notes.md:12 — needs a heading"
              "docs/My Notes.md:12:docs-accuracy"]
             ["path containing a colon"
              "10. [style] src/pool:v2/file.clj:34 — naming"
              "src/pool:v2/file.clj:34:style"]
             ["everything at once"
              "- [Coverage] `src/pool:v2/my file.clj:L34-40`: certifies nothing"
              "src/pool:v2/my file.clj:34:coverage"]]]
      (let [p (reviewer/parse-output (counted "NOT MERGEABLE" 1 (str line "\n")))]
        (is (= [expected] (:fingerprints p)) label)))))

(deftest the-count-block-is-not-mistaken-for-a-finding
  (is (= [] (:fingerprints (reviewer/parse-output
                            (counted "MERGEABLE" "none" ""))))
      "the count block lines carry a category in brackets but no path:line;
       reading one as a finding would invent a fingerprint out of nothing"))

;; ------------------------------------------------- verdict vs the evidence

(deftest mergeable-requires-the-verdict-line-and-a-zero-blocking-count
  (is (true? (reviewer/mergeable?
              {:verdict "MERGEABLE" :counts {"correctness/blocking" 0 "coverage" 0}})))
  (is (false? (reviewer/mergeable?
               {:verdict "MERGEABLE" :counts {"correctness/blocking" 1 "coverage" 0}}))
      "the verdict line is the reviewer's claim; the counts are the evidence")
  (is (false? (reviewer/mergeable?
               {:verdict "NOT MERGEABLE" :counts {"correctness/blocking" 0}})))
  (testing "no coverage clause: the spec and the core prompt both define
            MERGEABLE as no blocking finding and no coverage finding \"in which
            a test certifies a safety property it does not check\" — a
            judgement about one finding's content that a bare count cannot
            express. Demanding zero coverage findings outright would turn
            every benign coverage nit into a false NOT-clean"
    (is (true? (reviewer/mergeable?
                {:verdict "MERGEABLE" :counts {"correctness/blocking" 0 "coverage" 2}})))))

(deftest reconcile-overrides-a-verdict-its-own-counts-contradict
  (let [p (reviewer/reconcile
           (reviewer/parse-output
            (counted "MERGEABLE" 1 "1. [correctness/blocking] src/a.clj:7 — boom\n")))]
    (is (= "NOT MERGEABLE" (:verdict p))
        "mergeable? had zero production call sites, so a count block that
         contradicted the verdict line produced a MERGEABLE headline for agent
         A and a self-contradictory ledger row (verdict MERGEABLE, blocking 1)")
    (is (str/includes? (:body p) "count block")
        "the contradiction must be stated, not silently rewritten")))

(deftest reconcile-leaves-a-consistent-verdict-alone
  (doseq [out [(counted "MERGEABLE" "none" "")
               (counted "NOT MERGEABLE" 2 "1. [correctness/blocking] a:1 — x\n")]]
    (let [parsed (reviewer/parse-output out)]
      (is (= parsed (reviewer/reconcile parsed))
          "reconciliation must be a no-op when claim and evidence agree"))))

(deftest reconcile-passes-MALFORMED-through
  (let [p (reviewer/parse-output "the diff file was empty")]
    (is (= p (reviewer/reconcile p))
        "there is no verdict to reconcile, and an unparsed review's counts are
         all zero by construction — rewriting it to NOT MERGEABLE would claim
         a review happened")))

(deftest counted-findings-with-no-fingerprints-are-surfaced
  (let [p (reviewer/parse-output
           (counted "NOT MERGEABLE" 2
                    "1. [correctness/blocking] the retry loop is wrong\n"))
        w (reviewer/parse-warnings p)]
    (is (= 1 (count w)))
    (is (str/includes? (first w) "2 finding"))
    (is (str/includes? (first w) "one-re-raise")
        "non-zero counts with no parseable path:line is a parse failure:
         nothing carries an identity, so every finding is re-reported until
         the cap. Nothing used to notice")))

(deftest a-genuinely-clean-pass-warns-about-nothing
  (is (= [] (reviewer/parse-warnings
             (reviewer/parse-output (counted "MERGEABLE" "none" ""))))))

;; ------------------------------------------------ prompt/parser round trip

(deftest the-shipped-core-prompt-is-not-mistaken-for-a-review
  (testing "the prompt and the parser are the same contract in two files that
            drift silently — which is how the first-match verdict bug and the
            fingerprint-shape gaps both shipped. Feed the SHIPPED
            review_core.md through the parser: every VERDICT line in it is an
            indented example, so the whole document must read as MALFORMED"
    (let [core (slurp (io/resource "review_core.md"))
          p (reviewer/parse-output core)]
      (is (= "MALFORMED" (:verdict p)))
      (is (false? (reviewer/mergeable? p)))
      (is (= [] (:fingerprints p))))))

(deftest the-shipped-core-prompts-own-example-finding-lines-parse
  (testing "the other half of the same contract: the example finding lines the
            prompt tells the reviewer to copy must produce fingerprints when
            they appear under a real verdict"
    (let [core (slurp (io/resource "review_core.md"))
          examples (->> (str/split-lines core)
                        (map str/trim)
                        (filter #(re-find #"^\d+\.\s*\[" %)))]
      (is (seq examples) "fixture precondition: review_core.md shows examples")
      (doseq [line examples]
        (is (= 1 (count (:fingerprints
                         (reviewer/parse-output
                          (counted "NOT MERGEABLE" 1 (str line "\n"))))))
            (str "the prompt's own example line must parse: " line))))))
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
   of agent A's conversation history, the same machine and working tree. It is
   confined to Read, Grep and Glob — no shell, so nothing it runs can be
   rewritten by rtk and nothing it does can touch the tree."
  (:require [babashka.process :as p]
            [clojure.string :as str]))

(def categories
  ["correctness/blocking" "correctness/followup" "coverage"
   "docs-accuracy" "style"])

(def denied-tools
  "The reviewer's sandbox. THIS list is the mechanism; `--allowedTools` is
   not — see `claude-argv`.

   Grouped by what each entry would buy an escaped reviewer: a shell or a
   writer (Bash, Write, Edit, MultiEdit, NotebookEdit, Notebook*, Bash*),
   another agent to do it for it (Agent, Task, TaskStop, SendMessage,
   SendUserMessage, ListAgents), a route off this machine (WebFetch,
   WebSearch, Artifact* — Artifact publishes to the web, PushNotification,
   RemoteTrigger, Monitor, DesignSync, ShareOnboardingGuide), a way to make
   work happen later (Cron*, ScheduleWakeup, Workflow, Skill), a way to
   reach a tool not in this list at all (ToolSearch, the MCP resource
   readers), or a place to put a finding where the parser will never see it
   (ReportFindings, Task*).

   MultiEdit no longer exists in Claude Code 2.1.263 — it is kept because an
   unknown name costs one warning line on stderr, and a reintroduced editing
   tool would otherwise be granted silently."
  ["Agent" "Artifact" "ArtifactCheck" "ArtifactComments" "ArtifactData"
   "Bash" "BashOutput" "CronCreate" "CronDelete" "CronList" "DesignSync"
   "Edit" "EnterWorktree" "ExitWorktree" "KillShell" "ListAgents"
   "ListMcpResourcesTool" "Monitor" "MultiEdit" "NotebookEdit"
   "PushNotification" "ReadMcpResourceDirTool" "ReadMcpResourceTool"
   "RemoteTrigger" "ReportFindings" "ScheduleWakeup" "SendMessage"
   "SendUserMessage" "ShareOnboardingGuide" "Skill" "Task" "TaskCreate"
   "TaskGet" "TaskList" "TaskStop" "TaskUpdate" "ToolSearch" "WebFetch"
   "WebSearch" "Workflow" "Write"])

(defn claude-argv
  []
  ["claude" "-p"
   "--model" "opus"
   ;; `--disallowedTools` is the load-bearing mechanism and the only one.
   ;;
   ;; `--allowedTools` is a PRE-APPROVAL allowlist, not a tool restriction:
   ;; it says which calls skip the permission prompt, not which tools exist.
   ;; With `permissions.defaultMode: "bypassPermissions"` in the user's
   ;; ~/.claude/settings.json — and no allow/deny/ask rules at all — every
   ;; tool is auto-approved regardless. Measured: this exact invocation with
   ;; only `--allowedTools "Read,Grep,Glob"` still had Bash in its function
   ;; list and wrote a file outside every repo. `--permission-mode default`
   ;; does not close it either; the write still happened. The deny list does:
   ;; the tools vanish from the function list and the write does not happen.
   ;;
   ;; This is a deny list, so it is not airtight. A tool added to Claude Code
   ;; in a future version is granted to the reviewer by default, and only
   ;; appears here once someone notices. `denied-tools` needs re-checking
   ;; against a live probe on every Claude Code upgrade.
   "--disallowedTools" (str/join "," denied-tools)
   ;; No MCP server the user happens to have configured — several of them
   ;; write files and reach the network, and none of them are in the deny
   ;; list because their names are per-installation.
   "--strict-mcp-config"
   ;; Kept for intent, and harmless: it documents the three tools the review
   ;; is supposed to need. It restricts nothing.
   "--allowedTools" "Read,Grep,Glob"])

(defn- default-spawn
  [argv prompt dir]
  (let [{:keys [exit out err]} (p/sh argv {:dir dir :in prompt})]
    {:exit exit :out (or out "") :err (or err "")}))

(defn run!
  "Run the reviewer with `prompt` on stdin, in `repo-root`.
   Never throws: a spawn failure becomes a non-zero exit with the message in
   :err, so the caller can still tell the author what happened."
  [prompt repo-root opts]
  (let [spawn (or (:spawn-fn opts) default-spawn)]
    (try (spawn (claude-argv) prompt repo-root)
         (catch Exception e {:exit 127 :out "" :err (str (ex-message e))}))))

(defn- strip-emphasis
  "Remove inline markdown emphasis so one parser handles every shape a model
   actually emits. A backticked path (`` `src/retry.clj:42` ``) is the single
   likeliest reviewer shape, and a bolded one is next; both used to yield a
   counted finding with no fingerprint at all, which made the one-re-raise
   rule permanently unable to fire for it."
  [line]
  (str/replace line #"[`*]" ""))

(defn- normalized-lines
  [out]
  (mapv strip-emphasis (str/split-lines out)))

(defn- parse-verdict
  "The LAST line that starts a verdict at column 0.

   Two rules, each closing a different false-clean path.

   Column 0: leading whitespace means the line is quoted or indented — an
   echoed copy of the core prompt's own format example, say — not a real
   verdict. Without the anchor an echoed template parses as a clean pass,
   worse than the reviewer failing to run at all because it emits a positive
   signal for a review that never happened.

   Last, not first: a reviewer that restates the required format unindented
   before reviewing anything used to have that restatement parsed as its
   answer. The real verdict is the one it ends on."
  [lines]
  (->> lines
       (keep #(second (re-find #"^VERDICT:\s*(MERGEABLE|NOT MERGEABLE)" %)))
       last))

(defn- parse-counts
  "Read the per-category count block. \"none\" means 0 — a missing key and a
   zero count must not be confusable, or a clean pass reads as an unparsed one.

   Case-insensitive over emphasis-stripped lines for the same reason
   `parse-fingerprints` is: a bolded or capitalised count block that parses as
   all-zero is a false clean, since `mergeable?` reads these counts as the
   evidence that overrides the verdict line."
  [lines]
  (into {}
        (map (fn [cat]
               (let [re (re-pattern (str "(?i)^\\s*\\[" cat "\\]\\s+(none|\\d+)"))
                     n  (->> lines (keep #(second (re-find re %))) first)]
                 [cat (cond (nil? n) 0
                            (= "none" (str/lower-case n)) 0
                            :else (parse-long n))])))
        categories))

(def ^:private finding-line-re
  "One finding line, in every shape a model plausibly writes it.

   The invariant is `[category] <path>:<line>` — not the canonical
   `N. [category] path:line — text` the prompt asks for. Anything narrower
   has to enumerate shapes, and each shape it misses is a counted finding
   with an empty fingerprint: invisible to the one-re-raise rule, so
   re-reported on every pass straight into the 10-pass cap.

   Deliberately permissive about everything that is not the invariant:
     - any bullet, or none: `1.`  `2)`  `-`  `*`  `+`  `•`
     - any category case: `[Correctness/Blocking]`
     - `L`-prefixed and ranged lines: `:L42`  `:42-45`
     - any separator after the line number: em-dash, colon, comma, EOL
   Emphasis is stripped before this runs, so backticked and bolded paths
   arrive bare.

   The path is non-greedy and anchors on the first `:<digits>` boundary that
   is actually followed by a separator, so a path holding a space or an
   internal colon (`src/pool:v2/file.clj:34`) is still captured whole.

   A range collapses to its first line: a reviewer that writes `42` one pass
   and `42-45` the next must produce the same fingerprint, or the rule cannot
   see the re-raise."
  #"(?i)^\s*(?:\d+[.)]|[-+•])?\s*\[([a-z][a-z/-]*)\]\s+(.+?):L?(\d+)(?:-\d+)?(?=[\s:,;)\]]|$)")

(defn- parse-fingerprints
  "Stable identity for a finding: `file:line:category`, lower-cased category."
  [lines]
  (->> lines
       (keep (fn [line]
               (when-let [[_ cat path ln] (re-find finding-line-re line)]
                 (str path ":" ln ":" (str/lower-case cat)))))
       distinct
       vec))

(defn parse-output
  [out]
  (let [out   (or out "")
        lines (normalized-lines out)]
    (if-let [v (parse-verdict lines)]
      {:verdict v
       :counts (parse-counts lines)
       :fingerprints (parse-fingerprints lines)
       :body out}
      {:verdict "MALFORMED"
       :counts (zipmap categories (repeat 0))
       :fingerprints []
       :body (str/trim out)})))

(defn mergeable?
  "MERGEABLE means exactly: the reviewer said so, and its own count block
   agrees there is no [correctness/blocking] finding. The verdict line is the
   reviewer's claim; the counts are the evidence, and the evidence wins.

   No coverage clause, deliberately. The spec and the core prompt both define
   MERGEABLE as no blocking finding and no coverage finding \"in which a test
   certifies a safety property it does not check\" — a judgement about one
   finding's content, which a bare `[coverage] N` count cannot express. This
   function used to demand zero coverage findings outright, which would have
   turned any benign coverage nit into a false NOT-clean the moment it was
   wired in. The narrow clause stays where it can be judged: in the
   reviewer's own verdict line, which this function still requires."
  [{:keys [verdict counts]}]
  (and (= "MERGEABLE" verdict)
       (zero? (get counts "correctness/blocking" 0))))

(defn reconcile
  "Replace the reviewer's claimed verdict with the one its own counts support,
   and say so in the body when they disagreed.

   This is `mergeable?` wired in. Before it was, `mergeable?` had zero
   production call sites: any output whose count block contradicted its
   verdict line — `VERDICT: MERGEABLE` over `[correctness/blocking] 1` —
   produced a MERGEABLE headline for agent A and a self-contradictory ledger
   row, and the skill merged on it.

   MALFORMED is passed through: there is no verdict to reconcile, and the
   count block of an unparsed review is all zeros by construction."
  [parsed]
  (if (= "MALFORMED" (:verdict parsed))
    parsed
    (let [effective (if (mergeable? parsed) "MERGEABLE" "NOT MERGEABLE")]
      (if (= effective (:verdict parsed))
        parsed
        (assoc parsed
               :verdict effective
               :body (str (:body parsed)
                          "\n\npr-review-loop: the reviewer's verdict line said "
                          (:verdict parsed) " while its own count block reported "
                          (get (:counts parsed) "correctness/blocking" 0)
                          " [correctness/blocking] finding(s). Recorded as "
                          effective " — the counts are the evidence."))))))

(defn parse-warnings
  "Diagnostics about the parse itself, for the wake message.

   Non-zero counts with no fingerprints at all is a parse failure, not a
   quiet review: the findings exist, but none of them carries an identity, so
   the one-re-raise rule can never fire for any of them and every one is
   re-reported until the cap. Nothing used to notice this."
  [{:keys [counts fingerprints]}]
  (let [total (reduce + 0 (vals counts))]
    (cond-> []
      (and (pos? total) (empty? fingerprints))
      (conj (str "pr-review-loop parse warning: the count block reports "
                 total " finding(s) but not one finding line carried a"
                 " parseable `path:line`, so none of them has a fingerprint"
                 " and the one-re-raise rule cannot track any of them."
                 " Check the reviewer's finding-line format against"
                 " review_core.md.")))))
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
  - `(decide input opts)` → `{:action :silent|:review|:cap-reached :reason String …}` — pure decision, no side effects. Reads the ledger exactly **once** and hands the result to `ledger`'s pure predicates; a `:review` decision carries `:git-dir`, the resolved shared git directory. `opts` honours `:git-dir-fn`
  - `(-main & args)` → reads hook JSON on stdin, exits 0 or 2
  - `review!` returns an `:exit` of 0 or 2 on **every** branch, `lock/acquire!`
    included. It used to be evaluated in the `case` head, outside the try, so a
    worktree made babashka exit 1 on every push; `release!` in the `finally` is
    wrapped for the same reason

- [ ] **Step 1: Write the failing test**

`test/pr_review/trigger_test.clj`:

```clojure
(ns pr-review.trigger-test
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [pr-review.ledger :as ledger]
            [pr-review.lock :as lock]
            [pr-review.trigger :as trigger]))

(defn- tmp-repo
  "Returns [repo-root git-dir] for an ordinary clone."
  []
  (let [d (str (fs/create-temp-dir {:prefix "pr-review-trigger"}))
        g (str d "/.git")]
    (fs/create-dirs g)
    [d g]))

(defn- opts
  "Wire every collaborator to a stub so `decide` is exercised in isolation.

   :branch is read via `contains?`, not `(or branch \"feat/x\")`: the latter
   cannot distinguish an explicit `:branch nil` (simulating detached HEAD)
   from the key being omitted (the normal-branch default), so a caller
   passing `:branch nil` would silently get \"feat/x\" back instead of nil."
  [repo git-dir & {:keys [pr sha] :as kvs}]
  {:repo-root-fn (constantly repo)
   :git-dir-fn   (constantly git-dir)
   :branch-fn    (constantly (if (contains? kvs :branch) (:branch kvs) "feat/x"))
   :head-sha-fn  (constantly (or sha "headsha"))
   :open-pr-fn   (constantly pr)})

(defn- a-pr [n & {:keys [draft?]}]
  {:number n :isDraft (boolean draft?) :baseRefName "main"})

(defn- row
  [pr sha n & {:keys [fingerprints verdict]}]
  {:pr pr :sha sha :pass n :verdict (or verdict "NOT_MERGEABLE")
   :blocking 1 :followup 0 :coverage 0 :fingerprints (vec fingerprints)})

(defn- clean-reply
  "A well-formed reviewer reply with no findings."
  []
  (str "VERDICT: MERGEABLE — nothing to fix\n\n"
       "  [correctness/blocking]  none\n"
       "  [correctness/followup]  none\n"
       "  [coverage]              none\n"
       "  [docs-accuracy]         none\n"
       "  [style]                 none\n"))

(defn- review-opts
  "opts for `review!`: a stubbed diff and a stubbed reviewer, so nothing
   shells out to a real `claude -p`."
  [& {:keys [out exit err pid spawn-fn]}]
  (cond-> {:merge-base-fn (constantly "basesha")
           :diff-fn (constantly "diff --git a/a b/a\n")
           :pid (or pid 4242)
           :spawn-fn (or spawn-fn
                         (fn [_ _ _] {:exit (or exit 0)
                                      :out (or out (clean-reply))
                                      :err (or err "")}))}
    true identity))

;; ----------------------------------------------------------------- decide

(deftest no-repo-is-silent
  (let [d (trigger/decide {:cwd "/tmp"}
                          (assoc (opts nil nil) :repo-root-fn (constantly nil)))]
    (is (= :silent (:action d)))
    (is (str/includes? (:reason d) "not a git repo"))))

(deftest detached-head-is-silent
  (let [[r g] (tmp-repo)
        d (trigger/decide {:cwd r} (opts r g :branch nil))]
    (is (= :silent (:action d)))
    (is (str/includes? (:reason d) "detached HEAD")
        "must name detached HEAD specifically: this fixture's :open-pr-fn is
         already nil, so deleting the detached-HEAD branch of `decide` would
         still fall through to :silent via the no-open-PR branch, and this
         test would not catch it without a :reason assertion")))

(deftest no-open-pr-is-silent
  (let [[r g] (tmp-repo)
        d (trigger/decide {:cwd r} (opts r g :pr nil))]
    (is (= :silent (:action d)))
    (is (str/includes? (:reason d) "no open PR")
        "a push to a branch with no PR is the human's normal workflow, not an error")))

(deftest open-pr-yields-a-review-with-pass-one
  (let [[r g] (tmp-repo)
        d (trigger/decide {:cwd r} (opts r g :pr (a-pr 370)))]
    (is (= :review (:action d)))
    (is (= 370 (:pr d)))
    (is (= 1 (:pass d)))
    (is (= "headsha" (:sha d)))
    (is (= "main" (:base-ref d)))
    (is (false? (:draft? d)))
    (is (= g (:git-dir d))
        "the decision carries the resolved git dir; every state module reads
         it instead of assuming <repo-root>/.git")))

(deftest drafts-are-reviewed
  (let [[r g] (tmp-repo)
        d (trigger/decide {:cwd r} (opts r g :pr (a-pr 9 :draft? true)))]
    (is (= :review (:action d)))
    (is (true? (:draft? d)))))

(deftest pass-number-comes-from-the-ledger
  (let [[r g] (tmp-repo)]
    (doseq [n [1 2]] (ledger/append-pass! g (row 370 (str n) n)))
    (is (= 3 (:pass (trigger/decide {:cwd r} (opts r g :pr (a-pr 370))))))))

(deftest cap-stops-the-loop
  (let [[r g] (tmp-repo)]
    (doseq [n (range 1 (inc ledger/max-passes))]
      (ledger/append-pass! g (row 370 (str n) n)))
    (let [d (trigger/decide {:cwd r} (opts r g :pr (a-pr 370)))]
      (is (= :cap-reached (:action d)))
      (is (str/includes? (:reason d) "10")))))

(deftest an-already-reviewed-sha-is-silent
  (testing "repeat pushes of one commit are ordinary — `git push` twice,
            `--tags`, `--dry-run` and `--delete` all match
            `Bash(git push:*)`. Counting rows and never consulting the :sha
            the ledger faithfully records re-reviewed the same commit on every
            one of them, one cap slot each"
    (let [[r g] (tmp-repo)]
      (ledger/append-pass! g (row 370 "headsha" 1))
      (let [d (trigger/decide {:cwd r} (opts r g :pr (a-pr 370)))]
        (is (= :silent (:action d)))
        (is (str/includes? (:reason d) "headsha")
            "the reason must name the SHA that was already reviewed"))
      (testing "a new commit on the same PR still gets reviewed"
        (is (= :review (:action (trigger/decide
                                 {:cwd r} (opts r g :pr (a-pr 370) :sha "newsha")))))))))

(deftest twice-raised-followup-fingerprints-are-carried-into-the-decision
  (let [[r g] (tmp-repo)
        fp "src/a.clj:1:correctness/followup"]
    (doseq [n [1 2]]
      (ledger/append-pass! g (row 370 (str n) n :fingerprints [fp] :verdict "MERGEABLE")))
    (is (= [fp] (:prior-fingerprints
                 (trigger/decide {:cwd r} (opts r g :pr (a-pr 370) :sha "s3")))))))

(deftest a-blocking-finding-is-never-put-on-the-do-not-re-raise-list
  (testing "the filter used to key on the raise count alone, and the prompt
            then told the reviewer not to report those again. Two pushes that
            do not close a blocking defect, line number unchanged: pass 3
            reports MERGEABLE and the skill merges broken code"
    (let [[r g] (tmp-repo)
          blocking "src/a.clj:1:correctness/blocking"
          coverage "src/a.clj:2:coverage"
          followup "src/a.clj:3:correctness/followup"]
      (doseq [n [1 2]]
        (ledger/append-pass!
         g (row 370 (str n) n :fingerprints [blocking coverage followup])))
      (let [prior (:prior-fingerprints
                   (trigger/decide {:cwd r} (opts r g :pr (a-pr 370) :sha "s3")))]
        (is (= [followup] prior))
        (is (not-any? #{blocking} prior)
            "a blocking finding present after two passes has not been fixed")
        (is (not-any? #{coverage} prior))))))

(deftest decide-reads-the-ledger-once
  (testing "the one-re-raise filter used to call a per-fingerprint helper
            that re-slurped the whole ledger, on top of separate reads for
            the cap and the pass number: 138 full file reads for one decision
            at nine passes and fifteen findings"
    (let [[r g] (tmp-repo)
          fps (mapv #(str "src/f" % ".clj:1:style") (range 15))]
      (doseq [n (range 1 10)]
        (ledger/append-pass! g (row 370 (str n) n :fingerprints fps)))
      (let [reads (atom 0)
            orig  slurp]
        (with-redefs [slurp (fn [& args] (swap! reads inc) (apply orig args))]
          (is (= :review (:action (trigger/decide
                                   {:cwd r} (opts r g :pr (a-pr 370) :sha "fresh"))))))
        (is (= 1 @reads)
            (str "one decision must cost exactly one read of the ledger; got "
                 @reads))))))

;; ---------------------------------------------------------------- messages

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
      (is (str/includes? msg "BODY"))
      (is (str/includes? msg (str (fs/file-name "/r") " PR #370"))
          "the repo identifier must appear right before the PR number — a
           regression that dropped repo-root from the message would still
           satisfy every assertion above it"))))

(deftest findings-message-carries-parse-warnings
  (let [msg (trigger/findings-message
             {:repo-root "/r" :pr 1 :pass 1}
             {:verdict "NOT MERGEABLE" :body "BODY" :counts {}}
             ["PARSE WARNING TEXT"])]
    (is (str/includes? msg "PARSE WARNING TEXT")
        "a review whose findings carry no fingerprints cannot converge; agent
         A has to be told")))

;; --------------------------------------------------------------- review!

(deftest a-completed-review-records-one-pass-and-wakes-the-session
  (let [[r g] (tmp-repo)
        d {:repo-root r :git-dir g :pr 370 :pass 1 :sha "headsha"
           :base-ref "main" :draft? false :prior-fingerprints []}
        result (#'trigger/review! d (review-opts))]
    (is (= 2 (:exit result)))
    (is (= ["headsha"] (mapv :sha (ledger/read-passes g 370))))
    (is (= "MERGEABLE" (:verdict (first (ledger/read-passes g 370)))))
    (is (nil? (lock/read-lock g)) "the lock is released on the success path")))

(deftest the-ledger-row-records-the-reconciled-verdict-not-the-claimed-one
  (testing "mergeable? had zero production call sites: any output whose count
            block contradicted its verdict line yielded a MERGEABLE headline
            and a self-contradictory ledger row (verdict MERGEABLE,
            blocking 1)"
    (let [[r g] (tmp-repo)
          contradictory (str "VERDICT: MERGEABLE — nothing to fix\n\n"
                             "  [correctness/blocking]  1\n"
                             "  [correctness/followup]  none\n"
                             "  [coverage]              none\n"
                             "  [docs-accuracy]         none\n"
                             "  [style]                 none\n\n"
                             "1. [correctness/blocking] src/a.clj:7 — boom\n")
          d {:repo-root r :git-dir g :pr 1 :pass 1 :sha "s" :base-ref "main"
             :draft? false :prior-fingerprints []}
          result (#'trigger/review! d (review-opts :out contradictory))]
      (is (= 2 (:exit result)))
      (is (str/includes? (:message result) "NOT MERGEABLE")
          "the headline agent A reads must be the reconciled verdict")
      (is (= "NOT MERGEABLE" (:verdict (first (ledger/read-passes g 1))))
          "and so must the ledger row, or the next pass reads a clean history")
      (is (= 1 (:blocking (first (ledger/read-passes g 1))))))))

(deftest unresolved-base-ref-skips-the-reviewer-and-the-ledger
  (let [[r g] (tmp-repo)
        d {:repo-root r :git-dir g :pr 370 :pass 1 :sha "headsha" :base-ref "main"
           :draft? false :prior-fingerprints []}
        ;; :spawn-fn is a safety net, not the point of the test: if a
        ;; regression ever let review! reach the reviewer on this path, this
        ;; stub keeps the test from shelling out to a real `claude -p`.
        result (#'trigger/review! d (assoc (review-opts)
                                           :merge-base-fn (constantly nil)
                                           :diff-fn (constantly nil)))]
    (is (= 2 (:exit result)))
    (is (str/includes? (:message result) "main")
        "the diagnostic must name the unresolved base ref")
    (is (str/includes? (:message result) "git fetch origin main")
        "the diagnostic must give the concrete command that fixes it")
    (is (empty? (ledger/read-passes g 370))
        "no findings were produced; spending one of the ten cap slots on a
         pass that never reviewed anything would let a PR reach \"cap
         reached\" without a single real review")))

(deftest a-crashed-reviewer-does-not-consume-a-cap-slot
  (testing "six pushes against an expired token wrote six MALFORMED rows and,
            with four real passes, permanently exhausted the PR's budget. The
            unresolved-base-ref path already refuses to record a pass that
            reviewed nothing; the treatment must be identical"
    (let [[r g] (tmp-repo)
          d {:repo-root r :git-dir g :pr 370 :pass 1 :sha "headsha"
             :base-ref "main" :draft? false :prior-fingerprints []}
          result (#'trigger/review! d (review-opts :exit 1 :out ""
                                                   :err "OAuth token has expired"))]
      (is (= 2 (:exit result)) "agent A must still be woken")
      (is (str/includes? (:message result) "OAuth token has expired")
          "the real diagnosis is in the reviewer's stderr and is otherwise unread")
      (is (str/includes? (:message result) "exited 1")
          "the exit code must be named")
      (is (str/includes? (:message result) "did not consume")
          "and the message must say no slot was spent, because none was")
      (is (empty? (ledger/read-passes g 370))))))

(deftest a-malformed-reply-does-not-consume-a-cap-slot
  (let [[r g] (tmp-repo)
        d {:repo-root r :git-dir g :pr 370 :pass 1 :sha "headsha"
           :base-ref "main" :draft? false :prior-fingerprints []}
        result (#'trigger/review! d (review-opts :out "I could not read the diff"))]
    (is (= 2 (:exit result)))
    (is (str/includes? (:message result) "MALFORMED"))
    (is (str/includes? (:message result) "I could not read the diff"))
    (is (empty? (ledger/read-passes g 370))
        "a reply with no verdict produced no findings, so it buys no slot")))

(deftest a-superseded-trigger-goes-quiet-and-records-nothing
  (testing "kill-reviewers! kills the reviewer child, not the trigger, so the
            loser returns from a reviewer that was SIGTERMed mid-answer.
            Recording that would spend a slot and wake agent A with findings
            for a SHA that is already stale"
    (let [[r g] (tmp-repo)
          d {:repo-root r :git-dir g :pr 370 :pass 1 :sha "old"
             :base-ref "main" :draft? false :prior-fingerprints []}
          ;; A newer push takes the lock while our reviewer is running.
          steal (fn [_ _ _]
                  (lock/acquire! g {:pr 370 :sha "new"} {:pid 9999})
                  {:exit 143 :out "" :err "terminated"})
          result (#'trigger/review! d (review-opts :spawn-fn steal))]
      (is (= 0 (:exit result)) "silence, not a wake")
      (is (nil? (:message result)))
      (is (empty? (ledger/read-passes g 370)))
      (is (= 9999 (:pid (lock/read-lock g)))
          "and the loser must not have deleted the winner's lock record"))))

(deftest a-duplicate-push-is-silent
  (let [[r g] (tmp-repo)
        self (.pid (java.lang.ProcessHandle/current))]
    (spit (lock/lock-path g)
          (str "{\"pid\":" self ",\"pr\":370,\"sha\":\"headsha\",\"started\":1}"))
    (let [d {:repo-root r :git-dir g :pr 370 :pass 1 :sha "headsha"
             :base-ref "main" :draft? false :prior-fingerprints []}
          result (#'trigger/review! d (review-opts :pid 1))]
      (is (= 0 (:exit result)))
      (is (empty? (ledger/read-passes g 370))))))

;; -------------------------------------------------- the exit-code contract

(deftest review-exit-is-always-zero-or-two
  (testing "the one property this module exists to guarantee, and it had no
            test. Any other exit code makes Claude Code print `Failed with
            non-blocking status code:` and the review pass is silently lost"
    (let [[r g] (tmp-repo)
          d {:repo-root r :git-dir g :pr 370 :pass 1 :sha "headsha"
             :base-ref "main" :draft? false :prior-fingerprints []}
          check (fn [label thunk]
                  (is (contains? #{0 2} (:exit (thunk))) label))]
      (check "clean review" #(#'trigger/review! d (review-opts)))
      (check "reviewer crash" #(#'trigger/review! d (review-opts :exit 127 :err "no claude")))
      (check "malformed reply" #(#'trigger/review! d (review-opts :out "nope")))
      (check "unresolved base ref"
             #(#'trigger/review! d (assoc (review-opts) :diff-fn (constantly nil)
                                          :merge-base-fn (constantly nil))))
      (check "throwing lock/acquire!"
             (fn [] (with-redefs [lock/acquire! (fn [& _] (throw (ex-info "flock exploded" {})))]
                      (#'trigger/review! d (review-opts)))))
      (check "throwing lock/release!"
             (fn [] (with-redefs [lock/release! (fn [& _] (throw (ex-info "release exploded" {})))]
                      (#'trigger/review! d (review-opts)))))
      (check "throwing context/build!"
             (fn [] (#'trigger/review!
                     d (assoc (review-opts)
                              :diff-fn (fn [& _] (throw (ex-info "git exploded" {})))))))
      (check "throwing ledger/append-pass!"
             (fn [] (with-redefs [ledger/append-pass! (fn [& _] (throw (ex-info "disk full" {})))]
                      (#'trigger/review! d (review-opts))))))))

(deftest a-throwing-acquire-still-wakes-the-session-with-the-diagnosis
  (let [[r g] (tmp-repo)
        d {:repo-root r :git-dir g :pr 370 :pass 1 :sha "headsha"
           :base-ref "main" :draft? false :prior-fingerprints []}
        result (with-redefs [lock/acquire! (fn [& _] (throw (ex-info "flock exploded" {})))]
                 (#'trigger/review! d (review-opts)))]
    (is (= 2 (:exit result))
        "acquire! used to be evaluated in the `case` head, OUTSIDE the try, so
         a throw there escaped review! and -main and babashka exited 1")
    (is (str/includes? (:message result) "flock exploded"))
    (is (str/includes? (:message result) "PR #370"))))

(deftest a-throwing-release-does-not-swallow-a-completed-review
  (let [[r g] (tmp-repo)
        d {:repo-root r :git-dir g :pr 370 :pass 1 :sha "headsha"
           :base-ref "main" :draft? false :prior-fingerprints []}
        result (with-redefs [lock/release! (fn [& _] (throw (ex-info "release exploded" {})))]
                 (#'trigger/review! d (review-opts)))]
    (is (= 2 (:exit result)))
    (is (str/includes? (:message result) "MERGEABLE")
        "a throw out of a `finally` replaces the value the body already
         computed, so an unwrapped release! discards a completed review's
         findings and reports a crash instead")
    (is (= 1 (count (ledger/read-passes g 370))))))

;; ------------------------------------------------------ worktree end to end

(deftest a-worktree-whose-dot-git-is-a-file-still-reviews
  (testing "the fixture no test had. <repo-root>/.git is a FILE in a linked
            worktree, so fs/create-dirs on it throws
            FileAlreadyExistsException — and lock/acquire! used to be
            evaluated outside review!'s try, so that throw escaped -main and
            babashka exited 1 on every push from a worktree. Before that,
            read-passes found no ledger there at all, so neither the cap nor
            the re-raise rule ever engaged"
    (let [tmp  (str (fs/create-temp-dir {:prefix "pr-review-wt"}))
          main (str tmp "/main")
          wt   (str tmp "/wt")
          git! (fn [dir & args]
                 (let [{:keys [exit err]} (p/sh (into ["git"] args) {:dir dir})]
                   (when-not (zero? exit)
                     (throw (ex-info (str "fixture git failed: " args " " err) {})))))]
      (fs/create-dirs main)
      (git! main "init" "-q")
      (git! main "config" "user.email" "t@t.t")
      (git! main "config" "user.name" "t")
      (spit (str main "/f") "hi")
      (git! main "add" "f")
      (git! main "commit" "-qm" "init")
      (git! main "worktree" "add" "-q" wt "-b" "feat")
      (is (fs/regular-file? (str wt "/.git"))
          "fixture precondition: a linked worktree's .git is a file")

      (let [d (trigger/decide
               {:cwd wt}
               {:repo-root-fn (constantly wt)
                :branch-fn (constantly "feat")
                :head-sha-fn (constantly "wtsha")
                :open-pr-fn (constantly (a-pr 42))})]
        (is (= :review (:action d)))
        (is (fs/directory? (:git-dir d))
            "the resolved git dir must be a real directory")
        (is (= (str (fs/real-path (str main "/.git")))
               (str (fs/real-path (:git-dir d))))
            "one ledger, one lock and one context dir per repository — a
             worktree-local pair would make the cap and the lock meaningless
             across worktrees")

        (let [result (#'trigger/review! d (review-opts))]
          (is (= 2 (:exit result))
              "and the whole pass must complete without a throw escaping to a
               non-2 exit")
          (is (= ["wtsha"] (mapv :sha (ledger/read-passes (:git-dir d) 42)))
              "the pass lands in the shared ledger, visible to every worktree")
          (is (fs/exists? (str (:git-dir d) "/pr-review-context/wtsha.diff"))))))))
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
   `Failed with non-blocking status code:` notice and the pass is lost.

   That contract is why every filesystem-touching call below sits inside
   `review!`'s try, `lock/acquire!` included. A worktree used to make
   `acquire!` throw from the `case` head — outside the try — and babashka
   exited 1 on every push."
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

(defn- resolve-git-dir
  "The clone's shared git directory, or `<repo-root>/.git` when git cannot
   say. Resolved once per decision and threaded into every state module: all
   of them used to assume `<repo-root>/.git`, which is a *file* in a linked
   worktree."
  [repo-root opts]
  (or ((or (:git-dir-fn opts) #(gh/git-common-dir repo-root opts)))
      (str repo-root "/.git")))

(defn decide
  "Pure decision from the hook input. No side effects, no spawning.

   Reads the ledger exactly once and hands the result to ledger's pure
   predicates."
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
              (let [pr-num  (:number pr)
                    sha     ((or (:head-sha-fn opts) #(gh/head-sha repo-root opts)))
                    git-dir (resolve-git-dir repo-root opts)
                    passes  (ledger/read-passes git-dir pr-num)]
                (cond
                  ;; Idempotence, before the cap: repeat pushes of one commit
                  ;; are ordinary (`git push` twice, `--tags`, `--dry-run`,
                  ;; `--delete` all match `Bash(git push:*)`), and re-reviewing
                  ;; an already-reviewed SHA tells agent A nothing new while
                  ;; spending a cap slot for it.
                  (ledger/reviewed-sha? passes sha)
                  {:action :silent
                   :reason (str "PR #" pr-num " already has a recorded pass at "
                                sha)}

                  (ledger/cap-reached? passes)
                  {:action :cap-reached
                   :repo-root repo-root :git-dir git-dir :pr pr-num
                   :reason (str "review cap of " ledger/max-passes
                                " passes reached for PR #" pr-num)}

                  :else
                  {:action :review
                   :repo-root repo-root
                   :git-dir git-dir
                   :pr pr-num
                   :pass (ledger/next-pass-number passes)
                   :sha sha
                   :base-ref (:baseRefName pr)
                   :draft? (boolean (:isDraft pr))
                   :prior-fingerprints (ledger/suppressed-fingerprints passes)})))))))))

(defn findings-message
  "The text agent A will see. The harness prefixes it with a fixed, unhelpful
   wrapper and ignores rewakeMessage for third-party plugins, so this string
   has to introduce itself."
  ([d parsed] (findings-message d parsed nil))
  ([{:keys [repo-root pr pass]} parsed warnings]
   (str "pr-review-loop — " (fs/file-name repo-root)
        " PR #" pr ", pass " pass ": " (:verdict parsed) "\n\n"
        (:body parsed)
        (when (seq warnings)
          (str "\n\n" (str/join "\n" warnings)))
        "\n\nNext: use the pr-review-loop skill. Verify each"
        " [correctness/blocking] finding against the source before fixing it.")))

(defn- unresolved-base-message
  "Names the unresolved ref and the exact fix, so agent A does not have to
   guess why a branch with a real diff came back with nothing to say."
  [{:keys [repo-root pr pass base-ref]}]
  (str "pr-review-loop — " (fs/file-name repo-root)
       " PR #" pr ", pass " pass
       ": could not diff against base ref \"" base-ref "\" — this clone likely"
       " never fetched it, so the diff command itself failed rather than"
       " finding no changes. Fix: `git fetch origin " base-ref "`, then push"
       " again."))

(defn- failed-review-message
  "A review that produced no findings still has to wake agent A — the real
   diagnosis is sitting unread in the reviewer's stderr — but it must say
   plainly that no slot was spent, because none was."
  [{:keys [repo-root pr pass]} parsed res]
  (str "pr-review-loop — " (fs/file-name repo-root)
       " PR #" pr ", pass " pass ": review did not complete ("
       (:verdict parsed) ")"
       "\n\nreviewer process exited " (:exit res) ": " (:err res)
       (when-not (str/blank? (str (:body parsed)))
         (str "\n\n" (:body parsed)))
       "\n\nNo ledger row was written, so this attempt did not consume one of"
       " the " ledger/max-passes " review slots for PR #" pr
       ". Fix the cause and push again."))

(defn- crash-message
  [{:keys [repo-root pr pass]} e]
  (str "pr-review-loop — " (fs/file-name repo-root)
       " PR #" pr ", pass " pass " crashed: " (ex-message e)))

(defn- release-quietly!
  "release! reaches the filesystem — flock/with-file-lock creates its guard
   file and parent — so it can throw. A throw out of a `finally` replaces the
   value the body already computed, so an unwrapped release! is a second
   route to losing a completed review, and (before `review!` had an outer
   try) to a non-2 exit. A failed release is self-healing anyway: the record
   names a pid, and acquire! treats a dead holder's lock as free."
  [git-dir opts]
  (try (lock/release! git-dir opts) (catch Exception _ nil)))

(defn- run-review!
  "The reviewer pass proper, with the lock already held."
  [{:keys [repo-root git-dir pr pass sha base-ref draft? prior-fingerprints] :as d}
   opts]
  (let [ctx (context/build! repo-root git-dir
                            {:pr pr :sha sha :base-ref base-ref} opts)]
    (if (:diff-failed? ctx)
      ;; The diff command itself failed — almost always an unresolved base
      ;; ref. A 0-byte diff here looks exactly like a real empty one, so
      ;; spawning the reviewer would have it correctly report "nothing to
      ;; review" and the loop would record a false MERGEABLE. Refuse to
      ;; review, and refuse to spend a ledger slot on a pass that reviewed
      ;; nothing — the PR would still owe a real review even after the cap.
      {:exit 2 :message (unresolved-base-message d)}
      (let [text   (prompt/build {:core (core-prompt)
                                  :repo-root repo-root :git-dir git-dir
                                  :ctx ctx :pr pr :pass pass :draft? draft?
                                  :prior-fingerprints prior-fingerprints})
            res    (reviewer/run! text repo-root opts)
            ;; reconcile is mergeable? wired in: a count block that
            ;; contradicts the verdict line loses, here, once, so both the
            ;; headline and the ledger row carry the same reconciled verdict.
            parsed (reviewer/reconcile (reviewer/parse-output (:out res)))]
        (cond
          ;; A newer push superseded this trigger and killed its reviewer
          ;; mid-answer. Recording that truncated output would spend a slot
          ;; and wake agent A with findings for a SHA that is already stale.
          (lock/superseded? git-dir opts)
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
              (context/prune! git-dir context-keep)
              {:exit 2 :message (findings-message
                                 d parsed (reviewer/parse-warnings parsed))}))))))

(defn- review!
  "Always returns an :exit of 0 or 2. Nothing inside — acquire!, the context
   build, the reviewer, the ledger write, the release — may propagate, because
   -main has no try of its own and any other exit code is a silently lost
   pass rather than a loud failure."
  [{:keys [git-dir pr sha] :as d} opts]
  (try
    (if (= :duplicate (:status (lock/acquire! git-dir {:pr pr :sha sha} opts)))
      {:exit 0 :message nil}
      (try
        (run-review! d opts)
        ;; Same `opts` acquire! was called with, not just git-dir: release!
        ;; only deletes the record if its :pid still matches, and a test that
        ;; stubs :pid in opts to acquire! must have that same stub honoured on
        ;; release! or the two would disagree about who holds the lock.
        (finally (release-quietly! git-dir opts))))
    (catch Exception e
      {:exit 2 :message (crash-message d e)})))

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
wc -l "$(git rev-parse --git-common-dir)"/pr-review-ledger.jsonl 2>/dev/null || echo "0 (no ledger yet)"
git commit --allow-empty -qm "chore: human push, should not be reviewed"
git push
sleep 20
wc -l "$(git rev-parse --git-common-dir)"/pr-review-ledger.jsonl 2>/dev/null || echo "0 (no ledger yet)"
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
every finding. Write your real verdict line flush left, starting at column 0
with no leading whitespace, no quoting and no list marker. The parser only
accepts a verdict at column 0 on purpose: an indented line reads as a quoted
or illustrative example — including the two example lines immediately below —
never as your actual, final verdict. If you echo any part of this prompt back,
that echo stays indented and is not mistaken for your answer. If more than one
line does start at column 0 with `VERDICT:`, the LAST one is taken as your
answer — so do not restate the required format flush left before reviewing.

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

That list only ever holds follow-up, docs-accuracy and style findings. A
[correctness/blocking] or [coverage] finding is never suppressed, however many
passes it has survived: report it again every pass until it is actually fixed.
Two pushes that do not close a blocking defect must not produce a MERGEABLE
third pass.
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
`"$(git rev-parse --git-common-dir)"/pr-review-ledger.jsonl` has one line; and

```bash
diff <(cat "$(git rev-parse --git-common-dir)"/pr-review-context/<sha>.diff) <(git diff <base>...<sha>)
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
- Consumes: `<git-common-dir>/pr-review-ledger.jsonl`, `<git-common-dir>/pr-review-hint`
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
   the pass history from `$(git rev-parse --git-common-dir)/pr-review-ledger.jsonl`.
4. If there are `[correctness/followup]` findings, open a **new** PR for them
   and let the loop run there. Verify each one before fixing it, same as above.
5. If there are none, the job is done. Say so.

## When the cap is reached

Ten passes have run on this PR. Do not push again expecting another review.
Summarise what is unresolved and hand the decision to the user.

## Hinting the reviewer

To tell the reviewer something before it runs, write it to
`$(git rev-parse --git-common-dir)/pr-review-hint` before pushing. It is
included in the next review's prompt and consumed — it applies to exactly one
pass. Use the command, not a literal `.git/`: in a linked worktree `.git` is a
file and nothing can live under it.

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
   `$(git rev-parse --git-common-dir)/pr-review-hint` first — not a literal
   `.git/`, which is a file in a linked worktree.

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

Per-repo state, all under the clone's *shared* git directory — what
`git rev-parse --git-common-dir` prints, which is `<repo>/.git` in an ordinary
clone and the main clone's `.git` from every linked worktree, so all worktrees
of one repository share one ledger, one lock and one cap. Safe to delete:

| Path | Purpose |
|---|---|
| `<git-common-dir>/pr-review-ledger.jsonl` | pass history, drives the 10-pass cap and the one-re-raise rule |
| `<git-common-dir>/pr-review.lock` | at most one live reviewer per clone |
| `<git-common-dir>/pr-review-context/<sha>.diff` | the untruncated diff the reviewer reads |
| `<git-common-dir>/pr-review-hint` | one-shot note to the next review; consumed on read |

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

## Post-implementation fix wave (0.3.0)

Eight defects found by a whole-branch review of the shipped code. The
transcription of this plan was faithful; the design in it was not. Each is fixed
on this branch, with a test that fails against the shipped code, and the Task
code blocks and Interfaces lines above are synced to the result.

| Finding | Was | Now |
|---|---|---|
| C1 | `--allowedTools` treated as a tool restriction. It is a pre-approval allowlist, and with `permissions.defaultMode: "bypassPermissions"` the reviewer had Bash and wrote outside every repo — R7 unmet, R8 void | `--disallowedTools` + `--strict-mcp-config`. Probed: the reviewer's function list is exactly Glob, Grep, Read |
| C2 | The one-re-raise filter keyed on the raise count with no category filter, so a blocking finding went on the do-not-re-raise list and pass 3 reported MERGEABLE | Only `correctness/followup`, `docs-accuracy` and `style` are suppressible, in code and in `review_core.md` |
| C3 | `<repo-root>/.git` is a file in a worktree, `fs/create-dirs` threw, and `lock/acquire!` sat in the `case` head outside the try: babashka exited 1 on every push from a worktree | `gh/git-common-dir` threaded through ledger, lock, context and the hint path; `review!` wraps everything |
| C4 | `mergeable?` had zero call sites, and `parse-verdict` took the FIRST column-0 verdict | `reconcile` wires it in before both consumers; the LAST column-0 verdict wins; `mergeable?`'s coverage clause reconciled with the prompt (M1) |
| I1 | The kill SIGTERMed the bb trigger, not its `claude -p` child, so a supersede ran two reviewers and the loser exited 143 | `kill-reviewers!` kills the subtree below the recorded pid; the loser detects `superseded?` and exits 0 |
| I2 | `append-pass!` ran even on a crashed or MALFORMED review, so six pushes against an expired token exhausted the cap | A review that produced no findings consumes no slot, and says so |
| I3 | The ledger's `:sha` was recorded and never read, so every repeat push re-reviewed the same commit | A completed pass at this SHA exits 0 silently |
| I4 | `parse-fingerprints` matched one hand-written shape; ranges, backticks, bold, `L42`, capitals and `-` bullets all yielded an empty fingerprint | The invariant `[category] path:line` is matched instead, over emphasis-stripped lines; `parse-counts` swept as a sibling; `parse-warnings` surfaces a total parse failure |
| M2 | The filter re-slurped the whole ledger per distinct fingerprint | `decide` reads once; the predicates are pure over `passes` |
| M7 | `release!` in the `finally` could throw and replace a completed review's result | Wrapped |

New fixtures the shipped tests lacked, and which is why C3 and C4/I4 got
through: a repo whose `.git` is a **file** (a real `git worktree add`), an
assertion that `review!`'s exit is in `#{0 2}` on every branch including a
throwing `acquire!`, and a prompt/parser round trip that feeds the shipped
`review_core.md` through `parse-output` and expects MALFORMED.

Bump to 0.3.0 and reinstall, or none of it reaches the installed copy:

```bash
cd ~/.nixpkgs/claude-code-plugins
bb test
claude plugin uninstall pr-review-loop@nixpkgs-plugins
claude plugin install pr-review-loop@nixpkgs-plugins
```

Then start a **new** interactive session: plugin hooks do not hot-load.

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
