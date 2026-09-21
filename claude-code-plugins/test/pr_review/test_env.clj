(ns pr-review.test-env
  "Keeps the suite off the operator's live rotation state.

   `reviewer/run!` now SELECTS a token, and selection is a write: it advances
   a cursor in `$XDG_CACHE_HOME/pr-review-loop/tokens.edn`, the same file real
   reviews rotate through. Every test that reaches `run!` without pinning a
   token therefore moved the operator's rotation on by one — measured at three
   places in `reviewer-test` alone, plus every `trigger-test` that goes through
   `review!`, so `bb test` skipped several of their tokens per run.

   Naming those call sites one by one is the fix that goes stale: the next
   test to call `run!` is written without knowing any of this. The seam is
   `tokens/state-path`, so it is closed HERE, once, for every test in the
   namespace.

   The POOL is deliberately left alone, and a fixtured test does still read
   the operator's live `CLAUDE_TOKENS`. That read mutates nothing, and with
   the cursor pointed at a scratch file it cannot move the real rotation on.
   A test that needs a particular pool redefs it locally, where an inner
   `with-redefs` wins.

   `:once`, and `with-redefs` rather than `binding`, because `with-redefs`
   alters the root: a `:each` fixture would re-enter it per var for no gain,
   and the suite is single-threaded."
  (:require [babashka.fs :as fs]
            [pr-review.tokens :as tokens]))

(defn hermetic-tokens
  "`(use-fixtures :once hermetic-tokens)` in any namespace that can reach
   `reviewer/run!` or `reviewer/spawn-env`.

   `state-path` ONLY. Redefining `pool` here as well looked tidier and broke
   the tests that exercise `pool` itself — and it was closing a hazard that is
   not there: reading the operator's live pool mutates nothing, and with the
   cursor pointed at a scratch file no test can move the real rotation on. A
   test that needs a specific pool says so locally, where an inner
   `with-redefs` wins."
  [f]
  (let [state (str (fs/path (fs/create-temp-dir {:prefix "prl-test-tokens"})
                            "tokens.edn"))]
    (with-redefs [tokens/state-path (constantly state)]
      (f))))
