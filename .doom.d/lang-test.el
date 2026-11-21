;;; .nixpkgs/.doom.d/lang-test.el -*- lexical-binding: t; coding: utf-8; -*-

(require 'ert)
(require 'cl-lib)
(require 'treesit-auto)

(comment
  (ert-run-tests-interactively "+treesit-auto-")
  (ert "+treesit-auto-"))

;;; Tests for the treesit-auto remap-alist cache (lang.el)

(defvar +lang-test--recipe-calls 0)

(defun +lang-test--count-recipes (&rest _)
  "Stub for `treesit-auto--selected-recipes' that counts invocations."
  (cl-incf +lang-test--recipe-calls)
  nil)

(ert-deftest +treesit-auto-build-cached-test ()
  "Grammar verification must run once per world, not once per file open.

Each uncached build re-verifies every grammar via
`treesit-language-available-p' (a dlopen per grammar), which was ~10s
per file open. The cache advice must collapse repeated builds into
one and serve the same object on the second call."
  (setq +treesit-auto-remap-alist-cache 'uninitialized
        +lang-test--recipe-calls 0)
  (unwind-protect
      (let ((major-mode-remap-alist '((foo-mode . bar-mode))))
        (cl-letf (((symbol-function 'treesit-auto--selected-recipes)
                   #'+lang-test--count-recipes))
          (let ((first (treesit-auto--build-major-mode-remap-alist)))
            (should (= +lang-test--recipe-calls 1))
            (should (eq (treesit-auto--build-major-mode-remap-alist) first))
            (should (= +lang-test--recipe-calls 1)))))
    (setq +treesit-auto-remap-alist-cache 'uninitialized)))

(ert-deftest +treesit-auto-empty-build-cached-test ()
  "A nil build (no grammars ready, no global remaps) must stay cached.

Otherwise a fresh machine with zero grammars silently loses the
optimization and pays the full rebuild on every file open."
  (setq +treesit-auto-remap-alist-cache 'uninitialized
        +lang-test--recipe-calls 0)
  (unwind-protect
      (let ((major-mode-remap-alist nil))
        (cl-letf (((symbol-function 'treesit-auto--selected-recipes)
                   #'+lang-test--count-recipes))
          (should (null (treesit-auto--build-major-mode-remap-alist)))
          (should (= +lang-test--recipe-calls 1))
          (should (null (treesit-auto--build-major-mode-remap-alist)))
          (should (= +lang-test--recipe-calls 1))))
    (setq +treesit-auto-remap-alist-cache 'uninitialized)))

(ert-deftest +treesit-auto-cache-reset-on-install-test ()
  "A grammar installed mid-session must be remapped without a restart.

`treesit-auto-install' is t here, so grammars appear on a long-lived
daemon; the :after advice on `treesit-install-language-grammar' must
clear the cache so the next open rebuilds the remap alist."
  (setq +treesit-auto-remap-alist-cache 'uninitialized
        +lang-test--recipe-calls 0)
  (unwind-protect
      (cl-letf (((symbol-function 'treesit-auto--selected-recipes)
                 #'+lang-test--count-recipes))
        (treesit-auto--build-major-mode-remap-alist)
        (treesit-auto--build-major-mode-remap-alist)
        (should (= +lang-test--recipe-calls 1))
        (+treesit-auto-reset-remap-cache-a)
        (treesit-auto--build-major-mode-remap-alist)
        (should (= +lang-test--recipe-calls 2))
        (should (advice-member-p '+treesit-auto-reset-remap-cache-a
                                  'treesit-install-language-grammar))
        (should (advice-member-p '+treesit-auto-cache-remap-a
                                  'treesit-auto--build-major-mode-remap-alist)))
    (setq +treesit-auto-remap-alist-cache 'uninitialized)))
