;;; lang/clojure.el -*- lexical-binding: t; -*-

(after! clojure-mode
  (add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
  ;; This regexp matches shebang expressions like `#!/usr/bin/env boot'
  (add-to-list 'magic-mode-alist '("#!.*boot\\s-*$" . clojure-mode))
  (setq! clojure-toplevel-inside-comment-form t
         clojure-align-reader-conditionals t
         clojure-defun-indents '(fn-traced)
         clojure-verify-major-mode nil)
  ;; #_ is not a logical sexp
  (defadvice! corgi/clojure--looking-at-non-logical-sexp (command)
    :around #'clojure--looking-at-non-logical-sexp
    "Return non-nil if text after point is \"non-logical\" sexp.
\"Non-logical\" sexp are ^metadata and #reader.macros."
    (comment-normalize-vars)
    (comment-forward (point-max))
    (looking-at-p "\\(?:#?\\^\\)\\|#:?:?[[:alpha:]]\\|#_")))

(add-hook! (clojure-mode clojurescript-mode clojurec-mode)
  (cmd! (setq-local company-idle-delay 0.2
                    evil-shift-width 1)))
