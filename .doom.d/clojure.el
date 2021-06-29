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

(after! cider
  (after! lispy
    ;; make lispy-eval works in babashka repl
    (defadvice! +lispy-eval (orig-fn &rest args)
      :around #'lispy-eval
      (if (and (memq major-mode '(clojure-mode)) (functionp 'cider--babashka-version) (cider--babashka-version))
          (if (and lispy-mode (lispy-left-p))
              (save-excursion
                (call-interactively 'lispy-different)
                (call-interactively 'cider-eval-last-sexp))
            (call-interactively 'cider-eval-last-sexp))
        (apply orig-fn args)))

    (defadvice! +lispy-eval-and-insert (func &rest args)
      :around #'lispy-eval-and-insert
      (if (and (memq major-mode '(clojure-mode)) (functionp 'cider--babashka-version) (cider--babashka-version))
          (progn
            ;; (setq current-prefix-arg '(1))
            (call-interactively 'cider-pprint-eval-last-sexp))
        (apply func args)))))

(add-hook! (clojure-mode clojurescript-mode clojurec-mode)
  (cmd! (setq-local company-idle-delay 0.2
                    evil-shift-width 1)))
