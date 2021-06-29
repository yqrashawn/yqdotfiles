;;; lang/elisp.el -*- lexical-binding: t; -*-

(after! elisp-mode
  (setq! enable-local-variables t))
(after! dash
  (setq! dash-enable-fontlock t))

(use-package! eval-sexp-fu :hook (emacs-lisp-mode . eval-sexp-fu-flash-mode))
(use-package! lisp-extra-font-lock :hook (emacs-lisp-mode . lisp-extra-font-lock-mode))
(use-package! highlight-function-calls :hook (emacs-lisp-mode . highlight-function-calls-mode))
(use-package! easy-escape :hook (emacs-lisp-mode . easy-escape-minor-mode)) ; elisp regexp
