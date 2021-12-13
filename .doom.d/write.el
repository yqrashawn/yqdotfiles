;;; write.el -*- lexical-binding: t; -*-

(use-package! olivetti
  :hook ((notmuch-show-mode text-mode) . olivetti-mode)
  :init
  (setq! olivetti-minimum-body-width 90
         olivetti-enable-visual-line-mode nil))