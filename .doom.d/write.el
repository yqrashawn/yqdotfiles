;;; write.el -*- lexical-binding: t; -*-

(use-package! writeroom-mode
  ;; :hook (elfeed-show-mode . writeroom-mode)
  :hook (text-mode . writeroom-mode)
  ;; :hook (notmuch-show-mode . writeroom-mode)
  )
(use-package! olivetti
  :defer t
  ;; :hook (text-mode . olivetti-mode)
  ;; :hook (notmuch-show-mode . olivetti-mode)
  :init
  (setq! olivetti-minimum-body-width 90
         olivetti-enable-visual-line-mode nil))
