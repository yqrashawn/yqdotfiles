;;; visual.el -*- lexical-binding: t; -*-

(use-package! outline-minor-faces
  :hook (outline-minor-mode . outline-minor-faces-add-font-lock-keywords))

(use-package! backline
  :after outline
  :config
  (advice-add 'outline-flag-region :after 'backline-update))


(use-package! olivetti
  :hook (text-mode . olivetti-mode)
  :init
  (setq! olivetti-minimum-body-width 90
         olivetti-enable-visual-line-mode nil))

(plist-put! +ligatures-extra-symbols :not "¬")