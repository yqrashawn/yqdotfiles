;;; visual.el -*- lexical-binding: t; -*-

(use-package! outline-minor-faces
  :hook (outline-minor-mode . outline-minor-faces-add-font-lock-keywords))

(use-package! backline
  :after outline
  :config
  (advice-add 'outline-flag-region :after 'backline-update))

(plist-put! +ligatures-extra-symbols :not "¬")