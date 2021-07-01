;;; visual.el -*- lexical-binding: t; -*-

(use-package outline-minor-faces
  :hook (outline-minor-mode . outline-minor-faces-add-font-lock-keywords))

(plist-put! +ligatures-extra-symbols :not "¬")