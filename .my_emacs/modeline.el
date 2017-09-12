(setq-default powerline-height 8)
(setq mu4e-alert-mode-line t)
(setq evil-mode-line-format '(before . mode-line-front-space))
(add-to-list 'sml/replacer-regexp-list
             '("^~/workspace/THIRD/three.js" ":THREE:"))
(add-to-list 'sml/replacer-regexp-list
             '("^~/workspace/HOME/" ":HOME:"))
(add-to-list 'sml/replacer-regexp-list
             '("^~/workspace/OFFICE/" ":WORK:"))
(add-to-list 'sml/replacer-regexp-list
             '("^~/workspace/OFFICE/gltflmvviewer" ":LMV:"))
(add-to-list 'sml/replacer-regexp-list
             '("^~/workspace/OFFICE/gltflmvviewer/src" ":LMVS:"))
(add-to-list 'sml/replacer-regexp-list
             '("^~/workspace/OFFICE/gltflmvviewer/src/extensions" ":LMV|E:"))
(add-to-list 'sml/replacer-regexp-list
             '("^~/\\.emacs\\.d/" ":CONF:"))
