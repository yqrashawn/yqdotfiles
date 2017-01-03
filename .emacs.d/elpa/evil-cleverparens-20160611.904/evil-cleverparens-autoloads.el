;;; evil-cleverparens-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "evil-cleverparens" "evil-cleverparens.el"
;;;;;;  (22635 44883 0 0))
;;; Generated autoloads from evil-cleverparens.el

(autoload 'evil-cp-set-movement-keys "evil-cleverparens" "\
Sets the movement keys in
`evil-cleverparens-regular-movement-keys' or
`evil-cp-swapped-movement-keys' based on the value of
`evil-cleverparens-swap-move-by-word-and-symbol'.

\(fn)" t nil)

(autoload 'evil-cp-set-additional-movement-keys "evil-cleverparens" "\
Sets the movement keys is `evil-cp-additional-movement-keys'
for normal, visual and operator states if
`evil-cleverparens-use-additional-movement-keys' is true.

\(fn)" t nil)

(autoload 'evil-cp-set-additional-bindings "evil-cleverparens" "\
Sets the movement keys is `evil-cp-additional-bindings' for
normal-state if `evil-cleverparens-use-additional-bindings' is
true.

\(fn)" t nil)

(autoload 'evil-cleverparens-mode "evil-cleverparens" "\
Minor mode for setting up evil with smartparens and paredit
for an advanced modal structural editing experience.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "evil-cleverparens-text-objects" "evil-cleverparens-text-objects.el"
;;;;;;  (22635 44883 0 0))
;;; Generated autoloads from evil-cleverparens-text-objects.el
 (autoload 'evil-cp-a-form "evil-cleverparens-text-objects" nil t)
 (autoload 'evil-cp-inner-form "evil-cleverparens-text-objects" nil t)
 (autoload 'evil-cp-a-comment "evil-cleverparens-text-objects" nil t)
 (autoload 'evil-cp-inner-comment "evil-cleverparens-text-objects" nil t)
 (autoload 'evil-cp-a-defun "evil-cleverparens-text-objects" nil t)
 (autoload 'evil-cp-inner-defun "evil-cleverparens-text-objects" nil t)

;;;***

;;;### (autoloads nil nil ("evil-cleverparens-pkg.el" "evil-cleverparens-util.el")
;;;;;;  (22635 44883 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; evil-cleverparens-autoloads.el ends here
