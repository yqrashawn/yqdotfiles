;;; jscs-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "jscs" "jscs.el" (22600 4953 0 0))
;;; Generated autoloads from jscs.el

(autoload 'jscs-indent-apply "jscs" "\
Apply JSCS indentation rules.

\(fn)" t nil)

(autoload 'jscs-fix "jscs" "\
Format the current buffer according to the JSCS tool.

\(fn)" t nil)

(autoload 'jscs-fix-run-before-save "jscs" "\
Add this to .emacs to run jscs-fix on the current buffer when saving:
 (add-hook 'js-mode-hook #'jscs-fix-run-before-save)
 (add-hook 'js2-mode-hook #'jscs-fix-run-before-save)
 (add-hook 'js3-mode-hook #'jscs-fix-run-before-save).

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; jscs-autoloads.el ends here
