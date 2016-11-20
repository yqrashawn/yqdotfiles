;;; orgit-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "orgit" "orgit.el" (22556 42726 0 0))
;;; Generated autoloads from orgit.el

(eval-after-load "org" '(progn (org-add-link-type "orgit" 'orgit-status-open 'orgit-status-export) (add-hook 'org-store-link-functions 'orgit-status-store)))

(autoload 'orgit-status-store "orgit" "\


\(fn)" nil nil)

(autoload 'orgit-status-open "orgit" "\


\(fn PATH)" nil nil)

(autoload 'orgit-status-export "orgit" "\


\(fn PATH DESC FORMAT)" nil nil)

(eval-after-load "org" '(progn (org-add-link-type "orgit-log" 'orgit-log-open 'orgit-log-export) (add-hook 'org-store-link-functions 'orgit-log-store)))

(autoload 'orgit-log-store "orgit" "\


\(fn)" nil nil)

(autoload 'orgit-log-open "orgit" "\


\(fn PATH)" nil nil)

(autoload 'orgit-log-export "orgit" "\


\(fn PATH DESC FORMAT)" nil nil)

(eval-after-load "org" '(progn (org-add-link-type "orgit-rev" 'orgit-rev-open 'orgit-rev-export) (add-hook 'org-store-link-functions 'orgit-rev-store)))

(autoload 'orgit-rev-store "orgit" "\
Store a link to a Magit-Revision mode buffer.
With a prefix argument instead store the name of the branch that
points at the revision, if any.

\(fn)" nil nil)

(autoload 'orgit-rev-open "orgit" "\


\(fn PATH)" nil nil)

(autoload 'orgit-rev-export "orgit" "\


\(fn PATH DESC FORMAT)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; orgit-autoloads.el ends here
