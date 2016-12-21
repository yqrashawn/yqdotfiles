;;; orgit-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "orgit" "orgit.el" (22594 47653 0 0))
;;; Generated autoloads from orgit.el

(autoload 'orgit-link-set-parameters "orgit" "\


\(fn TYPE &rest PARAMETERS)" nil nil)

(eval-after-load "org" '(orgit-link-set-parameters "orgit" :store 'orgit-status-store :follow 'orgit-status-open :export 'orgit-status-export))

(autoload 'orgit-status-store "orgit" "\


\(fn)" nil nil)

(autoload 'orgit-status-open "orgit" "\


\(fn PATH)" nil nil)

(autoload 'orgit-status-export "orgit" "\


\(fn PATH DESC FORMAT)" nil nil)

(eval-after-load "org" '(orgit-link-set-parameters "orgit-log" :store 'orgit-log-store :follow 'orgit-log-open :export 'orgit-log-export))

(autoload 'orgit-log-store "orgit" "\


\(fn)" nil nil)

(autoload 'orgit-log-open "orgit" "\


\(fn PATH)" nil nil)

(autoload 'orgit-log-export "orgit" "\


\(fn PATH DESC FORMAT)" nil nil)

(eval-after-load "org" '(orgit-link-set-parameters "orgit-rev" :store 'orgit-rev-store :follow 'orgit-rev-open :export 'orgit-rev-export))

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
