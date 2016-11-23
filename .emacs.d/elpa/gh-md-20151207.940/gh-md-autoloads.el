;;; gh-md-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "gh-md" "gh-md.el" (22581 12850 0 0))
;;; Generated autoloads from gh-md.el

(defalias 'gh-md-render-region #'gh-md-convert-region)

(autoload 'gh-md-convert-region "gh-md" "\
Show a preview the markdown from a region from BEGIN to END.

EXPORT writes a file.

\(fn BEGIN END &optional EXPORT)" t nil)

(autoload 'gh-md-render-buffer "gh-md" "\
Render the markdown content from BUFFER.

\(fn)" t nil)

(autoload 'gh-md-export-region "gh-md" "\
Export to a file the markdown content from region BEGIN to END.

\(fn BEGIN END)" t nil)

(autoload 'gh-md-export-buffer "gh-md" "\
Export to a file the markdown content from BUFFER.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; gh-md-autoloads.el ends here
