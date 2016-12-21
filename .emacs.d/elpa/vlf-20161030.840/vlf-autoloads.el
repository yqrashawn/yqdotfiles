;;; vlf-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "vlf" "vlf.el" (22594 47663 0 0))
;;; Generated autoloads from vlf.el

(autoload 'vlf "vlf" "\
View Large FILE in batches.  When MINIMAL load just a few bytes.
You can customize number of bytes displayed by customizing
`vlf-batch-size'.
Return newly created buffer.

\(fn FILE &optional MINIMAL)" t nil)

;;;***

;;;### (autoloads nil "vlf-ediff" "vlf-ediff.el" (22594 47663 0 0))
;;; Generated autoloads from vlf-ediff.el

(autoload 'vlf-ediff-files "vlf-ediff" "\
Run batch by batch ediff over FILE-A and FILE-B.
Files are processed with VLF with BATCH-SIZE chunks.
Requesting next or previous difference at the end or beginning
respectively of difference list, runs ediff over the adjacent chunks.

\(fn FILE-A FILE-B BATCH-SIZE)" t nil)

;;;***

;;;### (autoloads nil "vlf-occur" "vlf-occur.el" (22594 47663 0 0))
;;; Generated autoloads from vlf-occur.el

(autoload 'vlf-occur-load "vlf-occur" "\
Load serialized `vlf-occur' results from current buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("vlf-base.el" "vlf-follow.el" "vlf-pkg.el"
;;;;;;  "vlf-search.el" "vlf-setup.el" "vlf-tune.el" "vlf-write.el")
;;;;;;  (22594 47663 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; vlf-autoloads.el ends here
