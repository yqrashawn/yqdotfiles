;;; imenu-anywhere-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "imenu-anywhere" "imenu-anywhere.el" (22623
;;;;;;  25974 0 0))
;;; Generated autoloads from imenu-anywhere.el

(autoload 'imenu-anywhere "imenu-anywhere" "\
Go to imenu tag defined in all reachable buffers.
Reachable buffers are determined by applying functions in
`imenu-anywhere-buffer-filter-functions' to all buffers returned
by `imenu-anywhere-buffer-list-function'.

Sorting is done within each buffer and takes into account items'
length. Thus more recent buffers in `buffer-list' and shorter
entries have higher priority.

\(fn)" t nil)

(autoload 'ido-imenu-anywhere "imenu-anywhere" "\
IDO interface for `imenu-anywhere'.
This is a simple wrapper around `imenu-anywhere' which uses
`ido-completing-read' as `completing-read-function'. If you use
`ido-ubiquitous' you might be better off by using `ido-anywhere'
instead, but there should be little or no difference.

\(fn)" t nil)

(autoload 'ivy-imenu-anywhere "imenu-anywhere" "\
IVY interface for `imenu-anywhere'
This is a simple wrapper around `imenu-anywhere' which uses
`ivy-completing-read' as `completing-read-function'.

\(fn)" t nil)

(autoload 'helm-imenu-anywhere "imenu-anywhere" "\
`helm' interface for `imenu-anywhere'.
Sorting is in increasing length of imenu symbols within each
buffer.  The pyramidal view allows distinguishing different
buffers.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; imenu-anywhere-autoloads.el ends here
