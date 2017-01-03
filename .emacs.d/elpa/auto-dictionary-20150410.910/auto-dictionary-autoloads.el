;;; auto-dictionary-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "auto-dictionary" "auto-dictionary.el" (22634
;;;;;;  6360 0 0))
;;; Generated autoloads from auto-dictionary.el

(autoload 'auto-dictionary-mode "auto-dictionary" "\
A minor mode that automatically sets `ispell-dictionary`.

\(fn &optional ARG)" t nil)

(autoload 'adict-guess-dictionary "auto-dictionary" "\
Automatically change ispell dictionary based on buffer language.
Calls `ispell-change-dictionary' and runs `adict-change-dictionary-hook'.  If
BUFFER is nil, the current buffer is used.  If IDLE-ONLY is set, abort
when an input event occurs.

\(fn &optional IDLE-ONLY)" t nil)

(autoload 'adict-change-dictionary "auto-dictionary" "\
Set buffer language to LANG and stop detecting it automatically.

\(fn &optional LANG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; auto-dictionary-autoloads.el ends here
