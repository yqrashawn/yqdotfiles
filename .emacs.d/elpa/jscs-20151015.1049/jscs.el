;;; jscs.el --- Consistent JavaScript editing using JSCS  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  papaeye

;; Author: papaeye <papaeye@gmail.com>
;; Keywords: languages, convenience
;; Package-Version: 20151015.1049
;; Version: 0.2.0alpha
;; Homepage: https://github.com/papaeye/emacs-jscs
;; Package-Requires: ((emacs "24.1") (cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; jscs.el helps consistent JavaScript editing using JSCS.
;;
;; Installation:
;;
;; jscs.el is available on [MELPA]<http://melpa.org/>.
;;
;; Usage:
;;
;; To apply JSCS indentation rules to JavaScript or JSON modes,
;; add the following code into your .emacs:
;;
;;     (add-hook 'js-mode-hook #'jscs-indent-apply)
;;     (add-hook 'js2-mode-hook #'jscs-indent-apply)
;;     (add-hook 'json-mode-hook #'jscs-indent-apply)
;;
;; To run "jscs --fix" interactively, run \\[jscs-fix].
;;
;; To run "jscs --fix" on JavaScript modes when saving,
;; add the following code into your .emacs:
;;
;;     (add-hook 'js-mode-hook #'jscs-fix-run-before-save)
;;     (add-hook 'js2-mode-hook #'jscs-fix-run-before-save)

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'json)

(defvar js-indent-level)
(defvar js2-basic-offset)

;; JSCS exit codes
(defconst jscs-exit-code-style-errors 2)
(defconst jscs-exit-missing-config 4)

(defgroup jscs nil
  "Consistent JavaScript editing using JSCS"
  :group 'tools)

(defcustom jscs-command "jscs"
  "The 'jscs' command."
  :type 'string
  :group 'jscs)

(defcustom jscs-node-command "node"
  "The 'node' command."
  :type 'string
  :group 'jscs)

(defcustom jscs-node-path (expand-file-name
			   "../lib/node_modules/"
			   (file-name-directory (executable-find jscs-command)))
  "The NODE_PATH environment variable."
  :type 'string
  :group 'jscs)

(defun jscs--load-config ()
  (with-temp-buffer
    (insert
     "var configFile = require('jscs/lib/cli-config');"
     "var Configuration = require('jscs/lib/config/configuration');"
     "var content = configFile.load();"
     "var config = new Configuration();"
     "config.registerDefaultRules();"
     "config.registerDefaultPresets();"
     "config.load(content);"
     "var result = config.getProcessedConfig();"
     "console.log(JSON.stringify(result));")
    (let ((process-environment process-environment))
      (push (concat "NODE_PATH=" jscs-node-path) process-environment)
      (call-process-region (point-min) (point-max) jscs-node-command t t))
    (goto-char (point-min))
    (ignore-errors
      (json-read))))

(defun jscs-indent--rule-validate-indentation (config)
  (let ((indent (cdr (assq 'validateIndentation config))))
    (when (consp indent)
      (setq indent (cdr (assq 'value indent))))
    (cond
     ((integerp indent)
      (cond
       ((memq major-mode '(js-mode json-mode))
	(setq-local js-indent-level indent))
       ((eq major-mode 'js2-mode)
	(setq-local js2-basic-offset indent)))
      (setq indent-tabs-mode nil))
     ((string= indent "\t")
      (setq indent-tabs-mode t)))))

(defun jscs-indent--rule-maximum-line-length (config)
  (let ((rule (cdr (assq 'maximumLineLength config)))
	tab-size)
    (when (consp rule)
      (setq tab-size (cdr (assq 'tabSize rule)))
      (when (integerp tab-size)
	(setq tab-width tab-size)))))

(defvar jscs-indent--rule-functions
  (list #'jscs-indent--rule-validate-indentation
	#'jscs-indent--rule-maximum-line-length))

;;;###autoload
(defun jscs-indent-apply ()
  "Apply JSCS indentation rules."
  (interactive)
  (let ((config (jscs--load-config)))
    (dolist (func jscs-indent--rule-functions)
      (funcall func config))))

;; The following code is based on gofmt of go-mode.el.
;;
;; Copyright 2013 The go-mode Authors. All rights reserved.
;; Use of this source code is governed by a BSD-style
;; license that can be found in the LICENSE.go-mode file.

(defcustom jscs-fix-show-errors 'buffer
  "Where to display jscs-fix error output.
It can either be displayed in its own buffer, in the echo area, or not at all.

Please note that Emacs outputs to the echo area when writing
files and will overwrite jscs-fix's echo output if used from inside
a `before-save-hook'."
  :type '(choice
	  (const :tag "Own buffer" buffer)
	  (const :tag "Echo area" echo)
	  (const :tag "None" nil))
  :group 'jscs)

(defun jscs-fix--delete-whole-line (&optional arg)
  "Delete the current line without putting it in the `kill-ring'.
Derived from function `kill-whole-line'.  ARG is defined as for that
function."
  (setq arg (or arg 1))
  (if (and (> arg 0)
	   (eobp)
	   (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0)
	   (bobp)
	   (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (cond ((zerop arg)
	 (delete-region (progn (forward-visible-line 0) (point))
			(progn (end-of-visible-line) (point))))
	((< arg 0)
	 (delete-region (progn (end-of-visible-line) (point))
			(progn (forward-visible-line (1+ arg))
			       (unless (bobp)
				 (backward-char))
			       (point))))
	(t
	 (delete-region (progn (forward-visible-line 0) (point))
			(progn (forward-visible-line arg) (point))))))

(defun jscs-fix--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
	;; Relative offset between buffer line numbers and line numbers
	;; in patch.
	;;
	;; Line numbers in the patch are based on the source file, so
	;; we have to keep an offset when making changes to the
	;; buffer.
	;;
	;; Appending lines decrements the offset (possibly making it
	;; negative), deleting lines increments it. This order
	;; simplifies the forward-line invocations.
	(line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
	(goto-char (point-min))
	(while (not (eobp))
	  (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
	    (error "invalid rcs patch or internal error in jscs-fix--apply-rcs-patch"))
	  (forward-line)
	  (let ((action (match-string 1))
		(from (string-to-number (match-string 2)))
		(len  (string-to-number (match-string 3))))
	    (cond
	     ((equal action "a")
	      (let ((start (point)))
		(forward-line len)
		(let ((text (buffer-substring start (point))))
		  (with-current-buffer target-buffer
		    (cl-decf line-offset len)
		    (goto-char (point-min))
		    (forward-line (- from len line-offset))
		    (insert text)))))
	     ((equal action "d")
	      (with-current-buffer target-buffer
		(jscs-fix--goto-line (- from line-offset))
		(cl-incf line-offset len)
		(jscs-fix--delete-whole-line len)))
	     (t
	      (error "invalid rcs patch or internal error in jscs-fix--apply-rcs-patch")))))))))

(defun jscs-fix--kill-error-buffer (errbuf)
  (let ((win (get-buffer-window errbuf)))
    (if win
	(quit-window t win)
      (kill-buffer errbuf))))

(defun jscs-fix--goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

(defun jscs-fix--process-errors (filename tmpfile errbuf)
  (with-current-buffer errbuf
    (if (eq jscs-fix-show-errors 'echo)
	(progn
	  (message "%s" (buffer-string))
	  (jscs-fix--kill-error-buffer errbuf))
      ;; Convert stderr to something understood by the compilation mode.
      (goto-char (point-min))
      (insert "jscs-fix errors:\n")
      (funcall #'jscs-fix--error-filter filename tmpfile)
      (compilation-mode)
      (display-buffer errbuf))))

(defun jscs-fix--error-filter (filename tmpfile)
  (while (search-forward-regexp
	  (concat "^\\(?:"
		  (regexp-quote tmpfile)
		  "\\): line \\([0-9]+\\), col \\([0-9]+\\), \\(.+\\)")
	  nil t)
    (replace-match (concat (file-name-nondirectory filename)
			   ":" (match-string 1) ":" (match-string 2)
			   ": " (match-string 3))
		   t t)))

;;;###autoload
(defun jscs-fix ()
  "Format the current buffer according to the JSCS tool."
  (interactive)
  (let* ((tmpfile-suffix (file-name-extension (buffer-file-name) t))
	 (tmpfile (make-temp-file "jscs-fix" nil tmpfile-suffix))
	 (patchbuf (get-buffer-create "*Jscs-Fix patch*"))
	 (errbuf (if jscs-fix-show-errors
		     (get-buffer-create "*Jscs-Fix Errors*")))
	 (coding-system-for-read 'utf-8)
	 (coding-system-for-write 'utf-8))

    (save-restriction
      (widen)
      (if errbuf
	  (with-current-buffer errbuf
	    (setq buffer-read-only nil)
	    (erase-buffer)))
      (with-current-buffer patchbuf
	(erase-buffer))

      (write-region nil nil tmpfile)

      (let ((exit (call-process jscs-command nil errbuf nil
				"--fix" "--reporter" "inline" tmpfile)))
	(cond
	 ((= exit jscs-exit-missing-config)
	  (message "No configuration found"))
	 (t
	  (if (zerop (call-process-region (point-min) (point-max) "diff"
					  nil patchbuf nil "-n" "-" tmpfile))
	      (message (if (zerop exit)
			   "Buffer is already jscs-fixed"
			 "Could not apply jscs-fix"))
	    (jscs-fix--apply-rcs-patch patchbuf)
	    (message (if (zerop exit)
			 "Applied jscs-fix"
		       "Applied jscs-fix partially")))))
	(when errbuf
	  (if (zerop exit)
	      (jscs-fix--kill-error-buffer errbuf)
	    (jscs-fix--process-errors (buffer-file-name) tmpfile errbuf)))))

    (kill-buffer patchbuf)
    (delete-file tmpfile)))

;;;###autoload
(defun jscs-fix-run-before-save ()
  "Add this to .emacs to run jscs-fix on the current buffer when saving:
 (add-hook 'js-mode-hook #'jscs-fix-run-before-save)
 (add-hook 'js2-mode-hook #'jscs-fix-run-before-save)
 (add-hook 'js3-mode-hook #'jscs-fix-run-before-save)."
  (interactive)
  (add-hook 'before-save-hook #'jscs-fix nil t))

(provide 'jscs)
;;; jscs.el ends here
