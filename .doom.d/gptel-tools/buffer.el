;;; .nixpkgs/.doom.d/gptel-tools/buffer.el -*- lexical-binding: t; -*-


;;; Commentary:
;; Buffer tool for gptel.
;; Moved from gptel-tools.el for better organization.

;;; Code:

(require 'gptel)
(require 'cl-lib)

;;; Get buffer file path utilities

(defun gptel-tools--get-buffer-file-path (buf-name)
  "Return the file path for buffer BUF-NAME."
  (when-let ((b (get-buffer buf-name)))
    (buffer-file-name b)))

;;; Tool registration

;; Register the get buffer file path tool with gptel
(when (fboundp 'gptel-make-tool)
  (gptel-make-tool
   :name "get_buffer_file_path"
   :function #'gptel-tools--get-buffer-file-path
   :description "Given a buffer-name, return the file path that the buffer is visiting or nil"
   :args '((:name "buffer_name" :type string
            :description "buffer name"))
   :category "emacs"
   :confirm nil
   :include t))

;;; List buffers utilities

(defun gptel-tools-list-buffers ()
  "Return a list of all visible buffer names."
  (mapcar #'buffer-name (buffer-list)))

(defun gptel-tools-filter-buffers-regex (pattern)
  "Return a list of buffer names matching given pattern (regex string). Supports PCRE if pcre2el is available."
  (let ((rx pattern)
        (pcre2el-available (require 'pcre2el nil t)))
    (let ((regex (if pcre2el-available
                     (condition-case nil
                         (rxt-pcre-to-elisp pattern)
                       (error pattern))
                   pattern)))
      (cl-remove-if-not (lambda (name) (string-match-p regex name))
                        (mapcar #'buffer-name (buffer-list))))))

(defun gptel-tools-visible-buffers ()
  "Return a list of buffer names that are currently visible in windows."
  (+visible-buffers))

;;; Tool registration

;; Register the list buffers tool and filter tool with gptel
(when (fboundp 'gptel-make-tool)
  (gptel-make-tool
   :name "list_buffers"
   :function #'gptel-tools-list-buffers
   :description "Return a list of all visible buffer names (strings)."
   :args nil
   :category "emacs"
   :confirm nil
   :include t)
  (gptel-make-tool
   :name "filter_buffers_regex"
   :function #'gptel-tools-filter-buffers-regex
   :description "Return a list of buffer names matching given pattern (regex string)."
   :args (list '(:name "pattern" :type string :description "regex pattern to match buffer names"))
   :category "emacs"
   :confirm nil
   :include t)
  (gptel-make-tool
   :name "visible_buffers"
   :function #'gptel-tools-visible-buffers
   :description "Return a list of buffer names that are currently visible in windows to the user, user usually want to change these buffers."
   :args nil
   :category "emacs"
   :confirm nil
   :include t))

;;; buffer-tool.el ends here
