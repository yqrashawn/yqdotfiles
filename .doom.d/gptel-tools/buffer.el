;;; buffer.el --- GPTEL buffer tool  -*- lexical-binding: t; -*-
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Homepage: https://github.com/yqrashawn/gptel-tools
;; Keywords: tools

;;; Commentary:
;;
;; Buffer manipulation utilities for GPTEL.
;;
;;; Code:

;;; Debug Configuration
(defvar gptelt-buffer-debug nil
  "When non-nil, log buffer tool operations for debugging.")


;;; Get buffer file path utilities

(defun gptelt--get-buffer-file-path (buf-name)
  "Return the file path for buffer BUF-NAME."
  (if-let ((b (get-buffer buf-name)))
      (buffer-file-name b)
    (error "Buffer not found: %s" buf-name)))

(comment
  (gptelt--get-buffer-file-path "buffer.el"))

(defun gptelt--get-file-buffer-name (file-path)
  "Return the buffer name visiting FILE-PATH, or nil if not found."
  (let ((buf (cl-find file-path (buffer-list)
                      :key #'buffer-file-name
                      :test #'string-equal)))
    (when buf (buffer-name buf))))

(comment
  (gptelt--get-file-buffer-name "/Users/yqrashawn/.nixpkgs/.doom.d/gptel-tools/buffer.el"))

;;; Tool registration

;; Register the get buffer file path tool and get file buffer name tool with gptel
(when (fboundp 'gptelt-make-tool)
  (gptelt-make-tool
   :name "get_buffer_file_path"
   :function #'gptelt--get-buffer-file-path
   :description "Given a buffer_name, return the file path that the buffer is visiting"
   :args '((:name "buffer_name" :type string
            :description "buffer name"))
   :category "buffer"
   :confirm nil
   :include t)

  (gptelt-make-tool
   :name "get_file_buffer_name"
   :function #'gptelt--get-file-buffer-name
   :description "Given a file_path, return the buffer name that is visiting the file or nil."
   :args '((:name "file_path" :type string :description "absolute file path"))
   :category "buffer"
   :confirm nil
   :include t))

;;; List buffers utilities

(defun gptelt-list-buffers ()
  "Return a list of all visible buffer names."
  (mapcar #'buffer-name (buffer-list)))

(comment
  (gptelt-list-buffers))

(defun gptelt-filter-buffers-regex (pattern)
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

(comment
  (gptelt-filter-buffers-regex ".*\\.el$"))

(defun gptelt-visible-buffers ()
  "Return a list of buffer names that are currently visible in windows."
  (seq-map 'buffer-name (+visible-buffers)))

(comment
  (gptelt-visible-buffers))

;;; Tool registration

;; Register the list buffers tool and filter tool with gptel
(gptelt-make-tool
 :name "list_buffers"
 :function #'gptelt-list-buffers
 :description (concat "Return a list of all buffer names currently open in Emacs. "
                      "Returns array of strings (buffer names). Use this to discover available buffers before reading them.")
 :args '()
 :category "buffer"
 :confirm nil
 :include t)
(gptelt-make-tool
 :name "filter_buffers_regex"
 :function #'gptelt-filter-buffers-regex
 :description (concat "Filter buffers by regex pattern. Returns array of buffer names matching the pattern. "
                      "Supports PCRE regex if pcre2el is available, otherwise uses Emacs regex."
                      "\n\nPARAMETER STRUCTURE:\n"
                      "{\n"
                      "  \"pattern\": \"string\" (required) - Regex pattern to match buffer names\n"
                      "}\n\n"
                      "Example: filter_buffers_regex('.*\\.el$') returns all Emacs Lisp buffers")
 :args '((:name "pattern" :type string :description "regex pattern to match buffer names"))
 :category "buffer"
 :confirm nil
 :include t)
(gptelt-make-tool
 :name "visible_buffers"
 :function #'gptelt-visible-buffers
 :description (concat "Return buffer names currently visible in windows (i.e., on screen). "
                      "These are buffers the user is likely working with. Returns array of buffer names. "
                      "Useful for identifying which buffers to read or edit.")
 :args '()
 :category "buffer"
 :confirm nil
 :include t)

;;; buffer.el ends here
