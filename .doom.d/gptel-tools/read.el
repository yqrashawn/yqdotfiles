;;; .nixpkgs/.doom.d/gptel-tools/read.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Reading utilities for gptel-tools:
;;  - Read a file's content by file path.
;;  - Read a buffer's content by buffer name.

;;; Code:

(require 'gptel)

(defun gptel-tools-read-file (file-path)
  "Return the contents of FILE-PATH as a string, or nil if file is not readable."
  (when (and (stringp file-path)
             (file-readable-p file-path))
    (with-temp-buffer
      (insert-file-contents file-path)
      (when (llm-danger-buffer-p) (error "User denied the read request"))
      (buffer-string))))

(defun gptel-tools-read-buffer (buffer-name)
  "Return the contents of the buffer named BUFFER-NAME as a string, or nil if buffer does not exist."
  (when-let ((b (get-buffer buffer-name)))
    (with-current-buffer b
      (when (llm-danger-buffer-p) (error "User denied the read request"))
      (buffer-string))))

;; Register the file and buffer reading tools with gptel
(when (fboundp 'gptel-make-tool)
  (gptel-make-tool
   :name "read-file"
   :function #'gptel-tools-read-file
   :description "Given a file-path, return the file content as a string (or nil if unreadable)."
   :args '((:name "file-path" :type string
            :description "absolute file path"))
   :category "emacs"
   :confirm nil
   :include t)
  (gptel-make-tool
   :name "read-buffer"
   :function #'gptel-tools-read-buffer
   :description "Given a buffer-name, return the buffer's content as a string (or nil if buffer doesn't exist)."
   :args '((:name "buffer-name" :type string
            :description "buffer name"))
   :category "emacs"
   :confirm nil
   :include t))

;;; read.el ends here
