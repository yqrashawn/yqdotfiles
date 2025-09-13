;;; .nixpkgs/.doom.d/gptel-tools/read.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Reading utilities for gptel-tools:
;;  - Read a file's content by file path.
;;  - Read a buffer's content by buffer name.

;;; Code:
(defun gptelt-read-file (file_path &optional offset limit)
  "Return up to LIMIT lines (default 2000) from FILE_PATH (must be absolute), starting at OFFSET (default 0). Return nil if not readable."
  (unless (and (stringp file_path) (file-name-absolute-p file_path))
    (error "file_path must be an absolute path"))
  (let ((max-lines (or limit 2000))
        (line-offset (or offset 0)))
    (when (file-readable-p file_path)
      (with-temp-buffer
        (insert-file-contents file_path)
        (when (llm-danger-buffer-p) (error "User denied the read request"))
        (goto-char (point-min))
        (forward-line line-offset)
        (let ((start (point)))
          (forward-line max-lines)
          (buffer-substring-no-properties start (point)))))))

(defun gptelt-read-buffer (buffer_name &optional offset limit)
  "Return up to LIMIT lines (default 2000) from BUFFER_NAME, starting at OFFSET (default 0). Nil if not exists."
  (let ((max-lines (or limit 2000))
        (line-offset (or offset 0)))
    (when-let ((b (get-buffer buffer_name)))
      (with-current-buffer b
        (when (llm-danger-buffer-p) (error "User denied the read request"))
        (goto-char (point-min))
        (forward-line line-offset)
        (let ((start (point)))
          (forward-line max-lines)
          (buffer-substring-no-properties start (point)))))))

;; Register the file and buffer reading tools with gptel
(when (fboundp 'gptelt-make-tool)
  (gptelt-make-tool
   :name "read_file"
   :function #'gptelt-read-file
   :description (concat "Read a file from the local filesystem. "
                        "The file_path parameter must be an absolute path. "
                        "Reads up to 2000 lines starting from the beginning of the file by default. "
                        "Optional: offset (line number to start at), limit (number of lines)")
   :args '((:name "file_path" :type string
            :description "The absolute path to the file to read (must be absolute, not relative)")
           (:name "offset" :type integer :optional t
            :description "The line number to start reading from. Only provide if the file is too large to read at once")
           (:name "limit" :type integer :optional t
            :description "The number of lines to read. Only provide if the file is too large to read at once."))
   :category "emacs"
   :confirm nil
   :include t)
  (gptelt-make-tool
   :name "read_buffer"
   :function #'gptelt-read-buffer
   :description (concat "Read a buffer by buffer name. "
                        "Reads up to 2000 lines starting from the beginning by default. "
                        "Optional: offset (line number to start at), limit (number of lines)")
   :args '((:name "buffer_name" :type string
            :description "The buffer name to read")
           (:name "offset" :type integer :optional t
            :description "The line number to start reading from. Only provide if the buffer is too large to read at once")
           (:name "limit" :type integer :optional t
            :description "The number of lines to read. Only provide if the buffer is too large to read at once."))
   :category "emacs"
   :confirm nil
   :include t))

;;; read.el ends here
