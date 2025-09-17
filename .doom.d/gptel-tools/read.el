;;; .nixpkgs/.doom.d/gptel-tools/read.el -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:
;; Reading utilities for gptel-tools:
;;  - Read a file's content by file path.
;;  - Read a buffer's content by buffer name.

;;; Code:
(defun gptelt-read-file (file_path &optional offset limit)
  "Return up to LIMIT lines (default 2000) from FILE_PATH (must be absolute), starting at OFFSET (default 0). Return nil if not readable. The returned content is wrapped with ␂ at the start and ␃ at the end. Before the wrapped content, a description is included with the total lines in the file and the start/end line numbers of the wrapped content."
  (unless (and (stringp file_path) (file-name-absolute-p file_path))
    (error "file_path must be an absolute path"))
  (let ((max-lines (or limit 2000))
        (line-offset (or offset 0)))
    (when (file-readable-p file_path)
      (with-temp-buffer
        (insert-file-contents file_path)
        (when (llm-danger-buffer-p) (error "User denied the read request"))
        (let* ((total-lines (count-lines (point-min) (point-max)))
               (start-line (1+ line-offset))
               (end-line (min total-lines (+ line-offset max-lines))))
          (goto-char (point-min))
          (forward-line line-offset)
          (let ((start (point)))
            (forward-line max-lines)
            (let ((content (buffer-substring-no-properties start (point))))
              (concat (format "[Total lines: %d]\n[Content lines: %d-%d]\n[File path: %s]\n"
                              total-lines start-line end-line file_path)
                      "\n␂" content "␃\n"))))))))

(comment
  (gptelt-read-file
   (expand-file-name "deps.edn" "~/.nixpkgs")))

(defun gptelt-read-buffer (buffer_name &optional offset limit)
  "Return up to LIMIT lines (default 2000) from BUFFER_NAME, starting at OFFSET (default 0). Nil if not exists. The returned content is wrapped with ␂ at the start and ␃ at the end. Before the wrapped content, a description is included with the total lines in the buffer and the start/end line numbers of the wrapped content."
  (let ((max-lines (or limit 2000))
        (line-offset (or offset 0)))
    (when-let ((b (get-buffer buffer_name)))
      (with-current-buffer b
        (when (llm-danger-buffer-p) (error "User denied the read request"))
        (save-excursion
          (let* ((total-lines (count-lines (point-min) (point-max)))
                 (start-line (1+ line-offset))
                 (end-line (min total-lines (+ line-offset max-lines))))
            (goto-char (point-min))
            (forward-line line-offset)
            (let ((start (point)))
              (forward-line max-lines)
              (let ((content (buffer-substring-no-properties start (point))))
                (concat (format "[Total lines: %d]\n[Content lines: %d-%d]\n[Buffer name: %s]\n%s"
                                total-lines start-line end-line buffer_name
                                (if-let ((buf-file (buffer-file-name b)))
                                    (format "[File path: %s]" buf-file)
                                  ""))
                        "\n␂" content "␃\n")))))))))

(comment
  (gptelt-read-buffer (current-buffer)))

;; Register the file and buffer reading tools with gptel
(when (fboundp 'gptelt-make-tool)
  (gptelt-make-tool
   :name "read_file"
   :function #'gptelt-read-file
   :description (concat "Read a file from the local filesystem. The file_path parameter must be an absolute path, not a relative path. "
                        "By default, it reads up to 2000 lines starting from the beginning of the file. "
                        "You can optionally specify a line offset and limit (especially handy for long files), "
                        "but it's recommended to read the whole file by not providing these parameters. "
                        "Any lines longer than 2000 characters will be truncated. "
                        "IMPORTANT: Use this tool for reading specific file paths. For reading currently open Emacs buffers, "
                        "use read_buffer instead as it's faster and doesn't require the full path.\n\n"
                        "The returned result includes a description line with the total lines in the file, "
                        "and the start/end line numbers of the wrapped content. "
                        "The content is wrapped with ␂ at the start and ␃ at the end.")
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
   :description (concat "Read a buffer by buffer name. Reads up to 2000 lines starting from the beginning by default. "
                        "Optional: offset (line number to start at), limit (number of lines). "
                        "This tool is optimized for reading currently open files in Emacs without needing the full file path. "
                        "Use this instead of read_file when you know the buffer name, as it's faster and more convenient. "
                        "To see available buffers, you can use the buffer management tools.\n\n"
                        "The returned result includes a description line with the total lines in the buffer, "
                        "and the start/end line numbers of the wrapped content. "
                        "The content is wrapped with ␂ at the start and ␃ at the end.")
   :args '((:name "buffer_name" :type string
            :description "The buffer name to read")
           (:name "offset" :type integer :optional t
            :description "The line number to start reading from. Only provide if the buffer is too large to read at once")
           (:name "limit" :type integer :optional t
            :description "The number of lines to read. Only provide if the buffer is too large to read at once."))
   :category "emacs"
   :confirm nil
   :include t))

(comment
  (gptelt-read-buffer "read.el"))

;;; read.el ends here
