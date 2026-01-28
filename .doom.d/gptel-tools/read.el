;;; read.el --- GPTEL read tool -*- lexical-binding: t; coding: utf-8; -*-
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Homepage: https://github.com/yqrashawn/gptel-tools
;; Keywords: tools, read

;;; Commentary:
;;
;; Buffer and file reading utilities for GPTEL.
;;
;;; Code:

;;; Commentary:
;; Reading utilities for gptel-tools:
;;  - Read a file's content by file path.
;;  - Read a buffer's content by buffer name.

;;; Code:
(defun gptelt--image-file-p (file-path)
  "Check if FILE-PATH is an image file based on extension."
  (when (stringp file-path)
    (let ((ext (downcase (file-name-extension file-path ""))))
      (member ext '(".png" ".jpg" ".jpeg" ".gif" ".bmp" ".webp" ".svg" ".tiff" ".ico")))))

(defun gptelt--file-to-base64 (file-path)
  "Convert FILE-PATH to base64 string with data URI prefix."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file-path)
    (base64-encode-region (point-min) (point-max) t)
    (let* ((ext (downcase (file-name-extension file-path "")))
           (mime-type (cond
                       ((member ext '(".jpg" ".jpeg")) "image/jpeg")
                       ((string= ext ".png") "image/png")
                       ((string= ext ".gif") "image/gif")
                       ((string= ext ".svg") "image/svg+xml")
                       ((string= ext ".webp") "image/webp")
                       ((member ext '(".tif" ".tiff")) "image/tiff")
                       ((string= ext ".bmp") "image/bmp")
                       ((string= ext ".ico") "image/x-icon")
                       (t "application/octet-stream"))))
      (concat "data:" mime-type ";base64," (buffer-string)))))

(defun gptelt-read-file (file_path &optional offset limit)
  "Return up to LIMIT lines (default 2000) from FILE_PATH (must be absolute), starting at OFFSET (default 0). Return nil if not readable. The returned content is wrapped with ␂ at the start and ␃ at the end. Before the wrapped content, a description is included with the total lines in the file and the start/end line numbers of the wrapped content. For image files, returns the base64-encoded content without any wrappers or descriptions."
  (unless (and (stringp file_path) (file-name-absolute-p file_path))
    (error "file_path must be an absolute path"))
  (when (file-readable-p file_path)
    (if (gptelt--image-file-p file_path)
        ;; Handle image files: return direct base64 without wrapping
        (gptelt--file-to-base64 file_path)
      ;; Handle regular files
      ;; Convert string numbers to integers
      (when (stringp offset) (setq offset (string-to-number offset)))
      (when (stringp limit) (setq limit (string-to-number limit)))
      (let ((max-lines (if (and limit (< limit 300)) 300 (or limit 2000)))
            (line-offset (or offset 0)))
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
                (concat (format "[File path: %s]\n[Total lines: %d]\n[Content lines: %d-%d]\n"
                                file_path total-lines start-line end-line)
                        "\n␂" content "␃")))))))))

(comment
  (gptelt-read-file
   (expand-file-name "deps.edn" "~/.nixpkgs"))
  (gptelt-read-file
   (expand-file-name "~/Library/CloudStorage/Dropbox/Screenshots/CleanShot 2025-09-16 at 08.45.48@2x.png")))

(defun gptelt-read-buffer (buffer_name &optional offset limit)
  "Return up to LIMIT lines (default 2000) from BUFFER_NAME, starting at OFFSET (default 0). Errors if buffer not exists. The returned content is wrapped with ␂ at the start and ␃ at the end. Before the wrapped content, a description is included with the total lines in the buffer and the start/end line numbers of the wrapped content. For image files, returns the base64-encoded content without any wrappers or descriptions."
  ;; Convert string numbers to integers
  (when (stringp offset) (setq offset (string-to-number offset)))
  (when (stringp limit) (setq limit (string-to-number limit)))
  (let ((b (get-buffer buffer_name)))
    (unless b
      ;; Return helpful error with available buffers instead of throwing internal error
      (let ((available-buffers (when (fboundp '++workspace-current-project-buffers-info)
                                 (++workspace-current-project-buffers-info))))
        (error "Buffer `%s` not found.\n\nAvailable buffers in current project:\n%s"
               buffer_name
               (or available-buffers "No buffers found"))))
    (with-current-buffer b
      (when (buffer-file-name b)
        (+gptel-tool-revert-to-be-visited-buffer b))
      (when (llm-danger-buffer-p b) (error "User denied the read request"))
      (if-let ((file-name (buffer-file-name b)))
          (if (gptelt--image-file-p file-name)
              ;; Handle image files: return direct base64 without wrapping
              (gptelt--file-to-base64 file-name)
            ;; Handle regular files
            (let ((max-lines (if (and limit (< limit 300)) 300 (or limit 2000)))
                  (line-offset (or offset 0)))
              (save-excursion
                (let* ((total-lines (count-lines (point-min) (point-max)))
                       (start-line (1+ line-offset))
                       (end-line (min total-lines (+ line-offset max-lines))))
                  (goto-char (point-min))
                  (forward-line line-offset)
                  (let ((start (point)))
                    (forward-line max-lines)
                    (let ((content (buffer-substring-no-properties start (point))))
                      (concat (format "[Total lines: %d]\n[Content lines: %d-%d]\n[Buffer name: %s]\n[File path: %s]\n"
                                      total-lines start-line end-line buffer_name file-name)
                              "\n␂" content "␃"))))))
            ;; Buffer without associated file
            (let ((max-lines (if (and limit (< limit 300)) 300 (or limit 2000)))
                  (line-offset (or offset 0)))
              (save-excursion
                (let* ((total-lines (count-lines (point-min) (point-max)))
                       (start-line (1+ line-offset))
                       (end-line (min total-lines (+ line-offset max-lines))))
                  (goto-char (point-min))
                  (forward-line line-offset)
                  (let ((start (point)))
                    (forward-line max-lines)
                    (let ((content (buffer-substring-no-properties start (point))))
                      (concat (format "[Total lines: %d]\n[Content lines: %d-%d]\n[Buffer name: %s]\n"
                                      total-lines start-line end-line buffer_name)
                              "\n␂" content "␃")))))))))))

(comment
  (gptelt-read-buffer (current-buffer))
  (gptelt-read-buffer "none-existing-buffer"))

;; Register the file and buffer reading tools with gptel
(when (fboundp 'gptelt-make-tool)
  (gptelt-make-tool
   :name "read_file"
   :function #'gptelt-read-file
   :description
   "Read a file from the local filesystem. The file_path parameter must be an absolute path, not a relative path. By default, it reads up to 2000 lines starting from the beginning of the file. You can optionally specify a line offset and limit (especially handy for long files), but it's recommended to read the whole file by not providing these parameters. Any lines longer than 2000 characters will be truncated. IMPORTANT: Use this tool for reading specific file paths. For reading currently open Emacs buffers, use read_buffer instead as it's faster and doesn't require the full path.

The returned result includes a description line with the total lines in the file, and the start/end line numbers of the wrapped content. The content is wrapped with ␂ at the start and ␃ at the end."
   :args '((:name "file_path" :type string
            :description "The absolute path to the file to read (must be absolute, not relative)")
           (:name "offset" :type integer :optional t :minimum 0
            :description "The line number to start reading from. Only provide if the file is too large to read at once")
           (:name "limit" :type integer :optional t :minimum 300
            :description "The number of lines to read. Only provide if the file is too large to read at once. Min value is 300."))
   :category "read"
   :confirm nil
   :include t)

  (gptelt-make-tool
   :name "read_buffer"
   :function #'gptelt-read-buffer
   :description
   "Read a buffer by buffer name. Reads up to 2000 lines starting from the beginning by default. Optional: offset (line number to start at), limit (number of lines). This tool is optimized for reading currently open files in Emacs without needing the full file path. Use this instead of read_file when you know the buffer name, as it's faster and more convenient. To see available buffers, you can use the buffer management tools.

The returned result includes a description line with the total lines in the buffer, and the start/end line numbers of the wrapped content. The content is wrapped with ␂ at the start and ␃ at the end. For image files, returns the base64-encoded content without any wrappers or descriptions."
   :args '((:name "buffer_name" :type string
            :description "The buffer name to read")
           (:name "offset" :type integer :optional t :minimum 0
            :description "The line number to start reading from. Only provide if the buffer is too large to read at once")
           (:name "limit" :type integer :optional t :minimum 300
            :description "The number of lines to read. Only provide if the buffer is too large to read at once. Min value is 300."))
   :category "read"
   :confirm nil
   :include t)

  (gptelt-make-tool
   :name "read_multiple_files"
   :function #'gptelt-read-multiple-files
   :description
   "Read multiple files in one call with per-file offset and limit control. Returns a list where each element is either a successful read (with :file-path and :content) or a failed read (with :file-path and :error). This is more efficient than calling read_file multiple times when you need to read several files.

Each result in the returned list is a plist with:
- :file-path - The file path that was attempted
- :content - The file content (if successful, same format as read_file)
- :error - Error message (if failed, e.g., file not found, not readable, permission denied)"
   :args
   '((:name "file_requests"
      :type array
      :items (:type object
              :properties
              (:file_path
               (:type string
                :description "Absolute path to the file to read")
               :offset
               (:type integer
                :optional t
                :minimum 0
                :description "Line number to start reading from")
               :limit
               (:type integer
                :optional t
                :minimum 300
                :description "Number of lines to read"))
              :required ["file_path"]
              :description "File request object with file_path (required), offset (optional), and limit (optional)")
      :description "List of file requests to read"))
   :category "read"
   :confirm nil
   :include t))

(defun gptelt-read-multiple-files (file_requests)
  "Read multiple files and return their contents or errors.
FILE_REQUESTS is a list or vector where each element is a plist with:
  - :file-path (required) - absolute file path
  - :offset (optional) - line number to start reading from
  - :limit (optional) - number of lines to read
Returns a list of plists, each containing either:
  - :file-path, :content (successful read)
  - :file-path, :error (failed read with error message)"
  ;; Convert vector to list if needed
  (when (vectorp file_requests)
    (setq file_requests (append file_requests nil)))
  (unless (listp file_requests)
    (error "file_requests must be a list or vector"))
  (mapcar
   (lambda (req)
     (let* ((file-path (or (plist-get req :file_path)
                           (plist-get req :file-path)))
            (offset (or (plist-get req :offset)
                        (plist-get req :offset)))
            (limit (or (plist-get req :limit)
                       (plist-get req :limit))))
       (condition-case err
           (let ((content (gptelt-read-file file-path offset limit)))
             (if content
                 (list :file-path file-path :content content)
               (list :file-path file-path :error "File not readable")))
         (error (list :file-path file-path :error (error-message-string err))))))
   file_requests))

(comment
  (gptelt-read-multiple-files
   (list (list :file-path (expand-file-name "deps.edn" "~/.nixpkgs"))
         (list :file-path (expand-file-name "shadow-cljs.edn" "~/.nixpkgs")
               :offset 0
               :limit 10)
         (list :file-path (expand-file-name "nonexistent.txt" "~/.nixpkgs")))))

(comment
  (gptelt-read-buffer "read.el"))

(when (fboundp 'gptelt-make-tool)
  (gptelt-make-tool
   :name "read_multiple_buffers"
   :function #'gptelt-read-multiple-buffers
   :description
   "Read multiple buffers in one call with per-buffer offset and limit control. Returns a list where each element is either a successful read (with :buffer-name and :content) or a failed read (with :buffer-name and :error). This is more efficient than calling read_buffer multiple times when you need to read several buffers.

Each result in the returned list is a plist with:
- :buffer-name - The buffer name that was attempted
- :content - The buffer content (if successful, same format as read_buffer)
- :error - Error message (if failed, e.g., buffer not found)"
   :args
   '((:name "buffer_requests"
      :type array
      :items (:type object
              :properties (:buffer_name (:type string
                                         :description "The buffer name to read")
                                        :offset (:type integer
                                                 :optional t
                                                 :minimum 0
                                                 :description "Line number to start reading from")
                                        :limit (:type integer
                                                :optional t
                                                :minimum 300
                                                :description "Number of lines to read"))
              :required ["buffer_name"]
              :description "Buffer request object with buffer_name (required), offset (optional), and limit (optional)")
      :description "List of buffer requests to read"))
   :category "read"
   :confirm nil
   :include t))

(defun gptelt-read-multiple-buffers (buffer_requests)
  "Read multiple buffers and return their contents or errors.
BUFFER_REQUESTS is a list or vector where each element is a plist with:
  - :buffer-name (required) - buffer name
  - :offset (optional) - line number to start reading from
  - :limit (optional) - number of lines to read
Returns a list of plists, each containing either:
  - :buffer-name, :content (successful read)
  - :buffer-name, :error (failed read with error message)"
  ;; Convert vector to list if needed
  (when (vectorp buffer_requests)
    (setq buffer_requests (append buffer_requests nil)))
  (unless (listp buffer_requests)
    (error "buffer_requests must be a list or vector"))
  (mapcar
   (lambda (req)
     (let* ((buffer-name (or (plist-get req :buffer_name)
                             (plist-get req :buffer-name)))
            (offset (or (plist-get req :offset)
                        (plist-get req :offset)))
            (limit (or (plist-get req :limit)
                       (plist-get req :limit))))
       (condition-case err
           (let ((content (gptelt-read-buffer buffer-name offset limit)))
             (if content
                 (list :buffer-name buffer-name :content content)
               (list :buffer-name buffer-name :error "Buffer not found")))
         (error (list :buffer-name buffer-name :error (error-message-string err))))))
   buffer_requests))

(comment
  (gptelt-read-multiple-buffers
   (list (list :buffer-name "read.el")
         (list :buffer-name "edit-file.el"
               :offset 0
               :limit 50)
         (list :buffer-name "nonexistent-buffer"))))

;;; read.el ends here
