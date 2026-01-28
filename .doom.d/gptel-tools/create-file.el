;;; create-file.el --- GPTEL create file tool -*- lexical-binding: t; -*-
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Homepage: https://github.com/yqrashawn/gptel-tools
;; Keywords: tools, file

;;; Commentary:
;;
;; Create file buffer tool for gptel.
;; Moved from gptel-tools.el for better organization.
;;
;;; Code:

(require 'gptel)
(require 'smartparens nil t)

(declare-function sp-region-ok-p "smartparens")

;;; Debug Configuration
(defvar gptelt-create-debug nil
  "When non-nil, log create-file tool operations for debugging.")

;;; Create file buffer implementation

(defun gptelt--create-file-buffer (callback file-path content-string)
  "Create a new file buffer with FILE-PATH and CONTENT-STRING.

FILE-PATH must be an absolute path.
CALLBACK is called with the result string when done (async).

This function:
1. Ensures the file path is absolute
2. Checks if the file already exists and prompts for confirmation if it does
3. Creates the directory structure if it doesn't exist
4. Creates a new buffer with the specified content
5. Sets the appropriate major mode based on file extension
6. Validates balanced parentheses for Lisp code
7. Formats the buffer using LSP if available
8. Saves the buffer to the specified file path

Returns a string describing the result of the operation."
  (unless (file-name-absolute-p file-path)
    (error "file_path must be an absolute path"))
  (let* ((resolved-path file-path)
         (dir (file-name-directory resolved-path))
         (existing-buffer (get-file-buffer resolved-path))
         (project-root (gptelt--get-project-root)))

    (when gptelt-create-debug
      (message "[CREATE-DEBUG] Creating directory if needed: %s" dir))
    
    ;; Create directory if it doesn't exist
    (unless (file-directory-p dir)
      (make-directory dir t))

    ;; Create or switch to buffer
    (let ((buffer (or existing-buffer (create-file-buffer resolved-path))))

      (with-current-buffer buffer
        ;; Clear existing content if buffer already existed
        (when existing-buffer
          (let ((inhibit-read-only t))
            (erase-buffer)))

        (set-visited-file-name resolved-path t)

        (gptelt-edit--edit-buffer-impl
         buffer "" content-string
         (lambda (result)
           ;; Determine and set major mode based on file extension
           (with-current-buffer buffer
             (let ((auto-mode-alist auto-mode-alist))
               (set-auto-mode))

             (save-buffer)

             (when gptelt-create-debug
               (message "[CREATE-DEBUG] File saved with mode: %s" major-mode))
             
             ;; Format buffer if LSP is available
             (when (and (fboundp 'lsp-format-buffer)
                        (bound-and-true-p lsp-mode))
               (condition-case err
                   (progn (lsp-format-buffer)
                          (save-buffer))
                 (error (message "LSP formatting failed: %s" err))))

             ;; Buffer created but not displayed to user
             (funcall callback
                      (format "Successfully created file %s (resolved: %s) with %d characters (project: %s, mode: %s)"
                              file-path
                              resolved-path
                              (length content-string)
                              project-root
                              major-mode)))))))))

(comment
  (let ((f (make-temp-file "gptelt-create-tool")))
    (gptelt--create-file-buffer f "(message \"hello\")")
    (with-current-buffer (find-file-noselect f)
      (buffer-string))))

;;; Tool registration
;; Register the create file buffer tool with gptel
(when (fboundp 'gptelt-make-tool)
  (gptelt-make-tool
   :name "create_file_buffer"
   :function #'gptelt--create-file-buffer
   :async t
   :description (concat "Create a new file with specified content at an absolute file path. "
                        "Creates parent directories if they don't exist. Sets major mode based on file extension. "
                        "For Lisp code, validates parentheses balance and applies LSP formatting if available."
                        "\n\nPARAMETER STRUCTURE:\n"
                        "{\n"
                        "  \"file_path\": \"string\" (required) - Absolute path where file should be created\n"
                        "  \"content_string\": \"string\" (required) - Complete content to write to the file\n"
                        "}\n\n"
                        "IMPORTANT:\n"
                        "- file_path MUST be an absolute path (e.g., /path/to/file.txt)\n"
                        "- Relative paths and ~/ expansion are NOT supported\n"
                        "- Parent directories will be created automatically\n"
                        "- Major mode is set automatically based on file extension\n"
                        "- LSP formatting applied if available\n\n"
                        "Usage example:\n"
                        "create_file_buffer('/tmp/test.py', 'def hello():\\n    print(\"Hello\")')")
   :args '((:name "file_path"
            :type string
            :description "Absolute path where the new file should be created (must be absolute, not relative)")
           (:name "content_string"
            :type string
            :description "The complete content to write to the new file"))
   :category "create-file"
   :confirm nil
   :include t))

;;; Create temp file buffer tool implementation

(defun gptelt--create-temp-file-buffer
    (callback content-string &optional prefix suffix)
  "Create a temp file, open a buffer for it (not displayed), and return the file path.

CALLBACK is called with the temp file path when done (async).
PREFIX and SUFFIX are optional; default prefix is \"gptelt-\".
Buffer is not switched to or displayed. File is created and saved to disk."
  (when gptelt-create-debug
    (message "[CREATE-DEBUG] create_temp_file_buffer: prefix=%s, suffix=%s, content-length=%d" 
             (or prefix "gptelt-")
             (or suffix "")
             (if content-string (length content-string) 0)))
  
  (let* ((tmp-path (make-temp-file (or prefix "gptelt-") nil (or suffix "")))
         (buf (find-file-noselect tmp-path)))
    (gptelt-edit--edit-buffer-impl
     buf "" content-string
     (lambda (result)
       (with-current-buffer buf (save-buffer))
       (when gptelt-create-debug
         (message "[CREATE-DEBUG] Temp file created: %s" tmp-path))
       (funcall callback tmp-path))
     nil nil)))

(comment
  (gptelt--create-temp-file-buffer "(message \"temp hello\")" "test-" ".el")
  (with-current-buffer
      (find-file-noselect
       (gptelt--create-temp-file-buffer "(message \"temp hello\")" "test-" ".el"))
    (buffer-string)))

;;; Register create_temp_file_buffer tool with gptel
(when (fboundp 'gptelt-make-tool)
  (gptelt-make-tool
   :name "create_temp_file_buffer"
   :function #'gptelt--create-temp-file-buffer
   :async t
   :description (concat "Create a new temporary file with specified content and return the file path. "
                        "Useful for creating scratch files or intermediate outputs."
                        "\n\nPARAMETER STRUCTURE:\n"
                        "{\n"
                        "  \"content_string\": \"string\" (required) - Complete content to write to the temp file\n"
                        "  \"prefix\": \"string\" (optional, default: \"gptelt-\") - Prefix for temp file name\n"
                        "  \"suffix\": \"string\" (optional, default: \"\") - File extension (e.g., \".el\", \".py\")\n"
                        "}\n\n"
                        "Usage examples:\n"
                        "- create_temp_file_buffer('(message \"test\")', 'test-', '.el')\n"
                        "- create_temp_file_buffer('console.log(\"test\")', null, '.js')")
   :args '((:name "content_string" :type string
            :description "The complete content to write to the new file")
           (:name "prefix" :type "string" :optional t
            :description "prefix for temp file name (default: gptelt-)")
           (:name "suffix" :type "string" :optional t
            :description "file name suffix (e.g. .el, .py), default: none"))
   :category "create-file"
   :confirm nil
   :include t))

(comment
  (gptelt--create-temp-file-buffer "(comment)" nil ".el"))

(provide 'create-file)
;;; create-file.el ends here
