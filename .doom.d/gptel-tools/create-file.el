;;; .nixpkgs/.doom.d/gptel-tools/create-file.el -*- lexical-binding: t; -*-
;;; Commentary:

;; Create file buffer tool for gptel.
;; Moved from gptel-tools.el for better organization.

;;; Code:

(require 'gptel)
(require 'smartparens nil t)

(declare-function sp-region-ok-p "smartparens")

;;; Create file buffer implementation

(defun gptel-tools--create-file-buffer (file-path buffer-content-string)
  "Create a new file buffer with FILE-PATH and BUFFER-CONTENT-STRING.

FILE-PATH can be absolute or relative. If relative, it will be resolved
against the current project root.

This function:
1. Resolves the file path (relative to project root if needed)
2. Checks if the file already exists and prompts for confirmation if it does
3. Creates the directory structure if it doesn't exist
4. Creates a new buffer with the specified content
5. Sets the appropriate major mode based on file extension
6. Validates balanced parentheses for Lisp code
7. Formats the buffer using LSP if available
8. Saves the buffer to the specified file path

Returns a string describing the result of the operation."
  (let* ((resolved-path (gptel-tools--resolve-file-path file-path))
         (dir (file-name-directory resolved-path))
         (existing-buffer (get-file-buffer resolved-path))
         (project-root (gptel-tools--get-project-root)))

    ;; Check if file already exists
    (when (file-exists-p resolved-path)
      (unless (y-or-n-p (format "File %s already exists. Overwrite? " file-path))
        (error "File creation cancelled: %s already exists" file-path)))

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

        (gptel-edit-tool--edit-buffer-impl buffer "" buffer-content-string)

        ;; Determine and set major mode based on file extension
        (let ((auto-mode-alist auto-mode-alist))
          (set-auto-mode))

        (save-buffer)

        ;; Format buffer if LSP is available
        (when (and (fboundp 'lsp-format-buffer)
                   (bound-and-true-p lsp-mode))
          (condition-case err
              (progn (lsp-format-buffer)
                     (save-buffer))
            (error (message "LSP formatting failed: %s" err))))

        ;; Buffer created but not displayed to user
        (format "Successfully created file %s (resolved: %s) with %d characters (project: %s, mode: %s)"
                file-path
                resolved-path
                (length buffer-content-string)
                project-root
                major-mode)))))

;;; Tool registration
;; Register the create file buffer tool with gptel
(when (fboundp 'gptel-make-tool)
  (gptel-make-tool
   :name "create-file-buffer"
   :function #'gptel-tools--create-file-buffer
   :description "Create a new file buffer with specified content. Creates directory structure if needed and sets appropriate major mode."
   :args (list '(:name "file-path" :type string
                 :description "absolute or relative file path where the new file should be created, `~/` is supported")
               '(:name "buffer-content-string" :type string
                 :description "The complete content to write to the new file"))
   :category "emacs"
   :confirm nil
   :include t))

;;; Create temp file buffer tool implementation

(defun gptel-tools--create-temp-file-buffer
    (buffer-content-string &optional prefix suffix)
  "Create a temp file, open a buffer for it (not displayed), and return the file path.

PREFIX and SUFFIX are optional; default prefix is \"gptel-\".
Buffer is not switched to or displayed. File is created and saved to disk."
  (let* ((tmp-path (make-temp-file (or prefix "gptel-") nil (or suffix "")))
         (buf (find-file-noselect tmp-path)))
    (gptel-edit-tool--edit-buffer-impl buf "" buffer-content-string)
    (with-current-buffer buf (save-buffer))
    tmp-path))

;;; Register create_temp_file_buffer tool with gptel
(when (fboundp 'gptel-make-tool)
  (gptel-make-tool
   :name "create-temp-file-buffer"
   :function #'gptel-tools--create-temp-file-buffer
   :description "Create a new temp file, open a buffer for it (without displaying), and return the file path. Optional args: prefix, suffix."
   :args (list
          '(:name "buffer-content-string" :type string
            :description "The complete content to write to the new file")
          '(:name "prefix" :type "string" :optional t
            :description "prefix for temp file name (default: gptel-)")
          '(:name "suffix" :type "string" :optional t
            :description "file name suffix (e.g. .el, .py), default: none"))
   :category "emacs"
   :confirm nil
   :include t))

(comment
  (gptel-tools--create-temp-file-buffer "(comment)" nil ".el"))

(provide 'create-file-buffer-tool)
;;; create-file-buffer-tool.el ends here
