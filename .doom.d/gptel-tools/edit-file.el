;;; .nixpkgs/.doom.d/gptel-tools/edit-tool.el -*- lexical-binding: t; -*-
;;; Commentary:

;; Improved implementation of gptel edit tool using ediff instead of smerge.
;; Inspired by and reuses patterns from gptel-rewrite.el.

;;; Code:

(require 'gptel)
(require 'ediff)
(require 'smartparens nil t)

(declare-function sp-region-ok-p "smartparens")
(declare-function ediff-setup-windows-plain "ediff-wind")

;;; Edit buffer
;;
;; Flow:
;; - Read the file if there's no buffer for it in Emacs
;; - Generate context based on file-path:
;;   - file-buffer
;;   - project-root with `++workspace-current-project-root'
;;   - buffer major-mode
;;   - buffer minor-modes
;; - If is Lisp (elisp, clojure, scheme ...) code, use `sp-region-ok-p' to check the new-string is balanced
;; - Use patch, ediff, smerge-mode and hydra-smerge/body to show diffs and let user decide
;; - Use `lsp-format-region' to format the changed region if `lsp-mode' is enabled in the buffer

;;; Variables
(defvar gptel-edit-tool--temp-buffers nil
  "List of temporary buffers created by edit tool for cleanup.")

;; Test comment added by edit_buffer tool
;; Test comment added by edit_file tool

;;; Utility functions (reused from gptel-tools.el)

(defun gptel-edit-tool--get-project-root ()
  "Get project root for current context."
  (if (fboundp '++workspace-current-project-root)
      (++workspace-current-project-root)
    (when (fboundp 'project-current)
      (when-let ((project (project-current)))
        (if (fboundp 'project-root)
            (project-root project)
          (car (project-roots project)))))))

(defun gptel-edit-tool--resolve-file-path (file-path)
  "Resolve FILE-PATH to absolute path.
If FILE-PATH is relative, resolve it against the current project root."
  (if (file-name-absolute-p file-path)
      file-path
    (let ((project-root (gptel-edit-tool--get-project-root)))
      (if project-root
          (expand-file-name file-path project-root)
        (expand-file-name file-path default-directory)))))

(defun gptel-edit-tool--get-buffer-context (file-path)
  "Generate context information for FILE-PATH including buffer, project, and mode info."
  (let* ((resolved-path (gptel-edit-tool--resolve-file-path file-path))
         (buffer (or (get-file-buffer resolved-path)
                     (when (file-exists-p resolved-path)
                       (find-file-noselect resolved-path))))
         (project-root (gptel-edit-tool--get-project-root))
         (major-mode (when buffer (buffer-local-value 'major-mode buffer)))
         (minor-modes (when buffer
                        (buffer-local-value 'minor-mode-list buffer))))
    (list :buffer buffer
          :file-path resolved-path
          :original-path file-path
          :project-root project-root
          :major-mode major-mode
          :minor-modes minor-modes
          :exists-p (and buffer (get-file-buffer resolved-path)))))

(defun gptel-edit-tool--is-lisp-mode-p (mode)
  "Check if MODE is a Lisp-related mode."
  (memq mode '(emacs-lisp-mode lisp-mode clojure-mode scheme-mode
               clojurescript-mode clojurec-mode common-lisp-mode
               lisp-interaction-mode)))

(defun gptel-edit-tool--check-buffer-balanced-parens (buffer)
  "Check if BUFFER has balanced parentheses for Lisp modes.
Returns (BALANCED-P . ERROR-MESSAGE)."
  (let ((major-mode (buffer-local-value 'major-mode buffer)))
    (if (and (gptel-edit-tool--is-lisp-mode-p major-mode)
             (fboundp 'sp-region-ok-p))
        (with-current-buffer buffer
          (condition-case err
              (if (sp-region-ok-p (point-min) (point-max))
                  (cons t nil)
                (cons nil "Buffer has unbalanced parentheses"))
            (error (cons nil (error-message-string err)))))
      (cons t nil)))) ; Non-Lisp modes always pass

;;; Edit buffer preparation (adapted from gptel-rewrite)

(defun gptel-edit-tool--prepare-edit-buffer (buffer old-string new-string)
  "Prepare new buffer with edit applied and return it.
This is used for ediff purposes, adapted from gptel--rewrite-prepare-buffer.

BUFFER is the original buffer.
OLD-STRING is the text to be replaced.
NEW-STRING is the replacement text."
  (let* ((original-buffer buffer)
         (original-file-name (buffer-file-name original-buffer))
         (newbuf-name (format "*gptel-edit-%s*" (buffer-name original-buffer)))
         (newbuf (get-buffer-create newbuf-name))
         (inhibit-read-only t)
         (inhibit-message t))

    ;; Track temp buffer for cleanup
    (push newbuf gptel-edit-tool--temp-buffers)

    ;; Copy original buffer content to new buffer
    (with-current-buffer newbuf
      (erase-buffer)
      (insert-buffer-substring original-buffer)
      (when original-file-name
        (set-visited-file-name original-file-name t))
      (set-auto-mode)

      ;; Apply the edit: replace old-string with new-string
      (goto-char (point-min))
      (if (search-forward old-string nil t)
          (let ((start (match-beginning 0))
                (end (match-end 0)))
            (goto-char start)
            (delete-region start end)
            (insert new-string)

            ;; Format the changed region if LSP is available
            (when (and (fboundp 'lsp-format-region)
                       (bound-and-true-p lsp-mode))
              (condition-case err
                  (lsp-format-region start (+ start (length new-string)))
                (error (message "LSP formatting failed: %s" err)))))
        (error "old-string not found in buffer content")))
    newbuf))

;;; Direct edit application

(defun gptel-edit-tool--apply-edit-directly (buffer old-string new-string)
  "Apply edit to BUFFER by directly replacing OLD-STRING with NEW-STRING.
Returns a message describing the result of the operation."
  (let* ((original-buffer buffer)
         (original-point (with-current-buffer original-buffer (point)))
         (replacement-start nil)
         (replacement-end nil))

    ;; Apply the edit directly in the original buffer
    (with-current-buffer original-buffer
      (save-excursion
        (goto-char (point-min))
        (if (search-forward old-string nil t)
            (let ((start (match-beginning 0))
                  (end (match-end 0)))
              (setq replacement-start start)
              (goto-char start)
              (delete-region start end)
              (insert new-string)
              (setq replacement-end (+ start (length new-string)))

              (save-buffer)
              ;; Format the changed region if LSP is available
              (when (and (fboundp 'lsp-format-region)
                         (bound-and-true-p lsp-mode))
                (condition-case err
                    (lsp-format-region replacement-start replacement-end)
                  (error (message "LSP formatting failed: %s" err)))))
          (error "old-string not found in buffer content")))

      ;; Restore point, adjusting for text length changes if needed
      (let ((point-adjustment (- (length new-string) (length old-string))))
        (if (and replacement-start (> original-point replacement-start))
            (goto-char (+ original-point point-adjustment))
          (goto-char original-point))))

    (format "Successfully replaced text in %s. Changed %d characters to %d characters."
            (buffer-name original-buffer)
            (length old-string)
            (length new-string))))

;;; Shared edit logic
(defun gptel-edit-tool--edit-buffer-impl (buffer old-string new-string)
  "Shared implementation for editing BUFFER by replacing OLD-STRING with NEW-STRING.
Returns a string describing the result of the operation."
  (let ((major-mode (buffer-local-value 'major-mode buffer))
        (buffer-name (buffer-name buffer))
        (temp-buffer nil))

    ;; Check if old-string exists in buffer
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (unless (search-forward old-string nil t)
          (error "old-string not found in buffer: %s" buffer-name))))

    ;; For Lisp code, check balance in a temp buffer after replacement
    (when (gptel-edit-tool--is-lisp-mode-p major-mode)
      (unwind-protect
          (progn
            ;; Create temp buffer with the replacement applied
            (setq temp-buffer (generate-new-buffer " *gptel-balance-check*"))
            (with-current-buffer temp-buffer
              ;; Copy original buffer content
              (insert-buffer-substring buffer)
              ;; Set the same major mode
              (funcall major-mode)
              ;; Apply the replacement
              (goto-char (point-min))
              (when (search-forward old-string nil t)
                (let ((start (match-beginning 0))
                      (end (match-end 0)))
                  (goto-char start)
                  (delete-region start end)
                  (insert new-string)))
              ;; Check balance of the entire buffer
              (pcase-let ((`(,balanced-p . ,error-msg)
                           (gptel-edit-tool--check-buffer-balanced-parens temp-buffer)))
                (unless balanced-p
                  (error "Buffer would have unbalanced parentheses after edit: %s" error-msg))))
            ;; Cleanup temp buffer
            (when temp-buffer
              (kill-buffer temp-buffer))))

      ;; Apply edit directly
      (gptel-edit-tool--apply-edit-directly buffer old-string new-string))))

;;; Main edit functions
(defun gptel-edit-tool-edit-buffer (buffer-name old-string new-string)
  "Edit buffer by replacing OLD-STRING with NEW-STRING in BUFFER-NAME.

BUFFER-NAME should be the name of an existing buffer.

This function:
1. Gets the buffer by name
2. Validates balanced parentheses for Lisp code
3. Applies the edit directly to the buffer
4. Formats the changed region if LSP is available

Returns a string describing the result of the operation."
  (let ((buffer (get-buffer buffer-name)))
    (unless buffer
      (error "Buffer not found: %s" buffer-name))

    (gptel-edit-tool--edit-buffer-impl buffer old-string new-string)))

(defun gptel-edit-tool-edit-file (file-path old-string new-string)
  "Edit file by replacing OLD-STRING with NEW-STRING in FILE-PATH.

FILE-PATH can be absolute or relative. If relative, it will be resolved
against the current project root.

This function:
1. Resolves the file path (relative to project root if needed)
2. Reads the file if no buffer exists for it
3. Generates context (project root, major mode, minor modes)
4. Validates balanced parentheses for Lisp code
5. Applies the edit directly to the buffer
6. Formats the changed region if LSP is available

Returns a string describing the result of the operation."
  (let* ((context (gptel-edit-tool--get-buffer-context file-path))
         (buffer (plist-get context :buffer))
         (resolved-path (plist-get context :file-path))
         (original-path (plist-get context :original-path)))

    (unless buffer
      (error "Could not open or create buffer for file: %s (resolved to: %s)"
             original-path resolved-path))

    (gptel-edit-tool--edit-buffer-impl buffer old-string new-string)))

;;; Tool registration

;; Register the edit_buffer tool with gptel
(when (fboundp 'gptel-make-tool)
  (gptel-make-tool
   :name "edit-buffer"
   :function #'gptel-edit-tool-edit-buffer
   :description "Edit a buffer by replacing old text with new text. When the file is a Lisp-family language, both old and new string are expected to be balanced s-expressions (like edit form, not arbitrary fragment) -- an error will be raised if either is unbalanced."
   :args (list '(:name "buffer-name" :type string
                 :description "The name of the buffer to edit")
               '(:name "old-string" :type string
                 :description "The exact text to be replaced")
               '(:name "new-string" :type string
                 :description "The new text to replace the old text with"))
   :category "emacs"
   :confirm nil
   :include t))

;; Register the edit_file tool with gptel
(when (fboundp 'gptel-make-tool)
  (gptel-make-tool
   :name "edit-file"
   :function #'gptel-edit-tool-edit-file
   :description "Edit a file by replacing old text with new text. Opens the file in a buffer if not already open. When the file is a Lisp-family language, both old and new string are expected to be balanced s-expressions (like edit form, not arbitrary fragment) -- an error will be raised if either is unbalanced."
   :args (list '(:name "file-path" :type string
                 :description "absolute or relative file path to the file to edit, `~/` is supported")
               '(:name "old-string" :type string
                 :description "The exact text to be replaced")
               '(:name "new-string" :type string
                 :description "The new text to replace the old text with"))
   :category "emacs"
   :confirm nil
   :include t))

;;; Cleanup function for manual use
(defun gptel-edit-tool-cleanup ()
  "Manually clean up any remaining temporary buffers from edit operations."
  (interactive)
  (gptel-edit-tool--cleanup-temp-buffers))

(provide 'gptel-edit-tool)
;;; gptel-edit-tool.el ends here
