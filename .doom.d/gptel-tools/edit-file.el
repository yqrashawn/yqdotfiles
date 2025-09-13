;;; .nixpkgs/.doom.d/gptel-tools/edit-tool.el -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(require 'gptel)
(require 'smartparens nil t)

(declare-function sp-region-ok-p "smartparens")

;;; Variables
(defvar gptel-edit-tool--temp-buffers nil
  "List of temporary buffers created by edit tool for cleanup.")


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
  "Generate context information for FILE-PATH (must be absolute path) including buffer, project, and mode info."
  (unless (file-name-absolute-p file-path)
    (error "file-path must be an absolute path"))
  (let* ((resolved-path file-path)
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

;;; Edit buffer preparation (adapted from gptel-rewrite)
(defun gptel-edit-tool--prepare-edit-buffer (buffer old-string new-string)
  "Prepare new buffer with edit applied and return it.

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
  "editing BUFFER by replacing OLD-STRING with NEW-STRING.

Returns a string describing the result of the operation."
  (let ((mj-mode (buffer-local-value 'major-mode buffer))
        (buf-name (buffer-name buffer))
        temp-buffer
        rst-buffer-string
        (edit-allowed t)
        unbalance-error)
    ;; Check if old-string exists in buffer
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (unless (search-forward old-string nil t)
          (error "old-string not found in buffer: %s" buf-name))))
    ;; For Lisp code, check balance in temp buffer after replacement (refuse edit if unbalanced)
    (when (gptel-edit-tool--is-lisp-mode-p mj-mode)
      (unwind-protect
          (progn
            ;; Create temp buffer with the replacement applied
            (setq temp-buffer (generate-new-buffer " *gptel-balance-check*"))
            (with-current-buffer temp-buffer
              (erase-buffer)
              ;; Copy original buffer content
              (insert-buffer-substring buffer)
              ;; Set the same major mode
              mj-mode
              (funcall mj-mode)
              major-mode
              ;; Apply the replacement
              (goto-char (point-min))
              (when (search-forward old-string nil t)
                (let ((start (match-beginning 0))
                      (end (match-end 0)))
                  (goto-char start)
                  (delete-region start end)
                  (insert new-string)))
              ;; Check balance (smartparens then parinfer)
              (pcase-let
                  ((`(,balanced-p . ,error-msg)
                    (gptel-tools--check-buffer-balanced-parens temp-buffer)))
                (if balanced-p
                    (setq rst-buffer-string (buffer-string))
                  (setq
                   edit-allowed nil
                   unbalance-error
                   (or error-msg "Buffer would have unbalanced parentheses after edit and could not be auto-balanced."))))))
        (when temp-buffer (kill-buffer temp-buffer))))
    (if (not edit-allowed)
        (error "%s" unbalance-error)
      ;; Non-Lisp or balanced, so apply edit directly
      (let ((rst-message
             (if rst-buffer-string
                 (gptel-tools--replace-buffer-directly buffer rst-buffer-string)
               (gptel-edit-tool--apply-edit-directly buffer old-string new-string))))
        (+gptel-context-add-buffer buffer)
        rst-message))))

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

FILE-PATH must be an absolute path.

This function:
1. Ensures file-path is absolute
2. Reads the file if no buffer exists for it
3. Generates context (project root, major mode, minor modes)
4. Validates balanced parentheses for Lisp code
5. Applies the edit directly to the buffer
6. Formats the changed region if LSP is available

Returns a string describing the result of the operation."
  (unless (file-name-absolute-p file-path)
    (error "file-path must be an absolute path"))
  (let* ((context (gptel-edit-tool--get-buffer-context file-path))
         (buffer (plist-get context :buffer))
         (resolved-path (plist-get context :file-path))
         (original-path (plist-get context :original-path)))

    (unless buffer
      (error "Could not open or create buffer for file: %s (resolved to: %s)"
             original-path resolved-path))

    (gptel-edit-tool--edit-buffer-impl buffer old-string new-string)))

;;; Tool registration
(when (fboundp 'gptel-make-tool)
  (gptel-make-tool
   :name "edit_buffer"
   :function #'gptel-edit-tool-edit-buffer
   :description "Edit a buffer by replacing old text with new text."
   :args (list '(:name "buffer_name" :type string
                 :description "The name of the buffer to edit")
               '(:name "old_string" :type string
                 :description "The exact text to be replaced")
               '(:name "new_string" :type string
                 :description "The new text to replace the old text with"))
   :category "emacs"
   :confirm nil
   :include t))

(when (fboundp 'gptel-make-tool)
  (gptel-make-tool
   :name "edit_file"
   :function #'gptel-edit-tool-edit-file
   :description "Edit a file by replacing old text with new text. Only accepts absolute file paths."
   :args (list '(:name "file_path" :type string
                 :description "Absolute path to the file to edit (must be absolute, not relative)")
               '(:name "old_string" :type string
                 :description "The exact text to be replaced")
               '(:name "new_string" :type string
                 :description "The new text to replace the old text with"))
   :category "emacs"
   :confirm nil
   :include t))


;;; Multi-edit logic

(defun gptel-edit-tool--multi-edit-buffer-impl (buffer edit-list)
  "Apply multiple edits to BUFFER.
EDIT-LIST is a list of cons cells (OLD . NEW), applied sequentially.
For Lisp buffers, checks parentheses balance after all edits.
Returns a string describing the result."
  (let ((mj-mode (buffer-local-value 'major-mode buffer))
        (buf-name (buffer-name buffer))
        (edit-allowed t)
        (unbalance-error nil)
        (applied-count 0)
        (original-point (with-current-buffer buffer (point))))
    ;; Apply all edits
    (with-current-buffer buffer
      (save-excursion
        (dolist (edit edit-list)
          (let ((old (car edit))
                (new (cdr edit)))
            (goto-char (point-min))
            (if (search-forward old nil t)
                (progn
                  (let ((start (match-beginning 0))
                        (end (match-end 0)))
                    (goto-char start)
                    (delete-region start end)
                    (insert new))
                  (setq applied-count (1+ applied-count)))
              (error "old-string not found: %s" old))))))
    ;; For Lisp, check balance after all edits
    (when (gptel-edit-tool--is-lisp-mode-p mj-mode)
      (let (temp-buffer)
        (unwind-protect
            (progn
              (setq temp-buffer (generate-new-buffer " *gptel-multi-balance-check*"))
              (with-current-buffer temp-buffer
                (erase-buffer)
                (insert-buffer-substring buffer)
                (funcall mj-mode)
                (pcase-let
                    ((`(,balanced-p . ,error-msg)
                      (gptel-tools--check-buffer-balanced-parens temp-buffer)))
                  (unless balanced-p
                    (setq edit-allowed nil
                          unbalance-error (or error-msg
                                              "Buffer would have unbalanced parentheses after multi-edit."))))))
          (when temp-buffer (kill-buffer temp-buffer)))))
    (if (not edit-allowed)
        (error "%s" unbalance-error)
      ;; Save buffer and format if lsp available
      (with-current-buffer buffer
        (save-buffer)
        (when (and (fboundp 'lsp-format-buffer)
                   (bound-and-true-p lsp-mode))
          (condition-case err
              (lsp-format-buffer)
            (error (message "LSP formatting failed: %s" err)))))
      (format "Successfully applied %d edits to buffer: %s" applied-count buf-name))))

;;; Public multi-edit entrypoints

(defun gptel-edit-tool-multi-edit-buffer (buffer-name edit-list)
  "Apply multiple edits to BUFFER-NAME.
EDIT-LIST is a list of cons cells (OLD . NEW), each applied sequentially.
For Lisp code, checks balance after all edits.
Returns a string describing the result."
  (let ((buffer (get-buffer buffer-name)))
    (unless buffer
      (error "Buffer not found: %s" buffer-name))
    (gptel-edit-tool--multi-edit-buffer-impl buffer edit-list)))

(defun gptel-edit-tool-multi-edit-file (file-path edit-list)
  "Apply multiple edits to FILE-PATH.
EDIT-LIST is a list of cons cells (OLD . NEW), each applied sequentially.
For Lisp code, checks balance after all edits.
Returns a string describing the result."
  (let* ((context (gptel-edit-tool--get-buffer-context file-path))
         (buffer (plist-get context :buffer))
         (resolved-path (plist-get context :file-path))
         (original-path (plist-get context :original-path)))
    (unless buffer
      (error "Could not open or create buffer for file: %s (resolved to: %s)"
             original-path resolved-path))
    (gptel-edit-tool--multi-edit-buffer-impl buffer edit-list)))

;;; Tool registration for multi-edit

(when (fboundp 'gptel-make-tool)
  (gptel-make-tool
   :name "multi_edit_buffer"
   :function #'gptel-edit-tool-multi-edit-buffer
   :description "Apply multiple edits to a buffer by replacing a list of old texts with new texts, sequentially. Each edit is a (old_string . new_string) pair."
   :args (list '(:name "buffer_name" :type string
                 :description "The name of the buffer to edit")
               '(:name "edit_list"
                 :type array
                 :items (:type object
                         :properties (:old_string (:type string :description "The exact text to be replaced")
                                      :new_string (:type string :description "The new text to replace the old text with"))
                         :required ["old_string" "new_string"]
                         :description "An object with old_string and new_string fields for each edit.")
                 :description "List of edits: each element is an object with old_string and new_string fields, applied sequentially."))
   :category "emacs"
   :confirm nil
   :include t))

(when (fboundp 'gptel-make-tool)
  (gptel-make-tool
   :name "multi_edit_file"
   :function #'gptel-edit-tool-multi-edit-file
   :description "Apply multiple edits to a file by replacing a list of old texts with new texts, sequentially. Each edit is a (old_string . new_string) pair. The file is opened if not already, all edits are applied in order, and saved."
   :args (list '(:name "file_path" :type string
                 :description "absolute or relative file path to the file to edit, `~/` is supported")
               '(:name "edit_list"
                 :type array
                 :items (:type object
                         :properties (:old_string (:type string :description "The exact text to be replaced")
                                      :new_string (:type string :description "The new text to replace the old text with"))
                         :required ["old_string" "new_string"]
                         :description "An object with old_string and new_string fields for each edit.")
                 :description "List of edits: each element is an object with old_string and new_string fields, applied sequentially."))
   :category "emacs"
   :confirm nil
   :include t))


;;; gptel-edit-tool.el ends here
