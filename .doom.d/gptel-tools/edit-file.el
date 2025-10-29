;;; .nixpkgs/.doom.d/gptel-tools/edit-tool.el -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(require 'gptel)
(require 'smartparens nil t)

(declare-function sp-region-ok-p "smartparens")

;;; Variables
(defvar gptelt-edit--temp-buffers nil
  "List of temporary buffers created by edit tool for cleanup.")


;;; Utility functions
(defun gptelt-edit--get-project-root ()
  "Get project root for current context."
  (if (fboundp '++workspace-current-project-root)
      (++workspace-current-project-root)
    (when (fboundp 'project-current)
      (when-let ((project (project-current)))
        (if (fboundp 'project-root)
            (project-root project)
          (car (project-roots project)))))))

(defun gptelt-edit--resolve-file-path (file-path)
  "Resolve FILE-PATH to absolute path.
If FILE-PATH is relative, resolve it against the current project root."
  (let ((file-path (expand-file-name file-path)))
    (unless (file-name-absolute-p file-path)
      (error "file_path must be an absolute path"))
    file-path))

(defun gptelt-edit--get-buffer-context (file-path)
  "Generate context information for FILE-PATH (must be absolute path) including buffer, project, and mode info."
  (let* ((resolved-path (gptelt-edit--resolve-file-path file-path))
         (buffer (or (get-file-buffer resolved-path)
                     (when (file-exists-p resolved-path)
                       (find-file-noselect resolved-path))))
         (project-root (gptelt-edit--get-project-root))
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

(defun gptelt-edit--is-lisp-mode-p (mode)
  "Check if MODE is a Lisp-related mode."
  (memq mode '(emacs-lisp-mode lisp-mode clojure-mode scheme-mode
               clojurescript-mode clojurec-mode common-lisp-mode
               lisp-interaction-mode)))

(defun gptelt-edit--prepare-edit-buffer (buffer old-string new-string)
  "Prepare new buffer with edit applied and return it.

BUFFER is the original buffer.
OLD-STRING is the text to be replaced.
NEW-STRING is the replacement text."
  (let* ((original-buffer buffer)
         (original-file-name (buffer-file-name original-buffer))
         (newbuf-name (format "*gptelt-edit-%s*" (buffer-name original-buffer)))
         (newbuf (get-buffer-create newbuf-name))
         (inhibit-read-only t)
         (inhibit-message t))

    ;; Track temp buffer for cleanup
    (push newbuf gptelt-edit--temp-buffers)

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
        (error "old_string not found in the original content, CHECK CAREFULLY.")))
    newbuf))

;;; Direct edit application

(defun gptelt-edit--apply-edit-directly (buffer old-string new-string &optional replace-all)
  "Apply edit to BUFFER by directly replacing OLD-STRING with NEW-STRING.
If REPLACE-ALL is non-nil, replace all occurrences. Otherwise, only the first.
Returns a message describing the result of the operation."
  (let* ((original-buffer buffer)
         (original-point (with-current-buffer original-buffer (point)))
         (replacement-count 0)
         (replacement-start nil)
         (replacement-end nil))

    ;; Apply the edit directly in the original buffer
    (with-current-buffer original-buffer
      (save-excursion
        (goto-char (point-min))
        (if replace-all
            (while (search-forward old-string nil t)
              (let ((start (match-beginning 0))
                    (end (match-end 0)))
                (setq replacement-start (or replacement-start start))
                (goto-char start)
                (delete-region start end)
                (insert new-string)
                (setq replacement-end (+ start (length new-string)))
                (setq replacement-count (1+ replacement-count))))
          (if (search-forward old-string nil t)
              (let ((start (match-beginning 0))
                    (end (match-end 0)))
                (setq replacement-start start)
                (goto-char start)
                (delete-region start end)
                (insert new-string)
                (setq replacement-end (+ start (length new-string)))
                (setq replacement-count 1))
            (error "old_string not found in original content, CHECK CAREFULLY.")))

        (save-buffer)
        ;; Format the changed region if LSP is available
        (when (and (fboundp 'lsp-format-region)
                   (bound-and-true-p lsp-mode))
          (condition-case err
              (lsp-format-region (or replacement-start (point-min)) (or replacement-end (point-max)))
            (error (message "LSP formatting failed: %s" err)))))

      ;; Restore point, adjusting for text length changes if needed
      (let ((point-adjustment (- (length new-string) (length old-string))))
        (if (and replacement-start (> original-point replacement-start))
            (goto-char (+ original-point (* point-adjustment replacement-count)))
          (goto-char original-point))))

    (format "Successfully replaced %d occurrence(s) of text in %s. Changed %d characters to %d characters."
            replacement-count
            (buffer-name original-buffer)
            (length old-string)
            (length new-string))))


;;; Shared edit logic
(defun gptelt-edit--edit-buffer-impl (buffer old-string new-string &optional replace-all)
  "editing BUFFER by replacing OLD-STRING with NEW-STRING.
If REPLACE-ALL is non-nil, replace all occurrences.
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
          (error "old_string not found in buffer: `%s`, CHECK CAREFULLY." buf-name))))
    ;; For Lisp code, check balance in temp buffer after replacement (refuse edit if unbalanced)
    (when (gptelt-edit--is-lisp-mode-p mj-mode)
      (unwind-protect
          (progn
            ;; Create temp buffer with the replacement applied
            (setq temp-buffer (generate-new-buffer " *gptelt-balance-check*"))
            (with-current-buffer temp-buffer
              (erase-buffer)
              ;; Copy original buffer content
              (insert-buffer-substring buffer)
              ;; Set the same major mode
              (funcall mj-mode)
              ;; Apply the replacement
              (goto-char (point-min))
              (if replace-all
                  (while (search-forward old-string nil t)
                    (let ((start (match-beginning 0))
                          (end (match-end 0)))
                      (goto-char start)
                      (delete-region start end)
                      (insert new-string)))
                (when (search-forward old-string nil t)
                  (let ((start (match-beginning 0))
                        (end (match-end 0)))
                    (goto-char start)
                    (delete-region start end)
                    (insert new-string))))
              ;; Check balance (smartparens then parinfer)
              (pcase-let
                  ((`(,balanced-p . ,error-msg)
                    (gptelt--check-buffer-balanced-parens temp-buffer)))
                (if balanced-p
                    (setq rst-buffer-string (buffer-string))
                  (setq
                   edit-allowed nil
                   unbalance-error
                   (or error-msg
                       (format! "The %s buffer would end up in an unbalanced state after replace. CHECK THE PARENTHESES CAREFULLY." (symbol-name mj-mode))))))))
        (when temp-buffer (kill-buffer temp-buffer))))
    (if (not edit-allowed)
        (error (substring-no-properties unbalance-error))
      ;; Non-Lisp or balanced, so apply edit directly
      (let ((rst-message
             (if rst-buffer-string
                 (gptelt--replace-buffer-directly buffer rst-buffer-string)
               (gptelt-edit--apply-edit-directly buffer old-string new-string replace-all))))
        ;; (+gptel-context-add-buffer buffer)
        rst-message))))

;;; Main edit functions
(defun gptelt-edit-edit-buffer (buffer-name old-string new-string &optional replace-all)
  "Edit buffer by replacing OLD-STRING with NEW-STRING in BUFFER-NAME.
If REPLACE-ALL is non-nil, replaces all occurrences.

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
    (gptelt-edit--edit-buffer-impl buffer old-string new-string replace-all)))

(comment
  (let ((f (make-temp-file "test-gptelt-edit")))
    (with-current-buffer (find-file-noselect f)
      (erase-buffer)
      (insert "old")
      (gptelt-edit-edit-buffer (current-buffer) "old" "new")
      (buffer-string))))

(defun gptelt-edit-edit-file (file-path old-string new-string &optional replace-all)
  "Edit file by replacing OLD-STRING with NEW-STRING in FILE-PATH.
If REPLACE-ALL is non-nil, replaces all occurrences.

FILE-PATH must be an absolute path.

This function:
1. Ensures file-path is absolute
2. Reads the file if no buffer exists for it
3. Generates context (project root, major mode, minor modes)
4. Validates balanced parentheses for Lisp code
5. Applies the edit directly to the buffer
6. Formats the changed region if LSP is available

Returns a string describing the result of the operation."
  (let* ((file-path (gptelt-edit--resolve-file-path file-path))
         (context (gptelt-edit--get-buffer-context file-path))
         (buffer (plist-get context :buffer))
         (resolved-path (plist-get context :file-path))
         (original-path (plist-get context :original-path)))
    (unless buffer
      (error "Could not open or create buffer for file: %s (resolved to: %s)"
             original-path resolved-path))
    (gptelt-edit--edit-buffer-impl buffer old-string new-string replace-all)))

(comment
  (let ((f (make-temp-file "test-gptelt-edit")))
    (with-current-buffer (find-file-noselect f)
      (erase-buffer)
      (insert "old")
      (save-buffer)
      (gptelt-edit-edit-file f "old" "new")
      (buffer-string))
    ;; fix balance
    (with-current-buffer (find-file-noselect f)
      (erase-buffer)
      (clojure-mode)
      (insert "(defn plus1 [n]\n (inc n))")
      (save-buffer)
      (gptelt-edit-edit-file
       f
       "(defn plus1 [n]\n (inc n))"
       "(defn plus1 [n]\n (inc n")
      (substring-no-properties (buffer-string)))
    (with-current-buffer (find-file-noselect f)
      (erase-buffer)
      (clojure-mode)
      (insert "(defn plus1 [n]\n (inc n))")
      (save-buffer)
      (gptelt-edit-edit-file
       f
       "(defn plus1 [n]\n (inc n))"
       "(defn plus1 [n]\n}}}}}}}]] (inc n))")
      (substring-no-properties (buffer-string)))))

;;; Tool registration
(when (fboundp 'gptelt-make-tool)
  (gptelt-make-tool
   :name "edit_buffer"
   :function #'gptelt-edit-edit-buffer
   :description (concat "Performs exact string replacements in buffers. "
                        "This tool edits already open Emacs buffers by replacing old_string with new_string. "
                        "\n\nIMPORTANT REQUIREMENTS:\n"
                        "1. UNIQUENESS: The old_string MUST uniquely identify the text to change\n"
                        "2. EXACT MATCH: Include all whitespace, indentation, and surrounding code exactly\n"
                        "3. CONTEXT: Include 3-5 lines before and after the change for uniqueness\n\n"
                        "Usage examples:\n"
                        "- Replace function: edit_buffer('main.py', 'def old_func():\\n    pass', 'def new_func():\\n    return True')\n"
                        "- Replace all occurrences: edit_buffer('config.js', 'localhost', '127.0.0.1', true)\n\n"
                        "For Lisp code, automatically validates parentheses balance and applies LSP formatting. "
                        "Use this instead of edit_file when the buffer is already open in Emacs.")
   :args '((:name "buffer_name" :type string
            :description "The name of the buffer to edit")
           (:name "old_string" :type string
            :description "The exact text to be replaced")
           (:name "new_string" :type string
            :description "The new text to replace the old text with")
           (:name "replace_all" :type boolean
            :description "If true, replace all occurrences. If false or omitted, replace only the first occurrence." :optional t))
   :category "emacs"
   :confirm nil
   :include t))

(when (fboundp 'gptelt-make-tool)
  (gptelt-make-tool
   :name "edit_file"
   :function #'gptelt-edit-edit-file
   :description (concat "Performs exact string replacements in files. "
                        "This tool edits files by replacing old_string with new_string. The file will be opened "
                        "if not already loaded, and automatically saved after the edit. "
                        "\n\nIMPORTANT REQUIREMENTS:\n"
                        "1. ABSOLUTE PATH: file_path must be an absolute path, not relative\n"
                        "2. UNIQUENESS: The old_string MUST uniquely identify the text to change\n"
                        "3. EXACT MATCH: Include all whitespace, indentation, and surrounding code exactly\n"
                        "4. CONTEXT: Include 3-5 lines before and after the change for uniqueness\n\n"
                        "Usage examples:\n"
                        "- Simple replacement: edit_file('/path/to/main.py', 'old_value = 1', 'new_value = 2')\n"
                        "- Function replacement: edit_file('/path/to/app.js', 'function oldName() {\\n  return false;\\n}', 'function newName() {\\n  return true;\\n}')\n"
                        "- Replace all: edit_file('/path/to/config.json', '\"debug\": false', '\"debug\": true', true)\n\n"
                        "For Lisp code, automatically validates parentheses balance and applies LSP formatting. "
                        "Creates the file if it doesn't exist (use empty old_string for new files).")
   :args '((:name "file_path" :type string
            :description "Absolute path to the file to edit (must be absolute, not relative)")
           (:name "old_string" :type string
            :description "The exact text to be replaced")
           (:name "new_string" :type string
            :description "The new text to replace the old text with")
           (:name "replace_all" :type boolean
            :description "If true, replace all occurrences. If false or omitted, replace only the first occurrence." :optional t))
   :category "emacs"
   :confirm nil
   :include t))

;;; Multi-edit logic
(defun gptelt-edit--multi-edit-buffer-impl (buffer edits)
  "Apply multiple edits to BUFFER. Each edit is a plist or cons cell.
Supports :replace_all for each edit (default nil)."
  (let ((applied-count 0))
    ;; oe one-edit
    (seq-doseq (oe edits)
      (let ((old (plist-get oe :old_string))
            (new (plist-get oe :new_string))
            (replace-all (plist-get oe :replace_all)))
        (gptelt-edit--edit-buffer-impl buffer old new replace-all)
        (setq applied-count (1+ applied-count))))
    (with-current-buffer buffer
      (save-buffer))
    (format "Successfully applied %d edits to buffer: %s" applied-count (buffer-name buffer))))


;;; Public multi-edit entrypoints

(defun gptelt-edit-multi-edit-buffer (buffer-name edits)
  "Apply multiple edits to BUFFER-NAME.
EDITS is a list where each element is either a cons cell (OLD . NEW), or an object with :old_string, :new_string, and optional :replace_all (boolean).
For Lisp code, checks balance after all edits.
Returns a string describing the result."
  (let ((buffer (get-buffer buffer-name)))
    (unless buffer
      (error "Buffer not found: %s" buffer-name))
    (gptelt-edit--multi-edit-buffer-impl buffer edits)))

(comment
  (let ((f (make-temp-file "test-gptelt-edit")))
    (with-current-buffer (find-file-noselect f)
      (erase-buffer)
      (insert "old1, old2")
      (gptelt-edit-multi-edit-buffer
       (current-buffer)
       '((:old_string "old1"
          :new_string "new1")
         (:old_string "old2"
          :new_string "new2")))
      (buffer-string))))

(defun gptelt-edit-multi-edit-file (file-path edits)
  "Apply multiple edits to FILE-PATH.
EDITS is a list where each element is either a cons cell (OLD . NEW), or an object with :old_string, :new_string, and optional :replace_all (boolean).
For Lisp code, checks balance after all edits.
Returns a string describing the result."
  (let* ((context (gptelt-edit--get-buffer-context file-path))
         (buffer (plist-get context :buffer))
         (resolved-path (plist-get context :file-path))
         (original-path (plist-get context :original-path)))
    (unless buffer
      (error "Could not open or create buffer for file: %s (resolved to: %s)"
             original-path resolved-path))
    (gptelt-edit--multi-edit-buffer-impl buffer edits)))

(comment
  (let ((f (make-temp-file "test-gptelt-edit")))
    (with-current-buffer (find-file-noselect f)
      (erase-buffer)
      (insert "old1, old2")
      (save-buffer)
      (gptelt-edit-multi-edit-file
       f
       '((:old_string "old1"
          :new_string "new1")
         (:old_string "old2"
          :new_string "new2")))
      (buffer-string))))

;;; Tool registration for multi-edit

(comment
  (gptelt--create-temp-file-buffer "")
  (gptelt-edit-multi-edit-buffer
   "gptelt-wZh7RJ"
   (list (list :old_string "a11" :new_string "a11"))))

(when (fboundp 'gptelt-make-tool)
  (gptelt-make-tool
   :name "multi_edit_buffer"
   :function #'gptelt-edit-multi-edit-buffer
   :description (concat "Apply multiple edits to a buffer by replacing a list of old texts with new texts, sequentially. "
                        "Each edit is applied in order to the result of the previous edit. "
                        "\n\nAdvantages over single edits:\n"
                        "- More efficient for multiple changes to the same buffer\n"
                        "- Maintains context between related changes\n"
                        "- Single save operation after all changes\n\n"
                        "Usage example:\n"
                        "multi_edit_buffer('app.py', [\n"
                        "  {\"old_string\": \"debug = False\", \"new_string\": \"debug = True\"},\n"
                        "  {\"old_string\": \"version = '1.0'\", \"new_string\": \"version = '1.1'\"}\n"
                        "])\n\n"
                        "Each edit object supports:\n"
                        "- old_string: Exact text to replace (required)\n"
                        "- new_string: Replacement text (required)\n"
                        "- replace_all: Replace all occurrences (optional, default false)\n\n"
                        "For Lisp code, validates parentheses balance after all edits are applied.")
   :args '((:name "buffer_name" :type string
            :description "The name of the buffer to edit")
           (:name "edits"
            :type array
            :items
            (:type object
             :properties
             (:old_string
              (:type string
               :description "The exact text to be replaced")
              :new_string
              (:type string
               :description "The new text to replace the old text with")
              :replace_all
              (:type boolean
               :description "If true, replace all occurrences. If false or omitted, replace only the first occurrence."
               :optional t))
             :required ["old_string" "new_string"]
             :description "An object with old_string, new_string, and optional replace_all fields for each edit.")
            :description "List of edits: each element is an object with old_string, new_string, and optional replace_all fields, applied sequentially."))
   :category "emacs"
   :confirm nil
   :include t))

(when (fboundp 'gptelt-make-tool)
  (gptelt-make-tool
   :name "multi_edit_file"
   :function #'gptelt-edit-multi-edit-file
   :description (concat "Apply multiple edits to a single file by replacing a list of old texts with new texts, sequentially. "
                        "Each edit operates on the result of the previous edit. The file is opened if not already loaded, "
                        "all edits are applied in order, and automatically saved. "
                        "\n\nBest practices:\n"
                        "- Use for related changes that should be applied together\n"
                        "- Ensure old_string patterns account for previous edits in the sequence\n"
                        "- More efficient than multiple separate edit_file calls\n\n"
                        "Usage example:\n"
                        "multi_edit_file('/path/to/config.js', [\n"
                        "  {\"old_string\": \"port: 3000\", \"new_string\": \"port: 8080\"},\n"
                        "  {\"old_string\": \"env: 'dev'\", \"new_string\": \"env: 'prod'\"}\n"
                        "])\n\n"
                        "Each edit object supports:\n"
                        "- old_string: Exact text to replace (required)\n"
                        "- new_string: Replacement text (required)\n"
                        "- replace_all: Replace all occurrences (optional, default false)\n\n"
                        "For Lisp code, validates parentheses balance after all edits. "
                        "Supports both absolute paths and paths relative to project root (~/ expansion supported).")
   :args '((:name "file_path" :type string
            :description "absolute or relative file path to the file to edit, `~/` is supported")
           (:name "edits"
            :type array
            :items
            (:type object
             :properties
             (:old_string (:type string
                           :description "The exact text to be replaced")
                          :new_string (:type string
                                       :description "The new text to replace the old text with")
                          :replace_all (:type boolean
                                        :description "If true, replace all occurrences. If false or omitted, replace only the first occurrence."
                                        :optional t))
             :required ["old_string" "new_string"]
             :description "An object with old_string, new_string, and optional replace_all fields for each edit.")
            :description "List of edits: each element is an object with old_string, new_string, and optional replace_all fields, applied sequentially."))
   :category "emacs"
   :confirm nil
   :include t))


;;; gptel-edit-tool.el ends here
