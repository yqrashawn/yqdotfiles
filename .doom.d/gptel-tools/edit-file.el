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

;;; Flexible matching utilities

(defun gptelt-edit--normalize-whitespace (text)
  "Normalize whitespace in TEXT for flexible matching.
Strips leading/trailing whitespace per line and normalizes internal whitespace."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (not (eobp))
      (beginning-of-line)
      (skip-chars-forward " \t")
      (let ((line-start (point)))
        (end-of-line)
        (skip-chars-backward " \t")
        (delete-region (point) (line-end-position))
        (goto-char line-start)
        (while (re-search-forward "[ \t]+" (line-end-position) t)
          (replace-match " "))
        (forward-line 1)))
    (buffer-string)))

(defun gptelt-edit--flexible-search (old-string)
  "Try to find OLD-STRING with flexible whitespace matching.
Returns (START . END) cons cell if found, nil otherwise."
  (let ((normalized-old (gptelt-edit--normalize-whitespace old-string)))
    (save-excursion
      (goto-char (point-min))
      (catch 'found
        (while (not (eobp))
          (let* ((region-start (point))
                 (region-end (min (+ region-start (* 2 (length old-string)))
                                  (point-max))))
            (when (> region-end region-start)
              (let ((region-text (buffer-substring-no-properties region-start region-end)))
                (when (string= normalized-old
                               (gptelt-edit--normalize-whitespace region-text))
                  (throw 'found (cons region-start region-end)))))
            (forward-line 1)))
        nil))))

(defun gptelt-edit--find-similar-text (old-string)
  "Find text similar to OLD-STRING for error reporting.
Returns a snippet of nearby text that might have been intended."
  (let* ((lines (split-string old-string "\n"))
         (first-line (car lines))
         (first-line-normalized (string-trim first-line)))
    (when (> (length first-line-normalized) 5)
      (save-excursion
        (goto-char (point-min))
        (when (search-forward first-line-normalized nil t)
          (let* ((line-start (line-beginning-position))
                 (context-start (max (point-min) (- line-start 200)))
                 (context-end (min (point-max) (+ line-start 200))))
            (buffer-substring-no-properties context-start context-end)))))))

;;; LLM self-correction
(defun gptelt-edit--llm-fix-old-string (buffer old-string instruction callback)
  "Use LLM to find the correct old_string in BUFFER asynchronously.
OLD-STRING is the failed match attempt.
INSTRUCTION describes what the user wants to change.
CALLBACK is called with the corrected old_string or nil if LLM can't find it."
  (let* ((buffer-content (with-current-buffer buffer (buffer-string)))
         (prompt (format "I'm trying to edit a file, but the exact string I'm looking for doesn't match.

File content:
```
%s
```

String I tried to match (but failed):
```
%s
```

What I'm trying to do: %s

Please find the EXACT string from the file that matches what I'm trying to change. Return ONLY the exact string from the file, with no explanation. If you can't find a match, return ERROR."
                         buffer-content
                         old-string
                         (or instruction "replace this text"))))
    (simple-llm-req
     prompt
     :backend gptel--gh-copilot-individual
     :model 'gpt-4.1
     :temperature 0.3
     :system "You are a precise text matching assistant. Your job is to find exact string matches in files. Always return the EXACT text from the file, preserving all whitespace, indentation, and formatting. If you can't find a match, return ERROR."
     :cb (lambda (response)
           (funcall callback
                    (if (or (null response) (string-match-p "ERROR" response))
                        nil
                      (string-trim response))))
     :error (lambda (_error)
              (funcall callback nil)))))

;;; Direct edit application

(defun gptelt-edit--apply-edit-directly (buffer old-string new-string callback &optional replace-all instruction skip-save)
  "Apply edit to BUFFER by directly replacing OLD-STRING with NEW-STRING.
If REPLACE-ALL is non-nil, replace all occurrences. Otherwise, only the first.
INSTRUCTION is optional natural language description of the change.
CALLBACK is called with the result message when done (async if LLM correction needed).
SKIP-SAVE if non-nil, skips saving the buffer (for multi-edit operations).

Tries multiple matching strategies:
1. Exact match
2. Flexible whitespace match
3. LLM self-correction (if instruction provided)"
  (let* ((original-buffer buffer)
         (original-point (with-current-buffer original-buffer (point)))
         (match-found nil)
         (corrected-old-string old-string))

    (with-current-buffer original-buffer
      (save-excursion
        (goto-char (point-min))
        
        ;; Strategy 1: Try exact match
        (setq match-found (search-forward old-string nil t))
        
        ;; Strategy 2: Try flexible whitespace match
        (unless match-found
          (when-let ((pos (gptelt-edit--flexible-search old-string)))
            (setq corrected-old-string 
                  (buffer-substring-no-properties (car pos) (cdr pos)))
            (goto-char (car pos))
            (setq match-found t)))))
    
    ;; Strategy 3: Try LLM self-correction (async)
    (if (and (not match-found) instruction)
        (gptelt-edit--llm-fix-old-string
         buffer old-string instruction
         (lambda (fixed-string)
           (if fixed-string
               (with-current-buffer original-buffer
                 (save-excursion
                   (goto-char (point-min))
                   (if (search-forward fixed-string nil t)
                       (gptelt-edit--do-replacement
                        original-buffer fixed-string new-string original-point
                        replace-all callback skip-save)
                     (funcall callback
                              (gptelt-edit--generate-error old-string)))))
             (funcall callback
                      (gptelt-edit--generate-error old-string)))))
      ;; Sync path: either match found or no instruction for LLM
      (if match-found
          (gptelt-edit--do-replacement
           original-buffer corrected-old-string new-string original-point
           replace-all callback skip-save)
        (funcall callback
                 (gptelt-edit--generate-error old-string))))))

(defun gptelt-edit--do-replacement (buffer old-string new-string original-point replace-all callback &optional skip-save)
  "Helper function to perform the actual replacement in BUFFER.
OLD-STRING is the text to replace, NEW-STRING is the replacement.
ORIGINAL-POINT is the cursor position before edit.
REPLACE-ALL determines if all occurrences should be replaced.
CALLBACK is called with the result message.
SKIP-SAVE if non-nil, skips saving the buffer (for multi-edit operations)."
  (let ((replacement-count 0)
        (replacement-start nil)
        (replacement-end nil))
    (with-current-buffer buffer
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
          (when (search-forward old-string nil t)
            (let ((start (match-beginning 0))
                  (end (match-end 0)))
              (setq replacement-start start)
              (goto-char start)
              (delete-region start end)
              (insert new-string)
              (setq replacement-end (+ start (length new-string)))
              (setq replacement-count 1))))
        
        (unless skip-save
          (+force-save-buffer))
        (when (and (fboundp 'lsp-format-region)
                   (bound-and-true-p lsp-mode))
          (condition-case err
              (lsp-format-region (or replacement-start (point-min)) 
                                 (or replacement-end (point-max)))
            (error (message "LSP formatting failed: %s" err)))))

      (let ((point-adjustment (- (length new-string) (length old-string))))
        (if (and replacement-start (> original-point replacement-start))
            (goto-char (+ original-point (* point-adjustment replacement-count)))
          (goto-char original-point))))

    (funcall callback
             (format "Successfully replaced %d occurrence(s) of text in %s."
                     replacement-count
                     (buffer-name buffer)))))

(defun gptelt-edit--generate-error (old-string)
  "Generate error message for failed match of OLD-STRING."
  (let ((similar-text (gptelt-edit--find-similar-text old-string)))
    (format "old_string not found. %s\n\nSearched for:\n%s%s"
            "CHECK CAREFULLY - try including more context lines."
            old-string
            (if similar-text
                (format "\n\nFound similar text nearby:\n%s" similar-text)
              ""))))


;;; Shared edit logic
(defun gptelt-edit--edit-buffer-impl-skip-save (buffer old-string new-string callback &optional replace-all instruction)
  "Wrapper for gptelt-edit--edit-buffer-impl that skips saving.
Used in multi-edit operations where save is deferred to the end."
  (gptelt-edit--edit-buffer-impl buffer old-string new-string callback replace-all instruction t))

(defun gptelt-edit--edit-buffer-impl (buffer old-string new-string callback &optional replace-all instruction skip-save)
  "editing BUFFER by replacing OLD-STRING with NEW-STRING.
If REPLACE-ALL is non-nil, replace all occurrences.
INSTRUCTION is optional natural language description of the change.
SKIP-SAVE if non-nil, skips saving the buffer (for multi-edit operations).
CALLBACK is called with the result string when done (async if LLM correction needed)."
  (let ((mj-mode (buffer-local-value 'major-mode buffer))
        (buf-name (buffer-name buffer))
        temp-buffer
        rst-buffer-string
        (edit-allowed t)
        unbalance-error
        (corrected-old-string old-string)
        (match-found nil))
    
    ;; Try to find old-string with multiple strategies (sync only)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (setq match-found (search-forward old-string nil t))
        
        (unless match-found
          ;; Try flexible whitespace match
          (when-let ((pos (gptelt-edit--flexible-search old-string)))
            (setq corrected-old-string 
                  (buffer-substring-no-properties (car pos) (cdr pos)))
            (setq match-found t)))))
    
    ;; Define the continuation function that does the actual edit
    (let ((do-edit
           (lambda (final-old-string &optional skip-save)
             ;; For Lisp code, check balance in temp buffer (async)
             (if (gptelt-edit--is-lisp-mode-p mj-mode)
                 (let ((temp-buffer (generate-new-buffer " *gptelt-balance-check*")))
                   (with-current-buffer temp-buffer
                     (erase-buffer)
                     (insert-buffer-substring buffer)
                     (funcall mj-mode)
                     (goto-char (point-min))
                     (if replace-all
                         (while (search-forward final-old-string nil t)
                           (let ((start (match-beginning 0))
                                 (end (match-end 0)))
                             (goto-char start)
                             (delete-region start end)
                             (insert new-string)))
                       (when (search-forward final-old-string nil t)
                         (let ((start (match-beginning 0))
                               (end (match-end 0)))
                           (goto-char start)
                           (delete-region start end)
                           (insert new-string))))
                     (gptelt--check-buffer-balanced-parens
                      temp-buffer
                      (lambda (result)
                        (let ((balanced-p (car result))
                              (error-msg (cdr result)))
                          (if balanced-p
                              (let ((balanced-string (with-current-buffer temp-buffer
                                                       (buffer-string))))
                                (kill-buffer temp-buffer)
                                (funcall callback
                                         (gptelt--replace-buffer-directly buffer balanced-string skip-save)))
                            (kill-buffer temp-buffer)
                            (funcall callback
                                     (substring-no-properties
                                      (or error-msg
                                          (format "The %s buffer would end up in an unbalanced state after replace. CHECK THE PARENTHESES CAREFULLY."
                                                  (symbol-name mj-mode)))))))))))
               ;; Non-Lisp mode, proceed directly
               (gptelt-edit--apply-edit-directly
                buffer final-old-string new-string callback replace-all instruction skip-save)))))
      
      ;; If match found or no instruction, proceed synchronously
      (if (or match-found (not instruction))
          (if match-found
              (funcall do-edit corrected-old-string)
            ;; No match and no instruction - error
            (funcall callback (gptelt-edit--generate-error old-string)))
        ;; No match but have instruction - try LLM correction (async)
        (gptelt-edit--llm-fix-old-string
         buffer old-string instruction
         (lambda (fixed-string)
           (if fixed-string
               (with-current-buffer buffer
                 (save-excursion
                   (goto-char (point-min))
                   (if (search-forward fixed-string nil t)
                       (funcall do-edit fixed-string)
                     (funcall callback (gptelt-edit--generate-error old-string)))))
             (funcall callback (gptelt-edit--generate-error old-string)))))))))

;;; Main edit functions
(defun gptelt-edit-edit-buffer (callback buffer-name old-string new-string &optional replace-all instruction)
  "Edit buffer by replacing OLD-STRING with NEW-STRING in BUFFER-NAME.
If REPLACE-ALL is non-nil, replaces all occurrences.
INSTRUCTION is optional natural language description of the change.
CALLBACK is called with the result string when done (async if LLM correction needed).

BUFFER-NAME should be the name of an existing buffer.

This function:
1. Gets the buffer by name
2. Tries multiple matching strategies (exact, flexible whitespace, LLM correction)
3. Validates balanced parentheses for Lisp code
4. Applies the edit directly to the buffer
5. Formats the changed region if LSP is available"
  (let ((buffer (get-buffer buffer-name)))
    (unless buffer
      (error "Buffer not found: %s" buffer-name))
    (gptelt-edit--edit-buffer-impl buffer old-string new-string callback replace-all instruction)))

(comment
  (let ((f (make-temp-file "test-gptelt-edit")))
    (with-current-buffer (find-file-noselect f)
      (erase-buffer)
      (insert "old")
      (gptelt-edit-edit-buffer (current-buffer) "old" "new")
      (buffer-string))))

(defun gptelt-edit-edit-file (callback file-path old-string new-string &optional replace-all instruction)
  "Edit file by replacing OLD-STRING with NEW-STRING in FILE-PATH.
If REPLACE-ALL is non-nil, replaces all occurrences.
INSTRUCTION is optional natural language description of the change.
CALLBACK is called with the result string when done (async if LLM correction needed).

FILE-PATH must be an absolute path.

This function:
1. Ensures file-path is absolute
2. Reads the file if no buffer exists for it
3. Generates context (project root, major mode, minor modes)
4. Tries multiple matching strategies (exact, flexible whitespace, LLM correction)
5. Validates balanced parentheses for Lisp code
6. Applies the edit directly to the buffer
7. Formats the changed region if LSP is available"
  (let* ((file-path (gptelt-edit--resolve-file-path file-path))
         (context (gptelt-edit--get-buffer-context file-path))
         (buffer (plist-get context :buffer))
         (resolved-path (plist-get context :file-path))
         (original-path (plist-get context :original-path)))
    (unless buffer
      (error "Could not open or create buffer for file: %s (resolved to: %s)"
             original-path resolved-path))
    (gptelt-edit--edit-buffer-impl buffer old-string new-string callback replace-all instruction)))

(comment
  (let ((f (make-temp-file "test-gptelt-edit")))
    (with-current-buffer (find-file-noselect f)
      (erase-buffer)
      (insert "old")
      (+force-save-buffer)
      (gptelt-edit-edit-file f "old" "new")
      (buffer-string))
    ;; fix balance
    (with-current-buffer (find-file-noselect f)
      (erase-buffer)
      (clojure-mode)
      (insert "(defn plus1 [n]\n (inc n))")
      (+force-save-buffer)
      (gptelt-edit-edit-file
       f
       "(defn plus1 [n]\n (inc n))"
       "(defn plus1 [n]\n (inc n")
      (substring-no-properties (buffer-string)))
    (with-current-buffer (find-file-noselect f)
      (erase-buffer)
      (clojure-mode)
      (insert "(defn plus1 [n]\n (inc n))")
      (+force-save-buffer)
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
   :async t
   :description (concat "Performs string replacements in buffers with intelligent matching. "
                        "This tool edits already open Emacs buffers by replacing old_string with new_string. "
                        "\n\nMATCHING STRATEGIES (tried in order):\n"
                        "1. Exact string match\n"
                        "2. Flexible whitespace match (normalizes spaces/tabs/indentation)\n"
                        "3. LLM self-correction (if 'instruction' parameter provided)\n\n"
                        "BEST PRACTICES:\n"
                        "1. Include 3-5 lines of context for uniqueness\n"
                        "2. Use 'instruction' parameter to describe your intent (helps with self-correction)\n"
                        "3. Don't worry about exact whitespace - flexible matching handles minor differences\n\n"
                        "Usage examples:\n"
                        "- Replace function: edit_buffer('main.py', 'def old_func():\\n    pass', 'def new_func():\\n    return True', false, 'rename function')\n"
                        "- Replace all occurrences: edit_buffer('config.js', 'localhost', '127.0.0.1', true)\n\n"
                        "For Lisp code, automatically validates parentheses balance and applies LSP formatting. "
                        "Use this instead of edit_file when the buffer is already open in Emacs.")
   :args '((:name "buffer_name" :type string
            :description "The name of the buffer to edit")
           (:name "old_string" :type string
            :description "The text to be replaced (will try flexible matching if exact match fails)")
           (:name "new_string" :type string
            :description "The new text to replace the old text with")
           (:name "replace_all" :type boolean
            :description "If true, replace all occurrences. If false or omitted, replace only the first occurrence." :optional t)
           (:name "instruction" :type string
            :description "Optional: Natural language description of what you're changing (e.g., 'update the port number', 'fix the function name'). Helps with LLM self-correction if exact match fails." :optional t))
   :category "edit"
   :confirm nil
   :include t))

(when (fboundp 'gptelt-make-tool)
  (gptelt-make-tool
   :name "edit_file"
   :function #'gptelt-edit-edit-file
   :async t
   :description (concat "Performs string replacements in files with intelligent matching. "
                        "This tool edits files by replacing old_string with new_string. The file will be opened "
                        "if not already loaded, and automatically saved after the edit. "
                        "\n\nMATCHING STRATEGIES (tried in order):\n"
                        "1. Exact string match\n"
                        "2. Flexible whitespace match (normalizes spaces/tabs/indentation)\n"
                        "3. LLM self-correction (if 'instruction' parameter provided)\n\n"
                        "BEST PRACTICES:\n"
                        "1. ABSOLUTE PATH: file_path must be an absolute path\n"
                        "2. Include 3-5 lines of context for uniqueness\n"
                        "3. Use 'instruction' parameter to describe your intent (helps with self-correction)\n"
                        "4. Don't worry about exact whitespace - flexible matching handles minor differences\n\n"
                        "Usage examples:\n"
                        "- Simple replacement: edit_file('/path/to/main.py', 'old_value = 1', 'new_value = 2', false, 'update constant')\n"
                        "- Function replacement: edit_file('/path/to/app.js', 'function oldName() {\\n  return false;\\n}', 'function newName() {\\n  return true;\\n}', false, 'rename function')\n"
                        "- Replace all: edit_file('/path/to/config.json', '\"debug\": false', '\"debug\": true', true)\n\n"
                        "For Lisp code, automatically validates parentheses balance and applies LSP formatting. "
                        "Creates the file if it doesn't exist (use empty old_string for new files).")
   :args '((:name "file_path" :type string
            :description "Absolute path to the file to edit (must be absolute, not relative)")
           (:name "old_string" :type string
            :description "The text to be replaced (will try flexible matching if exact match fails)")
           (:name "new_string" :type string
            :description "The new text to replace the old text with")
           (:name "replace_all" :type boolean
            :description "If true, replace all occurrences. If false or omitted, replace only the first occurrence." :optional t)
           (:name "instruction" :type string
            :description "Optional: Natural language description of what you're changing (e.g., 'update the port number', 'fix the function name'). Helps with LLM self-correction if exact match fails." :optional t))
   :category "edit"
   :confirm nil
   :include t))

;;; Multi-edit logic
(defun gptelt-edit--multi-edit-buffer-impl (buffer edits callback)
  "Apply multiple edits to BUFFER sequentially. Each edit is a plist or cons cell.
Supports :replace_all and :instruction for each edit (default nil).
CALLBACK is called with the final result message when all edits are done."
  ;; Convert vector to list if needed
  (when (vectorp edits)
    (setq edits (append edits nil)))
  (let ((applied-count 0)
        (total-edits (length edits))
        (save-needed nil))
    (cl-labels
        ((process-next-edit
           (remaining-edits)
           (if (null remaining-edits)
               ;; All edits done - save once at the end
               (progn
                 (when save-needed
                   (with-current-buffer buffer
                     (+force-save-buffer)))
                 (funcall callback
                          (format "Successfully applied %d edits to buffer: %s"
                                  applied-count (buffer-name buffer))))
             ;; Process next edit
             (let* ((oe (car remaining-edits))
                    (old (plist-get oe :old_string))
                    (new (plist-get oe :new_string))
                    (replace-all (plist-get oe :replace_all))
                    (instruction (plist-get oe :instruction)))
               (gptelt-edit--edit-buffer-impl
                buffer old new
                (lambda (result)
                  ;; Edit completed, mark that we need to save
                  (setq save-needed t)
                  (setq applied-count (1+ applied-count))
                  (process-next-edit (cdr remaining-edits)))
                replace-all instruction)))))
      (process-next-edit edits))))


;;; Public multi-edit entrypoints

(defun gptelt-edit-multi-edit-buffer (callback buffer-name edits)
  "Apply multiple edits to BUFFER-NAME.
EDITS is a list where each element is either a cons cell (OLD . NEW), or an object with :old_string, :new_string, and optional :replace_all (boolean).
For Lisp code, checks balance after each edit.
CALLBACK is called with the result string when all edits are done."
  (let ((buffer (get-buffer buffer-name)))
    (unless buffer
      (error "Buffer not found: %s" buffer-name))
    (gptelt-edit--multi-edit-buffer-impl buffer edits callback)))

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

(defun gptelt-edit-multi-edit-file (callback file-path edits)
  "Apply multiple edits to FILE-PATH.
EDITS is a list where each element is either a cons cell (OLD . NEW), or an object with :old_string, :new_string, and optional :replace_all (boolean).
For Lisp code, checks balance after each edit.
CALLBACK is called with the result string when all edits are done."
  (let* ((context (gptelt-edit--get-buffer-context file-path))
         (buffer (plist-get context :buffer))
         (resolved-path (plist-get context :file-path))
         (original-path (plist-get context :original-path)))
    (unless buffer
      (error "Could not open or create buffer for file: %s (resolved to: %s)"
             original-path resolved-path))
    (gptelt-edit--multi-edit-buffer-impl buffer edits callback)))

(comment
  (let ((f (make-temp-file "test-gptelt-edit")))
    (with-current-buffer (find-file-noselect f)
      (erase-buffer)
      (insert "old1, old2")
      (+force-save-buffer)
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
   :async t
   :description (concat "Apply multiple edits to a buffer with intelligent matching. "
                        "Each edit is applied in order to the result of the previous edit. "
                        "\n\nAdvantages over single edits:\n"
                        "- More efficient for multiple changes to the same buffer\n"
                        "- Maintains context between related changes\n"
                        "- Single save operation after all changes\n"
                        "- Each edit uses flexible matching and LLM self-correction\n\n"
                        "Usage example:\n"
                        "multi_edit_buffer('app.py', [\n"
                        "  {\"old_string\": \"debug = False\", \"new_string\": \"debug = True\", \"instruction\": \"enable debug mode\"},\n"
                        "  {\"old_string\": \"version = '1.0'\", \"new_string\": \"version = '1.1'\", \"instruction\": \"bump version\"}\n"
                        "])\n\n"
                        "Each edit object supports:\n"
                        "- old_string: Text to replace (required, flexible matching)\n"
                        "- new_string: Replacement text (required)\n"
                        "- replace_all: Replace all occurrences (optional, default false)\n"
                        "- instruction: Natural language description (optional, helps with self-correction)\n\n"
                        "For Lisp code, validates parentheses balance after each edit.")
   :args '((:name "buffer_name" :type string
            :description "The name of the buffer to edit")
           (:name "edits"
            :type array
            :items
            (:type object
             :properties
             (:old_string
              (:type string
               :description "The text to be replaced (will try flexible matching if exact match fails)")
              :new_string
              (:type string
               :description "The new text to replace the old text with")
              :replace_all
              (:type boolean
               :description "If true, replace all occurrences. If false or omitted, replace only the first occurrence."
               :optional t)
              :instruction
              (:type string
               :description "Optional: Natural language description of what you're changing"
               :optional t))
             :required ["old_string" "new_string"]
             :description "An object with old_string, new_string, and optional replace_all/instruction fields for each edit.")
            :description "List of edits: each element is an object with old_string, new_string, and optional replace_all fields, applied sequentially."))
   :category "edit"
   :confirm nil
   :include t))

(when (fboundp 'gptelt-make-tool)
  (gptelt-make-tool
   :name "multi_edit_file"
   :function #'gptelt-edit-multi-edit-file
   :async t
   :description (concat "Apply multiple edits to a single file with intelligent matching. "
                        "Each edit operates on the result of the previous edit. The file is opened if not already loaded, "
                        "all edits are applied in order, and automatically saved. "
                        "\n\nBest practices:\n"
                        "- Use for related changes that should be applied together\n"
                        "- Ensure old_string patterns account for previous edits in the sequence\n"
                        "- More efficient than multiple separate edit_file calls\n"
                        "- Each edit uses flexible matching and LLM self-correction\n\n"
                        "Usage example:\n"
                        "multi_edit_file('/path/to/config.js', [\n"
                        "  {\"old_string\": \"port: 3000\", \"new_string\": \"port: 8080\", \"instruction\": \"update port\"},\n"
                        "  {\"old_string\": \"env: 'dev'\", \"new_string\": \"env: 'prod'\", \"instruction\": \"switch to production\"}\n"
                        "])\n\n"
                        "Each edit object supports:\n"
                        "- old_string: Text to replace (required, flexible matching)\n"
                        "- new_string: Replacement text (required)\n"
                        "- replace_all: Replace all occurrences (optional, default false)\n"
                        "- instruction: Natural language description (optional, helps with self-correction)\n\n"
                        "For Lisp code, validates parentheses balance after each edit. "
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
   :category "edit"
   :confirm nil
   :include t))


;;; gptel-edit-tool.el ends here
