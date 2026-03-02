;;; edit-file.el --- GPTEL file/buffer edit tools -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(require 'gptel)
(require 'cl-lib)

;;; Utility functions

(defun gptelt-edit--get-buffer-for-file (file-path)
  "Get or open a buffer for FILE-PATH.
FILE-PATH is expanded via `expand-file-name'. Returns the buffer or nil."
  (let ((resolved (expand-file-name file-path)))
    (or (get-file-buffer resolved)
        (when (file-exists-p resolved)
          (find-file-noselect resolved)))))

;;; Matching strategies module

(defun gptelt-edit--normalize-line (line)
  "Normalize a single LINE for flexible matching.
Strips leading/trailing whitespace and collapses internal whitespace runs."
  (replace-regexp-in-string "[ \t]+" " " (string-trim line)))

;; Strategy 1: Exact match
(defun gptelt-edit--try-exact-match (old-string)
  "Try to find exact match for OLD-STRING in current buffer.
Returns (START . END) cons cell if found, nil otherwise."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward old-string nil t)
      (cons (match-beginning 0) (match-end 0)))))

;; Strategy 2: Flexible whitespace match
(defun gptelt-edit--try-flexible-match (old-string)
  "Try to find OLD-STRING with flexible whitespace matching in current buffer.
Compares normalized lines using a sliding window matching the line count
of OLD-STRING.  Returns (START . END) cons cell if found, nil otherwise."
  (let* ((old-lines (split-string old-string "\n"))
         (norm-old-lines (mapcar #'gptelt-edit--normalize-line old-lines))
         (n (length norm-old-lines)))
    (save-excursion
      (goto-char (point-min))
      (catch 'found
        (while (not (eobp))
          (let ((start (line-beginning-position))
                (buf-lines nil)
                (pos (point)))
            ;; Collect N lines from current position
            (dotimes (_ n)
              (unless (eobp)
                (push (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))
                      buf-lines)
                (forward-line 1)))
            (setq buf-lines (nreverse buf-lines))
            ;; Compare if we got enough lines
            (when (= (length buf-lines) n)
              (let ((norm-buf-lines (mapcar #'gptelt-edit--normalize-line buf-lines)))
                (when (equal norm-old-lines norm-buf-lines)
                  ;; Match found - return region spanning the matched lines
                  (let ((end (save-excursion
                               (goto-char start)
                               (forward-line (1- n))
                               (line-end-position))))
                    (throw 'found (cons start end))))))
            ;; Advance by one line from original position
            (goto-char pos)
            (forward-line 1)))
        nil))))

;; Strategy pipeline
(defun gptelt-edit--find-match (buffer old-string)
  "Find OLD-STRING in BUFFER using multiple strategies.
Returns plist with :match-pos (cons cell) and :corrected-string.
Returns nil if no match found."
  (with-current-buffer buffer
    (or
     ;; Strategy 1: Exact match
     (when-let ((pos (gptelt-edit--try-exact-match old-string)))
       (list :match-pos pos :corrected-string old-string))
     ;; Strategy 2: Flexible whitespace match
     (when-let ((pos (gptelt-edit--try-flexible-match old-string)))
       (list :match-pos pos
             :corrected-string
             (buffer-substring-no-properties
              (car pos) (cdr pos)))))))

;; Helper for error messages
(defun gptelt-edit--find-similar-text (buffer old-string)
  "Find text in BUFFER similar to OLD-STRING for error reporting.
Returns a snippet of nearby text that might have been intended."
  (let* ((lines (split-string old-string "\n"))
         (first-line (car lines))
         (first-line-normalized (string-trim first-line)))
    (when (> (length first-line-normalized) 5)
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-min))
          (when (search-forward first-line-normalized nil t)
            (let* ((line-start (line-beginning-position))
                   (context-start (max (point-min) (- line-start 200)))
                   (context-end (min (point-max) (+ line-start 200))))
              (buffer-substring-no-properties context-start context-end))))))))

;;; Error handling module

(defun gptelt-edit--generate-error (buffer old-string)
  "Generate error message for failed match of OLD-STRING in BUFFER."
  (let ((similar-text (gptelt-edit--find-similar-text buffer old-string)))
    (format "old_string not found. %s\n\nSearched for:\n%s%s"
            "CHECK CAREFULLY - try including more context lines."
            old-string
            (if similar-text
                (format "\n\nFound similar text nearby:\n%s" similar-text)
              ""))))

;;; LLM self-correction (Strategy 3)
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
     :backend +gptel-free-backend
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

;;; Edit application module
(defun gptelt-edit--apply-match-with-llm (buffer old-string instruction callback new-string replace-all)
  "Try LLM correction for failed match and apply edit.
CALLBACK is called with the result message."
  (gptelt-edit--llm-fix-old-string
   buffer old-string instruction
   (lambda (fixed-string)
     (if fixed-string
         (with-current-buffer buffer
           (save-excursion
             (goto-char (point-min))
             (if (search-forward fixed-string nil t)
                 (gptelt-edit--do-replacement
                  buffer fixed-string new-string
                  replace-all callback)
               (funcall callback
                        (gptelt-edit--generate-error buffer old-string)))))
       (funcall callback
                (gptelt-edit--generate-error buffer old-string))))))

(defun gptelt-edit--apply-edit-directly
    (buffer old-string new-string callback &optional replace-all instruction)
  "Apply edit to BUFFER by replacing OLD-STRING with NEW-STRING.
If REPLACE-ALL is non-nil, replace all occurrences. Otherwise, only the first.
INSTRUCTION is optional natural language description of the change.
CALLBACK is called with the result message when done (async if LLM correction needed).

Tries multiple matching strategies:
1. Exact match
2. Flexible whitespace match
3. LLM self-correction (if instruction provided)

IMPORTANT: Reverts buffer first to ensure we're working with disk state."
  ;; Revert buffer first to ensure we're editing the actual disk state
  (+gptel-tool-revert-to-be-visited-buffer buffer)

  ;; Try to find match using strategy pipeline
  (if-let ((match-result (gptelt-edit--find-match buffer old-string)))
      ;; Match found - apply replacement
      (gptelt-edit--do-replacement
       buffer
       (plist-get match-result :corrected-string)
       new-string
       replace-all
       callback)
    ;; No match - try LLM correction if instruction provided
    (if instruction
        (gptelt-edit--apply-match-with-llm
         buffer old-string instruction callback
         new-string replace-all)
      ;; No instruction and no match - error
      (funcall callback (gptelt-edit--generate-error buffer old-string)))))

(defun gptelt-edit--do-replacement
    (buffer old-string new-string replace-all callback)
  "Helper function to perform the actual replacement in BUFFER.
OLD-STRING is the text to replace, NEW-STRING is the replacement.
REPLACE-ALL determines if all occurrences should be replaced.
CALLBACK is called with the result message."
  (let ((replacement-count 0)
        (replacement-start nil)
        (replacement-end nil))
    (with-current-buffer buffer
      (let ((original-point (point)))
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

          (+force-save-buffer)

          (when (and (fboundp 'lsp-format-region)
                     (bound-and-true-p lsp-mode))
            (condition-case err
                (progn (lsp-format-region (or replacement-start (point-min))
                                          (or replacement-end (point-max)))
                       (+force-save-buffer))
              (error (message "LSP formatting failed: %s" err)))))

        ;; Restore point, clamped to buffer bounds
        (goto-char (min original-point (point-max)))))

    (funcall callback
             (format "Successfully replaced %d occurrence(s) of text in %s."
                     replacement-count
                     (buffer-name buffer)))))

;;; Balance checking and edit logic
(defun gptelt-edit--edit-buffer-impl
    (buffer old-string new-string callback &optional replace-all instruction)
  "Edit BUFFER by replacing OLD-STRING with NEW-STRING.
If REPLACE-ALL is non-nil, replace all occurrences.
INSTRUCTION is optional natural language description of the change.
CALLBACK is called with the result string when done (async if LLM correction needed)."
  (let* ((mj-mode (buffer-local-value 'major-mode buffer))
         (match-result (gptelt-edit--find-match buffer old-string)))

    ;; Define the continuation function that does the actual edit
    (let ((do-edit
           (lambda (final-old-string)
             ;; For Lisp code, check balance in temp buffer (async)
             (if (gptelt--is-lisp-mode-p mj-mode)
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
                                         (gptelt--replace-buffer-directly
                                          buffer balanced-string)))
                            (kill-buffer temp-buffer)
                            (funcall callback
                                     (substring-no-properties
                                      (or error-msg
                                          (format "The %s buffer would end up in an unbalanced state after replace. CHECK THE PARENTHESES CAREFULLY."
                                                  (symbol-name mj-mode)))))))))))
               ;; Non-Lisp mode, proceed directly
               (gptelt-edit--apply-edit-directly
                buffer final-old-string new-string callback replace-all instruction)))))

      ;; If match found or no instruction, proceed synchronously
      (if (or match-result (not instruction))
          (if match-result
              (funcall do-edit (plist-get match-result :corrected-string))
            ;; No match and no instruction - error
            (funcall callback (gptelt-edit--generate-error buffer old-string)))
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
                     (funcall callback (gptelt-edit--generate-error buffer old-string)))))
             (funcall callback (gptelt-edit--generate-error buffer old-string)))))))))

;;; Main edit functions
(defun gptelt-edit-edit-buffer
    (callback buffer-name old-string new-string &optional replace-all instruction)
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
    (+gptel-tool-revert-to-be-visited-buffer buffer)
    (gptelt-edit--edit-buffer-impl buffer old-string new-string callback replace-all instruction)))

(comment
  (let ((f (make-temp-file "test-gptelt-edit")))
    (with-current-buffer (find-file-noselect f)
      (erase-buffer)
      (insert "old")
      (gptelt-edit-edit-buffer
       'ignore
       (current-buffer) "old" "new")
      (buffer-string))))

(defun gptelt-edit-edit-file
    (callback file-path old-string new-string &optional replace-all instruction)
  "Edit file by replacing OLD-STRING with NEW-STRING in FILE-PATH.
If REPLACE-ALL is non-nil, replaces all occurrences.
INSTRUCTION is optional natural language description of the change.
CALLBACK is called with the result string when done (async if LLM correction needed).

FILE-PATH must be an absolute path.

This function:
1. Ensures file-path is absolute
2. Reads the file if no buffer exists for it
3. Tries multiple matching strategies (exact, flexible whitespace, LLM correction)
4. Validates balanced parentheses for Lisp code
5. Applies the edit directly to the buffer
6. Formats the changed region if LSP is available"
  (let* ((resolved-path (expand-file-name file-path))
         (buffer (gptelt-edit--get-buffer-for-file resolved-path)))
    (unless buffer
      (error "Could not open or create buffer for file: %s (resolved to: %s)"
             file-path resolved-path))
    (gptelt-edit--edit-buffer-impl buffer old-string new-string callback replace-all instruction)))

(comment
  (let ((f (make-temp-file "test-gptelt-edit")))
    (with-current-buffer (find-file-noselect f)
      (erase-buffer)
      (insert "old")
      (+force-save-buffer)
      (gptelt-edit-edit-file 'message f "old" "new")
      (buffer-string))
    ;; fix balance
    (with-current-buffer (find-file-noselect f)
      (erase-buffer)
      (clojure-mode)
      (insert "(defn plus1 [n]\n (inc n))")
      (+force-save-buffer)
      (gptelt-edit-edit-file
       'message
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
       'message
       f
       "(defn plus1 [n]\n (inc n))"
       "(defn plus1 [n]\n}}}}}}}]] (inc n))")
      (substring-no-properties (buffer-string)))))

;;; MCP Parameter Translation Wrappers

(defvar gptelt-edit-debug nil
  "When non-nil, log edit tool parameter translation for debugging.")

(defun gptelt-edit--error-wrapping-callback (callback)
  "Wrap CALLBACK to tag non-success results with <tool_use_error>.
Returns a new callback that passes through success messages unchanged
and wraps error messages in <tool_use_error> tags."
  (lambda (result)
    (funcall callback
             (if (or (not (stringp result))
                     (string-prefix-p "Successfully" result)
                     (string-prefix-p "<tool_use_error>" result))
                 result
               (format "<tool_use_error>%s</tool_use_error>" result)))))

(defun gptelt-edit-edit-file-mcp (callback file-path old-string new-string &optional replace-all instruction)
  "MCP wrapper for edit-file that matches :args specification.
CALLBACK is first, then individual parameters as defined in :args.

Validates all required parameters and provides clear error messages."
  (when gptelt-edit-debug
    (message "[EDIT-DEBUG] edit_file: path=%s old=%s new=%s replace_all=%s instruction=%s"
             file-path
             (and (stringp old-string) (substring old-string 0 (min 20 (length old-string))))
             (and (stringp new-string) (substring new-string 0 (min 20 (length new-string))))
             replace-all
             (if instruction "provided" "nil")))

  (let ((callback (gptelt-edit--error-wrapping-callback callback)))
    ;; Validate required parameters
    (cond
     ((or (null file-path) (not (stringp file-path)) (string-empty-p file-path))
      (funcall callback "Error: Parameter 'file_path' is required and must be a non-empty string"))
     ((or (null old-string) (not (stringp old-string)))
      (funcall callback "Error: Parameter 'old_string' is required and must be a string"))
     ((or (null new-string) (not (stringp new-string)))
      (funcall callback "Error: Parameter 'new_string' is required and must be a string"))
     ((and replace-all (not (booleanp replace-all)))
      (funcall callback (format "Error: Parameter 'replace_all' must be a boolean (true/false), got: %S" replace-all)))
     ((and instruction (not (stringp instruction)))
      (funcall callback (format "Error: Parameter 'instruction' must be a string, got: %S" instruction)))
     (t
      ;; All validation passed - proceed with edit
      (gptelt-edit-edit-file
       callback file-path old-string new-string replace-all instruction)))))

(defun gptelt-edit-edit-buffer-mcp (callback buffer-name old-string new-string &optional replace-all instruction)
  "MCP wrapper for edit-buffer that matches :args specification.
CALLBACK is first, then individual parameters as defined in :args.

Validates all required parameters and provides clear error messages."
  (when gptelt-edit-debug
    (message "[EDIT-DEBUG] edit_buffer: buf=%s old=%s new=%s replace_all=%s instruction=%s"
             buffer-name
             (and (stringp old-string) (substring old-string 0 (min 20 (length old-string))))
             (and (stringp new-string) (substring new-string 0 (min 20 (length new-string))))
             replace-all
             (if instruction "provided" "nil")))

  (let ((callback (gptelt-edit--error-wrapping-callback callback)))
    ;; Validate required parameters
    (cond
     ((or (null buffer-name) (not (stringp buffer-name)) (string-empty-p buffer-name))
      (funcall callback "Error: Parameter 'buffer_name' is required and must be a non-empty string"))
     ((or (null old-string) (not (stringp old-string)))
      (funcall callback "Error: Parameter 'old_string' is required and must be a string"))
     ((or (null new-string) (not (stringp new-string)))
      (funcall callback "Error: Parameter 'new_string' is required and must be a string"))
     ((and replace-all (not (booleanp replace-all)))
      (funcall callback (format "Error: Parameter 'replace_all' must be a boolean (true/false), got: %S" replace-all)))
     ((and instruction (not (stringp instruction)))
      (funcall callback (format "Error: Parameter 'instruction' must be a string, got: %S" instruction)))
     (t
      ;; All validation passed - proceed with edit
      (gptelt-edit-edit-buffer
       callback buffer-name old-string new-string replace-all instruction)))))

(defun gptelt-edit-multi-edit-file-mcp (callback file-path edits)
  "MCP wrapper for multi-edit-file that matches :args specification.
CALLBACK is first, then file-path, then edits list/vector.

Validates all required parameters and provides clear error messages.
Automatically converts vector to list if needed."
  (when gptelt-edit-debug
    (message "[EDIT-DEBUG] multi_edit_file: path=%s edits=%S (type=%s)"
             file-path edits (type-of edits)))

  (let ((callback (gptelt-edit--error-wrapping-callback callback)))
    ;; Validate required parameters
    (cond
     ((or (null file-path) (not (stringp file-path)) (string-empty-p file-path))
      (funcall callback "Error: Parameter 'file_path' is required and must be a non-empty string"))
     ((null edits)
      (funcall callback "Error: Parameter 'edits' is required and must be a list or array"))
     ((not (or (listp edits) (vectorp edits)))
      (funcall callback (format "Error: Parameter 'edits' must be a list or array, got: %S" (type-of edits))))
     (t
      ;; Convert vector to list if needed
      (when (vectorp edits)
        (setq edits (append edits nil)))

      ;; Validate edits list is not empty
      (if (zerop (length edits))
          (funcall callback "Error: Parameter 'edits' must contain at least one edit")
        ;; All validation passed - proceed with multi-edit
        (gptelt-edit-multi-edit-file callback file-path edits))))))

(defun gptelt-edit-multi-edit-buffer-mcp (callback buffer-name edits)
  "MCP wrapper for multi-edit-buffer that matches :args specification.
CALLBACK is first, then buffer-name, then edits list/vector.

Validates all required parameters and provides clear error messages.
Automatically converts vector to list if needed."
  (when gptelt-edit-debug
    (message "[EDIT-DEBUG] multi_edit_buffer: buf=%s edits=%S (type=%s)"
             buffer-name edits (type-of edits)))

  (let ((callback (gptelt-edit--error-wrapping-callback callback)))
    ;; Validate required parameters
    (cond
     ((or (null buffer-name) (not (stringp buffer-name)) (string-empty-p buffer-name))
      (funcall callback "Error: Parameter 'buffer_name' is required and must be a non-empty string"))
     ((null edits)
      (funcall callback "Error: Parameter 'edits' is required and must be a list or array"))
     ((not (or (listp edits) (vectorp edits)))
      (funcall callback (format "Error: Parameter 'edits' must be a list or array, got: %S" (type-of edits))))
     (t
      ;; Convert vector to list if needed
      (when (vectorp edits)
        (setq edits (append edits nil)))

      ;; Validate edits list is not empty
      (if (zerop (length edits))
          (funcall callback "Error: Parameter 'edits' must contain at least one edit")
        ;; All validation passed - proceed with multi-edit
        (gptelt-edit-multi-edit-buffer callback buffer-name edits))))))

;;; Tool registration
(when (fboundp 'gptelt-make-tool)
  (gptelt-make-tool
   :name "edit_buffer"
   :function #'gptelt-edit-edit-buffer-mcp  ; Use MCP wrapper
   :async t
   :description (concat "Performs string replacements in buffers with intelligent matching. "
                        "This tool edits already open Emacs buffers by replacing old_string with new_string. "
                        "\n\nPARAMETER STRUCTURE:\n"
                        "{"
                        "  \"buffer_name\": \"string\" (required) - Name of the buffer to edit\n"
                        "  \"old_string\": \"string\" (required) - Text to be replaced\n"
                        "  \"new_string\": \"string\" (required) - Replacement text\n"
                        "  \"replace_all\": boolean (optional, default: false) - Replace all occurrences\n"
                        "  \"instruction\": \"string\" (optional) - Natural language hint for self-correction\n"
                        "}\n\n"
                        "MATCHING STRATEGIES (tried in order):\n"
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
   :function #'gptelt-edit-edit-file-mcp  ; Use MCP wrapper
   :async t
   :description (concat "Performs string replacements in files with intelligent matching. "
                        "This tool edits files by replacing old_string with new_string. The file will be opened "
                        "if not already loaded, and automatically saved after the edit. "
                        "\n\nPARAMETER STRUCTURE:\n"
                        "{"
                        "  \"file_path\": \"string\" (required) - Absolute path to the file\n"
                        "  \"old_string\": \"string\" (required) - Text to be replaced\n"
                        "  \"new_string\": \"string\" (required) - Replacement text\n"
                        "  \"replace_all\": boolean (optional, default: false) - Replace all occurrences\n"
                        "  \"instruction\": \"string\" (optional) - Natural language hint for self-correction\n"
                        "}\n\n"
                        "MATCHING STRATEGIES (tried in order):\n"
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
                        "For Lisp code, automatically validates parentheses balance and applies LSP formatting.")
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
  (let ((applied-count 0))
    (cl-labels
        ((process-next-edit (remaining-edits)
           (if (null remaining-edits)
               (funcall
                callback
                (format "Successfully applied %d edits to buffer: %s"
                        applied-count (buffer-name buffer)))
             ;; Process next edit
             (let* ((oe (car remaining-edits))
                    (old (plist-get oe :old_string))
                    (new (plist-get oe :new_string))
                    (replace-all (plist-get oe :replace_all))
                    (instruction (plist-get oe :instruction)))
               (gptelt-edit--edit-buffer-impl
                buffer old new
                (lambda (result)
                  ;; Check if the edit failed
                  (if (string-match-p "old_string not found\\|ERROR:" result)
                      ;; Edit failed - stop processing and report error
                      (let ((status-lines
                             (cl-loop for i from 1 to (length edits)
                                      collect
                                      (cond
                                       ((< i (1+ applied-count))
                                        (format "- Edit %d/%d: SUCCESS" i (length edits)))
                                       ((= i (1+ applied-count))
                                        (format "- Edit %d/%d: FAILED" i (length edits)))
                                       (t
                                        (format "- Edit %d/%d: SKIPPED (not applied)" i (length edits)))))))
                        (funcall callback
                                 (format "Multi-edit failed:\n%s\n\nError from failed edit:\n%s"
                                         (string-join status-lines "\n")
                                         result)))
                    ;; Edit succeeded - continue with next edit
                    (setq applied-count (1+ applied-count))
                    (process-next-edit (cdr remaining-edits))))
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
    (+gptel-tool-revert-to-be-visited-buffer buffer)
    (gptelt-edit--multi-edit-buffer-impl buffer edits callback)))

(comment
  (gptelt-edit-multi-edit-buffer
   'message
   "a.txt"
   '((:old_string "a"
      :new_string "aa")
     (:old_string "a"
      :new_string "aa")
     (:old_string "b"
      :new_string "aa")
     (:old_string "a"
      :new_string "aa")
     (:old_string "a"
      :new_string "aa")))

  (let ((f (make-temp-file "test-gptelt-edit")))
    (with-current-buffer (find-file-noselect f)
      (erase-buffer)
      (insert "old1, old2")
      (gptelt-edit-multi-edit-buffer
       'ignore
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
  (let* ((resolved-path (expand-file-name file-path))
         (buffer (gptelt-edit--get-buffer-for-file resolved-path)))
    (unless buffer
      (error "Could not open or create buffer for file: %s (resolved to: %s)"
             file-path resolved-path))
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
   :function #'gptelt-edit-multi-edit-buffer-mcp  ; Use MCP wrapper
   :async t
   :description (concat "Apply multiple edits to a buffer with intelligent matching. "
                        "Each edit is applied in order to the result of the previous edit. "
                        "\n\nPARAMETER STRUCTURE:\n"
                        "{"
                        "  \"buffer_name\": \"string\" (required) - Name of the buffer to edit\n"
                        "  \"edits\": array (required) - List of edit objects, applied sequentially\n"
                        "}\n\n"
                        "EDIT OBJECT STRUCTURE:\n"
                        "{"
                        "  \"old_string\": \"string\" (required) - Text to be replaced\n"
                        "  \"new_string\": \"string\" (required) - Replacement text\n"
                        "  \"replace_all\": boolean (optional, default: false) - Replace all occurrences\n"
                        "  \"instruction\": \"string\" (optional) - Natural language hint for self-correction\n"
                        "}\n\n"
                        "Advantages over single edits:\n"
                        "- More efficient for multiple changes to the same buffer\n"
                        "- Maintains context between related changes\n"
                        "- Single save operation after all changes\n"
                        "- Each edit uses flexible matching and LLM self-correction\n\n"
                        "Usage example:\n"
                        "multi_edit_buffer('app.py', [\n"
                        "  {\"old_string\": \"debug = False\", \"new_string\": \"debug = True\", \"instruction\": \"enable debug mode\"},\n"
                        "  {\"old_string\": \"version = '1.0'\", \"new_string\": \"version = '1.1'\", \"instruction\": \"bump version\"}\n"
                        "])\n\n"
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
   :function #'gptelt-edit-multi-edit-file-mcp  ; Use MCP wrapper
   :async t
   :description (concat "Apply multiple edits to a single file with intelligent matching. "
                        "Each edit operates on the result of the previous edit. The file is opened if not already loaded, "
                        "all edits are applied in order, and automatically saved. "
                        "\n\nPARAMETER STRUCTURE:\n"
                        "{"
                        "  \"file_path\": \"string\" (required) - Absolute path to the file\n"
                        "  \"edits\": array (required) - List of edit objects, applied sequentially\n"
                        "}\n\n"
                        "EDIT OBJECT STRUCTURE:\n"
                        "{"
                        "  \"old_string\": \"string\" (required) - Text to be replaced\n"
                        "  \"new_string\": \"string\" (required) - Replacement text\n"
                        "  \"replace_all\": boolean (optional, default: false) - Replace all occurrences\n"
                        "  \"instruction\": \"string\" (optional) - Natural language hint for self-correction\n"
                        "}\n\n"
                        "Best practices:\n"
                        "- Use for related changes that should be applied together\n"
                        "- Ensure old_string patterns account for previous edits in the sequence\n"
                        "- More efficient than multiple separate edit_file calls\n"
                        "- Each edit uses flexible matching and LLM self-correction\n\n"
                        "Usage example:\n"
                        "multi_edit_file('/path/to/config.js', [\n"
                        "  {\"old_string\": \"port: 3000\", \"new_string\": \"port: 8080\", \"instruction\": \"update port\"},\n"
                        "  {\"old_string\": \"env: 'dev'\", \"new_string\": \"env: 'prod'\", \"instruction\": \"switch to production\"}\n"
                        "])\n\n"
                        "For Lisp code, validates parentheses balance after each edit.")
   :args '((:name "file_path" :type string
            :description "Absolute path to the file to edit (must be absolute, not relative). Supports `~/` expansion.")
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


;;; edit-file.el ends here
