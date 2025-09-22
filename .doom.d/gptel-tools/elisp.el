;;; .nixpkgs/.doom.d/gptel-tools/elisp.el -*- lexical-binding: t; -*-

;;; Tool: Evaluate Elisp Buffer (ask for confirmation, show buffer if not visible)
(defun gptelt-evaluate-elisp-buffer (buffer-name)
  "Evaluate elisp BUFFER-NAME after confirming with user. Show buffer if not visible. Bury if not shown before."
  (interactive)
  (let* ((buf (get-buffer buffer-name))
         (buf-file (buffer-file-name buf))
         (shown-before (get-buffer-window buf))
         (eval-ok? nil)
         (eval-err nil)
         win)
    (unless buf (error "Buffer not found: %s" buffer-name))
    (unless (eq (buffer-local-value 'major-mode buf) 'emacs-lisp-mode)
      (error "Buffer %s is not in emacs-lisp-mode" buffer-name))
    (setq win (unless shown-before (display-buffer buf)))
    (when (or
           shown-before
           (y-or-n-p (format "Evaluate buffer %s? " buffer-name)))
      (setq eval-err
            (condition-case err
                (progn
                  (eval-buffer buf t buf-file nil t)
                  (setq eval-ok? t))
              (error (format "Error evaluate buffer `%s`: %s"
                             buf
                             (error-message-string err)))))
      (message "Evaluated buffer: %s" buffer-name))
    (when (and win (window-live-p win))
      (quit-restore-window win 'bury))
    (if eval-ok?
        (format "Successfully evaluated buffer `%s`" buffer-name)
      eval-err)))

(comment
  (gptelt-evaluate-elisp-buffer (current-buffer)))

;;; Tool: Evaluate Elisp File (load file into buffer, then evaluate, with confirmation)
(defun gptelt-evaluate-elisp-file (file-path)
  "Load FILE-PATH into buffer and evaluate after confirming with user. Show buffer if not visible. Bury if not shown before."
  (interactive)
  (unless (file-name-absolute-p file-path) (error "file-path must be absolute"))
  (unless (string-suffix-p ".el" file-path)
    (error "File %s is not an .el file" file-path))
  (gptelt-evaluate-elisp-buffer (find-file-noselect file-path)))

(comment
  (gptelt-evaluate-elisp-file
   (expand-file-name ".doom.d/gptel-tools/elisp.el" "~/.nixpkgs")))

;;; Tool: Evaluate Elisp String (put string in temp buffer, eval after confirmation, return result)
(defun gptelt-evaluate-elisp-string (elisp-string)
  "Put ELISP-STRING into a temp buffer for confirmation, then eval using (eval (read ...)), return result."
  (interactive)
  (let* ((buf (generate-new-buffer " *gptel-eval-elisp*"))
         (eval-result nil)
         (eval-err nil)
         win)
    (with-current-buffer buf
      (emacs-lisp-mode)
      (insert elisp-string)
      (goto-char (point-min)))
    (setq win (display-buffer buf))
    (when (or t (y-or-n-p "Evaluate elisp string in temp buffer? "))
      (condition-case err
          (setq eval-result (eval (read elisp-string)))
        (error (setq eval-err (format "Error evaluating elisp string: %s" (error-message-string err))))))
    (when (and win (window-live-p win))
      (quit-restore-window win 'bury))
    (when (buffer-live-p buf) (kill-buffer buf))
    (if eval-err
        eval-err
      (format "Result: %S" eval-result))))

(comment
  (gptelt-evaluate-elisp-string "(print 1)"))

;;; Tool: Get Function Documentation
(defun gptelt-get-function-doc (function-name)
  "Get documentation for FUNCTION-NAME. Returns the docstring or error message."
  (interactive)
  (let ((func-symbol (if (stringp function-name)
                         (intern-soft function-name)
                       function-name)))
    (if (and func-symbol (fboundp func-symbol))
        (let ((doc (documentation func-symbol)))
          (if doc
              doc
            (format "Function `%s` exists but has no documentation" function-name)))
      (format "Function `%s` is not defined" function-name))))

(comment
  (gptelt-get-function-doc "save-buffer"))

;;; Tool: Get Variable Documentation
(defun gptelt-get-variable-doc (variable-name)
  "Get documentation for VARIABLE-NAME. Returns the docstring or error message."
  (interactive)
  (let ((var-symbol (if (stringp variable-name)
                        (intern-soft variable-name)
                      variable-name)))
    (if (and var-symbol (boundp var-symbol))
        (let ((doc (documentation-property var-symbol 'variable-documentation)))
          (if doc
              doc
            (format "Variable `%s` exists but has no documentation" variable-name)))
      (format "Variable `%s` is not defined" variable-name))))

(comment
  (gptelt-get-variable-doc "load-path"))

;;; Tool: Info Lookup Symbol
(defun gptelt-info-lookup-symbol (symbol)
  "Look up SYMBOL in Elisp Info documentation and return the complete documentation node."
  (interactive)
  (unless (stringp symbol)
    (format "Invalid symbol name: %s" symbol))
  (let ((symbol-name symbol)
        (result nil))
    (condition-case nil
        (with-temp-buffer
          (let ((mode 'emacs-lisp-mode)
                (info-buf nil)
                (node nil)
                (manual nil)
                (content nil))

            (emacs-lisp-mode)

            (save-window-excursion
              (info-lookup-symbol symbol-name mode)

              (setq info-buf (get-buffer "*info*"))

              (when info-buf
                (with-current-buffer info-buf
                  (goto-char (point-min))
                  (when (re-search-forward
                         "^File: \\([^,]+\\),  Node: \\([^,\n]+\\)"
                         nil t)
                    (setq manual (match-string 1))
                    (setq node (match-string 2))
                    (when (string-match "\\.info\\'" manual)
                      (setq manual
                            (substring manual 0 (match-beginning 0)))))

                  (goto-char (point-min))
                  (when (re-search-forward "^File: [^,]+,  Node: [^,\n]+.*\n" nil t)
                    (let ((start (point))
                          (end nil))
                      (if (re-search-forward "^\^_" nil t)
                          (setq end (match-beginning 0))
                        (setq end (point-max)))
                      (setq content (buffer-substring-no-properties start end)))))))

            (when (and node content)
              (setq result
                    (format "Found symbol '%s' in manual '%s', node '%s'\n\nInfo reference: (%s)%s\n\nContent:\n%s"
                            symbol-name manual node manual node content)))))
      (error
       (setq result
             (format "Symbol '%s' not found in Elisp Info documentation" symbol-name))))

    (or result
        (format "Symbol '%s' not found in Elisp Info documentation" symbol-name))))

(comment
  (gptelt-info-lookup-symbol "defun")
  (gptelt-info-lookup-symbol "mapcar"))

;;; Tool: Get Function Source Code
(defun gptelt-get-function-source-code (function-name)
  "Get source code for FUNCTION-NAME. Returns the source code or error message."
  (interactive)
  (let ((func-symbol (if (stringp function-name)
                         (intern-soft function-name)
                       function-name)))
    (if (and func-symbol (fboundp func-symbol))
        (condition-case err
            (let* ((lib (or (symbol-file func-symbol 'defun)
                            (help-C-file-name func-symbol 'subr)))
                   (buf-point (find-function-search-for-symbol func-symbol nil lib)))
              (if (and buf-point (cdr buf-point))
                  (with-current-buffer (car buf-point)
                    (save-excursion
                      (goto-char (cdr buf-point))
                      (let ((start (point))
                            (end (progn (forward-sexp) (point))))
                        (buffer-substring-no-properties start end))))
                (format "Could not locate source for function `%s`" function-name)))
          (error (format "Error retrieving source for `%s`: %s"
                         function-name (error-message-string err))))
      (format "Function `%s` is not defined" function-name))))

(comment
  (gptelt-get-function-source-code "save-buffer")
  (gptelt-get-function-source-code "gptelt-get-function-doc"))

;;; Tool: Search Function by Regex
(defun gptelt-search-function-by-regex (regex-filter &optional limit)
  "Search for elisp function names matching REGEX-FILTER. Return up to LIMIT results (default 100)."
  (interactive)
  (let ((limit (or limit 100))
        (count 0)
        (results '())
        (regex (rxt-pcre-to-elisp regex-filter)))
    (mapatoms
     (lambda (symbol)
       (when (and (< count limit)
                  (fboundp symbol)
                  (string-match-p regex (symbol-name symbol)))
         (push (symbol-name symbol) results)
         (setq count (1+ count)))))
    (if results
        (format "Found %d function(s) matching '%s':\n%s"
                (length results)
                regex-filter
                (mapconcat #'identity (nreverse results) "\n"))
      (format "No functions found matching regex: %s" regex-filter))))

(comment
  (gptelt-search-function-by-regex "^gptelt-(rg|read)" 10)
  (gptelt-search-function-by-regex "save" 5))

;;; Tool: Run ERT test by selector regex and return summary report
(defun gptelt-run-ert-test (test_selector_regex)
  "Run ERT tests matching TEST_SELECTOR_REGEX, return summary report as string. Handles 'test-started and 'test-ended events."
  (let ((msgb (generate-new-buffer "*temp-ert-report*" t)))
    (letf!
      ((defadvice message (:around (fn msg &rest args))
         (with-current-buffer msgb
           (insert (apply fn msg args))
           (insert "\n"))))
      (ert-run-tests-batch test_selector_regex)
      (let ((report (with-current-buffer msgb (buffer-string))))
        (kill-buffer msgb)
        report))))

(comment
  (gptelt-run-ert-test "list-buffers-test")
  (gptelt-run-ert-test "clj-test-"))

;;; gptel tool registration
(when (fboundp 'gptelt-make-tool)
  (gptelt-make-tool
   :name "evaluate_elisp_buffer"
   :function #'gptelt-evaluate-elisp-buffer
   :description "Evaluate emacs-lisp-mode a buffer by buffer name."
   :args '((:name "buffer_name" :type string :description "The name of the buffer to evaluate"))
   :category "elisp"
   :confirm nil
   :include t)

  (gptelt-make-tool
   :name "evaluate_elisp_file"
   :function #'gptelt-evaluate-elisp-file
   :description "Evaluate a \".el\" file by loading it as a buffer and evaluating."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the elisp file to evaluate"))
   :category "elisp"
   :confirm nil
   :include t)

  (gptelt-make-tool
   :name "run_ert_tests"
   :function #'gptelt-run-ert-test
   :description "Run elisp ert test(s) by selector regex, return summary report."
   :args '((:name "test_selector_regex" :type string :description "Regex selector for tests to run."))
   :category "elisp"
   :confirm nil
   :include t)

  (gptelt-make-tool
   :name "evaluate_elisp_string"
   :function #'gptelt-evaluate-elisp-string
   :description "Evaluate emacs-lisp code from a string in a temp buffer, after user confirmation. Returns the result string."
   :args '((:name "elisp_string" :type string :description "The elisp code string to evaluate."))
   :category "elisp"
   :confirm nil
   :include t)

  (gptelt-make-tool
   :name "elisp_get_function_doc"
   :function #'gptelt-get-function-doc
   :description "Get documentation for an Emacs Lisp function."
   :args '((:name "function_name" :type string :description "The name of the function to get documentation for"))
   :category "elisp"
   :confirm nil
   :include t)

  (gptelt-make-tool
   :name "elisp_get_function_source_code"
   :function #'gptelt-get-function-source-code
   :description "Get source code for an Emacs Lisp function."
   :args '((:name "function_name" :type string :description "The name of the function to get source code for"))
   :category "elisp"
   :confirm nil
   :include t)

  (gptelt-make-tool
   :name "elisp_search_function_by_regex"
   :function #'gptelt-search-function-by-regex
   :description "Search for elisp function names matching a regex pattern. Returns up to limit results (default 100)."
   :args '((:name "regex_filter" :type string :description "Regex pattern to match function names against")
           (:name "limit" :type integer :description "Maximum number of results to return (optional, default 100)"))
   :category "elisp"
   :confirm nil
   :include t)

  (gptelt-make-tool
   :name "elisp_get_variable_doc"
   :function #'gptelt-get-variable-doc
   :description "Get documentation for an Emacs Lisp variable."
   :args '((:name "variable_name" :type string :description "The name of the variable to get documentation for"))
   :category "elisp"
   :confirm nil
   :include t)

  (gptelt-make-tool
   :name "elisp_info_lookup_symbol"
   :function #'gptelt-info-lookup-symbol
   :description "Look up Elisp symbols in Info documentation and return the complete documentation node. Returns the full content of the Info node containing the symbol's documentation from the Emacs Lisp Reference Manual."
   :args '((:name "symbol" :type string :description "The symbol to look up (string)"))
   :category "elisp"
   :confirm nil
   :include t))
