;;; .nixpkgs/.doom.d/gptel-tools/elisp.el -*- lexical-binding: t; -*-

(require 'find-func)

;;; Tool 1: Given an elisp function name, return its docstring

(defun gptel-elisp-get-docstring (func-name)
  "Return the docstring of elisp function named FUNC-NAME (string).
Returns nil if not found or no doc is available."
  (let* ((sym (and (stringp func-name) (intern-soft func-name)))
         (doc (and (functionp sym) (documentation sym t))))
    (or doc (format "No docstring found for `%s`" func-name))))

;;; Tool 2: Given an elisp function name, return its source code

(defun gptel-elisp-get-source (func-name)
  "Return the source code of elisp function named FUNC-NAME (string).
Returns an error message if source is not found."
  (let* ((sym (and (stringp func-name) (intern-soft func-name))))
    (cond
     ((not (functionp sym))
      (format "No such function: %s" func-name))
     (t
      (condition-case err
          (let* ((location (find-function-noselect sym))
                 (buf (car location))
                 (pos (cdr location)))
            (when (and buf pos)
              (with-current-buffer buf
                (save-excursion
                  (goto-char pos)
                  (let ((sexp (read (current-buffer))))
                    (format "%S" sexp))))))
        (error (format "Could not retrieve source for `%s` (%s)" func-name (error-message-string err))))))))

;;; Tool 3: Find elisp callable functions by regex pattern

(defun gptel-elisp-find-functions (regex-pattern)
  "Find all elisp callable functions matching REGEX-PATTERN (string).
Returns a list of matching function names, or an error message if the regex is invalid."
  (condition-case err
      (let ((matches '())
            (case-fold-search nil)) ; case-sensitive matching
        (mapatoms
         (lambda (sym)
           (when (and (fboundp sym)
                      (string-match-p regex-pattern (symbol-name sym)))
             (push (symbol-name sym) matches))))
        (if matches
            (format "Found %d functions matching '%s':\n%s"
                    (length matches)
                    regex-pattern
                    (mapconcat 'identity (sort matches 'string<) "\n"))
          (format "No functions found matching pattern: %s" regex-pattern)))
    (error (format "Invalid regex pattern '%s': %s" regex-pattern (error-message-string err)))))

;;; gptel tool registration

;; (when (fboundp 'gptel-make-tool)
;;   (gptel-make-tool
;;    :name "elisp-get-doc"
;;    :function #'gptel-elisp-get-docstring
;;    :description "Given an elisp function name (string), return its docstring, or an error message if not found."
;;    :args (list '(:name "func-name" :type string :description "Name of the elisp function (string or symbol)"))
;;    :category "elisp"
;;    :confirm nil
;;    :include t)

;;   (gptel-make-tool
;;    :name "elisp-get-source"
;;    :function #'gptel-elisp-get-source
;;    :description "Given an elisp function name (string), return its elisp source code if available, or an error message if not found."
;;    :args (list '(:name "func-name" :type string :description "Name of the elisp function (string or symbol)"))
;;    :category "elisp"
;;    :confirm nil
;;    :include t)

;;   (gptel-make-tool
;;    :name "elisp-find-functions"
;;    :function #'gptel-elisp-find-functions
;;    :description "Find all elisp callable functions matching a regex pattern. Returns a list of matching function names, or an error message if the regex is invalid."
;;    :args (list '(:name "regex-pattern" :type string :description "Regular expression pattern to match function names"))
;;    :category "elisp"
;;    :confirm nil
;;    :include t))
