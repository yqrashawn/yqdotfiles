;;; .nixpkgs/.doom.d/gptel-tools/treesit.el -*- lexical-binding: t; -*-

(require 'treesit)

;;; Tree-sitter Symbol Listing

(defun treesit-list-symbols (language query-patterns)
  "List symbols in current buffer using tree-sitter."
  (when-let ((parser (treesit-parser-create language))
             (root-node (treesit-parser-root-node parser)))
    (treesit-query-capture root-node query-patterns)))

(defun treesit-list-clojure-symbols ()
  "List Clojure symbols (functions, vars, etc)."
  (treesit-list-symbols
   'clojure
   '((list_lit (sym_lit) @function.name) @function.def
     (sym_lit) @symbol)))

(defun treesit-list-elisp-symbols ()
  "List top-level Emacs Lisp symbols (defuns, defvars, etc)."
  (treesit-list-symbols
   'elisp
   '((source_file (function_definition (symbol) @name) @definition))))

(defun treesit-list-js-symbols ()
  "List JavaScript symbols (functions, variables, classes)."
  (treesit-list-symbols
   'javascript
   '((function_declaration name: (identifier) @function.name)
     (function_expression name: (identifier) @function.name)
     (arrow_function) @function.anonymous
     (variable_declarator name: (identifier) @variable.name)
     (class_declaration name: (identifier) @class.name)
     (method_definition name: (property_identifier) @method.name)
     (identifier) @symbol)))

(defun treesit-list-ts-symbols ()
  "List TypeScript symbols."
  (treesit-list-symbols
   'typescript
   '((function_declaration name: (identifier) @function.name)
     (method_definition name: (property_identifier) @method.name)
     (class_declaration name: (type_identifier) @class.name)
     (interface_declaration name: (type_identifier) @interface.name)
     (type_alias_declaration name: (type_identifier) @type.name)
     (variable_declarator name: (identifier) @variable.name)
     (identifier) @symbol)))

(defun my-list-all-symbols (&optional buffer)
  "List all symbols in current buffer based on major mode."
  (interactive)
  (condition-case err
      (let* ((source-buffer (or buffer (current-buffer)))
             (mj-mode (with-current-buffer source-buffer major-mode))
             (symbols
              (with-current-buffer source-buffer
                (pcase mj-mode
                  ('clojure-mode (treesit-list-clojure-symbols))
                  ('emacs-lisp-mode (treesit-list-elisp-symbols))
                  ('lisp-interaction-mode (treesit-list-elisp-symbols))
                  ('js-mode (treesit-list-js-symbols))
                  ('js2-mode (treesit-list-js-symbols))
                  ('typescript-mode (treesit-list-ts-symbols))
                  ('typescript-ts-mode (treesit-list-ts-symbols))
                  (_ (user-error "Unsupported major mode: %s" mj-mode))))))
        (if symbols
            (with-current-buffer (get-buffer-create "*Symbols*")
              (erase-buffer)
              (insert (format "Symbols in %s (major-mode: %s)\n\n"
                              (buffer-name source-buffer) mj-mode))
              (dolist (capture symbols)
                (let ((name (car capture))
                      (node (cdr capture)))
                  (when node
                    (condition-case node-err
                        (let* ((node-start (treesit-node-start node))
                               (node-text (treesit-node-text node))
                               (line-num (with-current-buffer source-buffer
                                           (save-excursion
                                             (goto-char (max 1 (min node-start (point-max))))
                                             (line-number-at-pos)))))
                          (insert (format "%s: %s (line %d, pos %d)\n"
                                          name
                                          (or node-text "")
                                          (or line-num 1)
                                          node-start)))
                      (error (insert (format "%s: <error getting node info: %s>\n"
                                             name (error-message-string node-err))))))))
              (pop-to-buffer (current-buffer)))
          (message "No symbols found in current buffer")))
    (error (message "Error listing symbols: %s" (error-message-string err)))))

(defun treesit-debug-symbols ()
  "Debug function to test tree-sitter symbol detection."
  (interactive)
  (message "Major mode: %s" major-mode)
  (message "Buffer: %s" (buffer-name))
  (message "Tree-sitter available: %s" (treesit-available-p))
  (let ((lang (pcase major-mode
                ('clojure-mode 'clojure)
                ('emacs-lisp-mode 'elisp)
                ('lisp-interaction-mode 'elisp)
                ('js-mode 'javascript)
                ('js2-mode 'javascript)
                ('typescript-mode 'typescript)
                ('typescript-ts-mode 'typescript))))
    (if lang
        (progn
          (message "Detected language: %s" lang)
          (message "Language available: %s" (treesit-language-available-p lang))
          (condition-case err
              (let ((parser (treesit-parser-create lang)))
                (message "Parser created successfully")
                (message "Root node: %s" (treesit-parser-root-node parser)))
            (error (message "Parser creation failed: %s" err))))
      (message "No supported language for major mode: %s" major-mode))))

(provide 'treesit-tools)

;;; treesit.el ends here
