;;; .nixpkgs/.doom.d/gptel-tools/clojure.el -*- lexical-binding: t; -*-

(require 'cider)

;;; utilities
(defun gptelt-clojure--ensure-workspace (type &optional ns)
  (when (eq type 'clj)
    (unless (++workspace-clj?)
      (error "This is not a clojure project"))
    (unless (++workspace-clj-repl-connected?)
      (error "Clojure nREPL is not connected"))
    (when ns
      (unless (member ns (cider-sync-request:ns-list))
        (error "Namespace '%s' is not available" ns))))
  (when (eq type 'cljs)
    (unless (++workspace-cljs?)
      (error "This is not a clojure project"))
    (unless (++workspace-cljs-repl-connected?)
      (error "Clojure nREPL is not connected"))))

(defun gptel-clojure--resolve-file-path (file-path)
  (let* ((proj-root (++workspace-current-project-root))
         (abs-path (if (f-absolute-p file-path)
                       file-path
                     (expand-file-name file-path proj-root))))
    abs-path))

(defun gptel-clojure--ensure-current-project-file (file-path)
  (let ((abs-path (gptel-clojure--resolve-file-path file-path)))
    (unless (file-exists-p abs-path)
      (error "File '%s' does not exist" abs-path))
    (unless (string-prefix-p
             (file-truename (++workspace-current-project-root))
             (file-truename abs-path))
      (error "File '%s' is not in current clojure project" abs-path))))

;;; list all loaded namespace
(defun gptelt-clj-list-ns (&optional filter-regex)
  "List all loaded namespaces in clj nREPL connection.
Optional FILTER-REGEX filters the namespace list before returning."
  (gptelt-clojure--ensure-workspace 'clj)
  (let ((ns-list (cider-sync-request:ns-list)))
    (if filter-regex
        (seq-filter
         (lambda (ns)
           (string-match-p
            (rxt-pcre-to-elisp filter-regex) ns))
         ns-list)
      ns-list)))

(comment
  (gptelt-clj-list-ns)
  (gptelt-clj-list-ns "nrepl"))

;;; list all classpath entries
(defun gptelt-clj-list-classpath (&optional filter-regex)
  "List all classpath entries in clj nREPL connection.
Optional FILTER-REGEX filters the classpath list before returning."
  (gptelt-clojure--ensure-workspace 'clj)
  (let ((classpath-list (cider-classpath-entries)))
    (if filter-regex
        (seq-filter
         (lambda (classpath)
           (string-match-p
            (rxt-pcre-to-elisp filter-regex) classpath))
         classpath-list)
      classpath-list)))

(comment
  (gptelt-clj-list-classpath)
  (gptelt-clj-list-classpath "\\.jar$"))

;;; eval clj string
(defun gptelt-eval-clj-string (clj-string &optional namespace)
  "Evaluate CLJ-STRING in NAMESPACE (defaults to 'user') and return result or error.
Checks if clj nREPL is connected and namespace is available before evaluation."
  (gptelt-clojure--ensure-workspace 'clj (or namespace "user"))
  (let ((ns (or namespace "user"))
        allow?)
    (gptelt-clojure--ensure-workspace 'clj ns)

    (with-temp-buffer
      (insert clj-string)
      (let ((win (display-buffer (current-buffer))))
        (clojure-mode)
        (setq allow?
              (y-or-n-p (format "Evaluate clj code in ns %s? " ns)))
        (quit-restore-window win 'bury)))

    (if allow?
        ;; Evaluate the code
        (condition-case err
            (let ((result
                   (cider-nrepl-sync-request:eval clj-string nil ns)))
              (or (nrepl-dict-get result "value")
                  (nrepl-dict-get result "err")
                  (error "Unexpected evaluation result")))
          (error (format "Error evaluating clj code: %s" (error-message-string err))))
      "User rejected the eval request")))

(comment
  (gptelt-eval-clj-string "+" "clojure.core"))

;;; get buffer namespace
(defun gptelt-clj-get-buffer-ns (buffer-name)
  "Get the namespace of the given BUFFER-NAME containing Clojure code.
Returns the namespace string if found, or nil if no namespace is detected."
  (gptelt-clojure--ensure-workspace 'clj)
  (let ((buffer (get-buffer buffer-name)))
    (unless buffer
      (error "Buffer '%s' not found" buffer-name))
    (with-current-buffer buffer
      (unless (or (eq major-mode 'clojure-mode)
                  (eq major-mode 'clojurescript-mode)
                  (eq major-mode 'clojurec-mode))
        (error "Buffer '%s' is not a Clojure buffer (mode: %s)" buffer-name major-mode))
      (save-excursion
        (goto-char (point-min))
        (when-let ((ns-form (cider-ns-form)))
          (let ((ns-name (cider-get-ns-name)))
            (if ns-name
                ns-name
              (error "Could not determine namespace from buffer '%s'" buffer-name))))))))

(comment
  (gptelt-clj-get-buffer-ns (buffer-name (find-file-noselect "../../src/user.clj")))
  (gptelt-clj-get-buffer-ns "some-clj-buffer"))

;;; get file namespace
(defun gptelt-clj-get-file-ns (file-path)
  "Get the namespace of the given FILE-PATH containing Clojure code.
Ensures file is in current project and is a .clj, .cljc, or .cljs file.
Returns the namespace string if found, or an error if not."
  ;; Ensure clj workspace and nREPL
  (gptelt-clojure--ensure-workspace 'clj)
  (gptel-clojure--ensure-current-project-file file-path)
  (let ((abs-path (gptel-clojure--resolve-file-path file-path)))
    (let ((buf (find-file-noselect abs-path)))
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-min))
          (when-let ((ns-form (cider-ns-form)))
            (let ((ns-name (cider-get-ns-name)))
              (if ns-name
                  ns-name
                (error "Could not determine namespace from file '%s'" abs-path)))))))))

(comment
  (gptelt-clj-get-file-ns "src/user.clj"))

;;; get namespace file info
(defun gptelt-clj-get-ns-file-url (namespace)
  "Get file information for NAMESPACE.
Returns a list with: (file-path file-exists-p total-lines).
Ensures clj workspace and nREPL connection before proceeding."
  (gptelt-clojure--ensure-workspace 'clj namespace)
  (condition-case err
      (let* ((file-path (cider-sync-request:ns-path namespace t)))
        (if (string= file-path "")
            (error "File for ns %s does not exist, this usually means this namespace is not loaded" namespace)
          file-path))
    (error (error "Error getting namespace file info: %s" (error-message-string err)))))

(comment
  (gptelt-clj-get-ns-file-url "user")
  (gptelt-clj-get-ns-file-url "users"))

;;; get symbol documentation
(defun gptelt-clj-get-doc-for-symbol (symbol &optional namespace)
  "Get documentation for SYMBOL in optional NAMESPACE.
Returns the documentation string if available, or an error if not found.
Ensures clj workspace and nREPL connection before proceeding."
  (gptelt-clojure--ensure-workspace 'clj)
  (condition-case err
      (let* ((qualified-symbol (if namespace
                                   (format "%s/%s" namespace symbol)
                                 symbol))
             (info (cider-var-info qualified-symbol)))
        (if info
            (let ((doc (nrepl-dict-get info "doc"))
                  (name (nrepl-dict-get info "name"))
                  (ns (nrepl-dict-get info "ns"))
                  (arglists (nrepl-dict-get info "arglists-str"))
                  (macro (nrepl-dict-get info "macro"))
                  (special (nrepl-dict-get info "special-form")))
              (format "%s%s%s%s%s"
                      (if ns (format "%s/%s" ns name) name)
                      (if arglists (format "\n%s" arglists) "")
                      (cond
                       (special "\nSpecial Form")
                       (macro "\nMacro")
                       (t ""))
                      (if doc (format "\n\n%s" doc) "\n\nNo documentation available.")
                      ""))
          (error "Symbol '%s' not found or not resolved" qualified-symbol)))
    (error (error "Error getting symbol documentation: %s" (error-message-string err)))))

(comment
  (gptelt-clj-get-doc-for-symbol "defn"))

;;; evaluate buffer
(defun gptelt-clj-eval-buffer (buffer-name)
  "Evaluate a Clojure buffer by BUFFER-NAME.
Ensures the buffer exists, its file is in the current project, and evaluates it."
  (gptelt-clojure--ensure-workspace 'clj)
  (let* ((buffer (get-buffer buffer-name))
         (shown-before (get-buffer-window buffer))
         win)
    (unless buffer
      (error "Buffer '%s' not found" buffer-name))
    (let ((file-path (buffer-file-name buffer)))
      (unless file-path
        (error "Buffer '%s' is not associated with a file" buffer-name))
      (gptel-clojure--ensure-current-project-file file-path)
      (with-current-buffer buffer
        (unless (or (eq major-mode 'clojure-mode)
                    (eq major-mode 'clojurec-mode))
          (error "Buffer '%s' is not a Clojure buffer (mode: %s)" buffer-name major-mode))

        (setq win (unless shown-before (display-buffer buffer)))

        (when (or
               shown-before
               (y-or-n-p (format "Evaluate Clojure file %s? " file-path)))
          (unwind-protect
              (condition-case err
                  (progn
                    (cider-load-buffer buffer)
                    (format "Successfully evaluated buffer '%s'" buffer-name))
                (error (format "Error evaluating buffer '%s': %s" buffer-name (error-message-string err))))
            (when (and win (window-live-p win))
              (quit-restore-window win 'bury))))))))

(comment
  (gptelt-clj-eval-buffer
   (find-file-noselect "../../src/user.clj")))

;;; evaluate file
;;; evaluate file
(defun gptelt-clj-eval-file (file-path)
  "Evaluate a Clojure source file by FILE-PATH.
Ensures the file exists, is in the current project, loads it into buffer,
shows buffer if not visible, asks for user permission, and evaluates it."
  (gptelt-clojure--ensure-workspace 'clj)
  (gptel-clojure--ensure-current-project-file file-path)
  (let* ((abs-path (gptel-clojure--resolve-file-path file-path))
         (buffer (find-file-noselect abs-path))
         (eval-ok? nil)
         (eval-err nil))
    (gptelt-clj-eval-buffer buffer)))

(comment
  (gptelt-clj-eval-file "src/user.clj"))

;;; gptel tool registration
(when (fboundp 'gptelt-make-tool)
  (gptelt-make-tool
   :name "clj_list_ns"
   :function #'gptelt-clj-list-ns
   :description "List all loaded namespaces in clj nREPL connection. Supports optional regex filtering."
   :args '((:name "filter_regex"
            :type string
            :optional t
            :description "Optional regex pattern to filter namespace names"))
   :category "clojure"
   :confirm nil
   :include t)

  (gptelt-make-tool
   :name "clj_list_classpath"
   :function #'gptelt-clj-list-classpath
   :description "List all classpath entries in clj nREPL connection. Supports optional regex filtering."
   :args '((:name "filter_regex"
            :type string
            :optional t
            :description "Optional regex pattern to filter classpath entry paths"))
   :category "clojure"
   :confirm nil
   :include t)

  (gptelt-make-tool
   :name "clj_eval_string"
   :function #'gptelt-eval-clj-string
   :description "Evaluate given clojure code string in given namespace, get eval result or error."
   :args '((:name "clj_string"
            :type string
            :description "The Clojure code string to evaluate")
           (:name "namespace"
            :type string
            :optional t
            :description "The namespace to evaluate in (optional, defaults to 'user')"))
   :category "clojure"
   :confirm nil
   :include t)
  (gptelt-make-tool
   :name "clj_get_buffer_ns"
   :function #'gptelt-clj-get-buffer-ns
   :description "Get the namespace of a Clojure buffer by buffer name."
   :args '((:name "buffer_name"
            :type string
            :description "The name of the Clojure buffer to get the namespace from"))
   :category "clojure"
   :confirm nil
   :include t)

  (gptelt-make-tool
   :name "clj_get_file_ns"
   :function #'gptelt-clj-get-file-ns
   :description "Get the namespace of a Clojure source file in the current project, given the file path. File must be under project root and be a .clj, .cljc."
   :args '((:name "file_path"
            :type string
            :description "Path to the Clojure file in the current project root (relative or absolute)"))
   :category "clojure"
   :confirm nil
   :include t)

  (gptelt-make-tool
   :name "clj_get_ns_file_url"
   :function #'gptelt-clj-get-ns-file-url
   :description "Get file url for a given namespace: file path"
   :args '((:name "namespace"
            :type string
            :description "The namespace to get file information for"))
   :category "clojure"
   :confirm nil
   :include t)

  (gptelt-make-tool
   :name "clj_get_doc_for_symbol"
   :function #'gptelt-clj-get-doc-for-symbol
   :description "Get documentation for a Clojure symbol (function, macro, var, namespace, etc.). Returns formatted documentation including arglists, type, and docstring."
   :args '((:name "symbol"
            :type string
            :description "The Clojure symbol to get documentation for (e.g., 'map', 'reduce', 'defn')")
           (:name "namespace"
            :type string
            :optional t
            :description "Optional namespace to qualify the symbol, default to clojure.core (e.g., 'clojure.core')"))
   :category "clojure"
   :confirm nil
   :include t)

  (gptelt-make-tool
   :name "clj_eval_buffer"
   :function #'gptelt-clj-eval-buffer
   :description "Evaluate a Clojure buffer by buffer name. Ensures the buffer exists, its file is in the current project, and evaluates the entire buffer."
   :args '((:name "buffer_name"
            :type string
            :description "The name of the Clojure buffer to evaluate"))
   :category "clojure"
   :confirm nil
   :include t)

  (gptelt-make-tool
   :name "clj_eval_file"
   :function #'gptelt-clj-eval-file
   :description "Evaluate a Clojure source file by file path. Ensures the file exists, is in the current project, and evaluates the entire file."
   :args '((:name "file_path"
            :type string
            :description "Path to the Clojure file in the current project root (relative or absolute)"))
   :category "clojure"
   :confirm nil
   :include t))
