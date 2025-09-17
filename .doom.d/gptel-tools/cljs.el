;;; .nixpkgs/.doom.d/gptel-tools/cljs.el -*- lexical-binding: t; -*-

(defun gptelt-cljs-ensure-helper-loaded ()
  (gptelt-cider--eval-buffer
   'clj
   (find-file-noselect"~/.nixpkgs/env/dev/cljs_helper.clj")
   t t))

;;; list all loaded namespace
(defun gptelt-cljs-list-ns (build-id _runtime-id &optional regex-filter)
  (gptelt-cljs-ensure-helper-loaded)
  (gptelt-eval--clj-string
   (if regex-filter
       (format "(get-build-namespaces %s \"%s\")" build-id regex-filter)
     (format "(get-build-namespaces %s)" build-id))
   "cljs-helper"
   t))

(comment
  (gptelt-cljs-list-ns :ground 1)
  (gptelt-cljs-list-ns :ground 1 "user")
  (gptelt-cljs-list-ns "ajax")
  (gptelt-cljs-list-ns "json")
  (gptelt-cljs-list-ns "router"))

;;; list all running builds
(defun gptelt-cljs-get-project-states ()
  (gptelt-cljs-ensure-helper-loaded)
  (gptelt-eval--clj-string "(get-shadow-cljs-info)" "cljs-helper" t))

(comment
  (gptelt-cljs-get-project-states))

;;; eval cljs string
(defun gptel-cljs-eval-string (build-id runtime-id cljs-code &optional ns)
  (gptelt-eval--clj-string
   (format "(cljs-eval %s %d \"%s\" %s)" build-id runtime-id cljs-code ns)
   "cljs-helper" t))

(comment
  (gptel-cljs-eval-string :ground 10 "js/location.href")
  (gptel-cljs-eval-string :ground 10 "js/document.body")
  (gptel-cljs-eval-string :ground 10 "js/location.hreff.f"))

;;; eval buffer
(defun gptelt-cljs-eval-buffer (buffer-name)
  "Evaluate a Clojurescript buffer by BUFFER-NAME.

Ensures the buffer exists, its file is in the current project, and evaluates it."
  (gptelt-cider--eval-buffer 'cljs buffer-name))

;;; eval file
(defun gptelt-cljs-eval-file (file-path)
  "Evaluate a Clojure source file by FILE-PATH.

Ensures the file exists, is in the current project, loads it into buffer,
shows buffer if not visible, asks for user permission, and evaluates it."
  (gptelt-clojure--ensure-workspace 'cljs)
  (gptel-clojure--ensure-current-project-file file-path)
  (let* ((abs-path (gptel-clojure--resolve-file-path file-path))
         (buffer (find-file-noselect abs-path)))
    (gptelt-cljs-eval-buffer buffer)))

;;; get symbol documentation
(defun gptelt-cljs-get-symbol-doc (symbol build-id runtime-id &optional namespace)
  "Get documentation for a ClojureScript symbol in given build and runtime.
Returns the documentation string if available, or an error if not found."
  (gptelt-cljs-ensure-helper-loaded)
  (-> (gptelt-eval--clj-string
       (format "(get-symbol-doc %s %d \"%s\" \"%s\")"
               build-id runtime-id symbol
               (or namespace "cljs.user"))
       "cljs-helper" t)
      (read)))

(comment
  (gptelt-cljs-get-symbol-doc "defn" ":ground" -1))

;;; get symbol source code
(defun gptelt-cljs-get-symbol-source-code (symbol build-id runtime-id &optional namespace)
  "Get source code for a ClojureScript symbol in given build and runtime.
Returns the source code string if available, or an error if not found."
  (gptelt-cljs-ensure-helper-loaded)
  (-> (gptelt-eval--clj-string
       (format "(get-symbol-source-code %s %d \"%s\" \"%s\")"
               build-id runtime-id symbol (or namespace "cljs.user"))
       "cljs-helper" t)
      (read)))

(comment
  (gptelt-cljs-get-symbol-source-code "defn" ":ground" -1)
  (gptelt-cljs-get-symbol-source-code "defff" ":ground" -1))

;;; gptel tool registration
(when (fboundp 'gptelt-make-tool)
  (gptelt-make-tool
   :name "cljs_list_ns"
   :function #'gptelt-cljs-list-ns
   :description "List all loaded namespaces in cljs nREPL connection. Supports optional regex filtering."
   :args '((:name "build_id"
            ;; TODO: check if this symbol works
            :type string
            :description
            "The build ID to evaluate in, must be a string starts with `:`")
           (:name "runtime_id"
            :type integer
            :description "The runtime ID")
           (:name "filter_regex"
            :type number
            :optional t
            :description "Optional regex pattern to filter namespace names"))
   :category "clojurescript"
   :confirm nil
   :include t)
  (gptelt-make-tool
   :name "cljs_get_project_states"
   :function #'gptelt-cljs-get-project-states
   :description "Get current shadow-cljs project states including active builds, runtimes, and build information, you MUST call this before calling other cljs tools."
   :args '()
   :category "clojurescript"
   :confirm nil
   :include t)
  (gptelt-make-tool
   :name "cljs_eval_string"
   :function #'gptel-cljs-eval-string
   :description "Evaluate given cljs code string in given build and runtime, get eval result, error, stdout, stderr."
   :args '((:name "build_id"
            :type string
            :description
            "The build ID to evaluate in, must be a string starts with `:`")
           (:name "runtime_id"
            :type integer
            :description "The runtime ID to evaluate in")
           (:name "cljs_code"
            :type string
            :description "The ClojureScript code string to evaluate")
           (:name "ns"
            :type string
            :optional t
            :description "The namespace to evaluate in (optional, defaults to 'cljs.user')"))
   :category "clojurescript"
   :confirm nil
   :include t)

  (gptelt-make-tool
   :name "cljs_get_symbol_doc"
   :function #'gptelt-cljs-get-symbol-doc
   :description "Get documentation for a ClojureScript symbol (function, macro, var, etc.). Returns formatted documentation including arglists, type, and docstring."
   :args '((:name "symbol"
            :type string
            :description "The ClojureScript symbol to get documentation for (e.g., 'map', 'reduce', 'defn')")
           (:name "build_id"
            :type string
            :description "The build ID to evaluate in, must be a string starts with `:`")
           (:name "runtime_id"
            :type integer
            :description "The runtime ID to evaluate in")
           (:name "namespace"
            :type string
            :optional t
            :description "Optional namespace to qualify the symbol (e.g., 'cljs.core', defaults to 'cljs.user')"))
   :category "clojurescript"
   :confirm nil
   :include t)

  (gptelt-make-tool
   :name "cljs_get_symbol_source_code"
   :function #'gptelt-cljs-get-symbol-source-code
   :description "Get source code for a ClojureScript symbol (function, macro, var, etc.). Returns the complete source code definition."
   :args '((:name "symbol"
            :type string
            :description "The ClojureScript symbol to get source code for (e.g., 'map', 'reduce', 'when')")
           (:name "build_id"
            :type string
            :description "The build ID to evaluate in, must be a string starts with `:`")
           (:name "runtime_id"
            :type integer
            :description "The runtime ID to evaluate in")
           (:name "namespace"
            :type string
            :optional t
            :description "Optional namespace to qualify the symbol (e.g., 'cljs.core', defaults to 'cljs.user')"))
   :category "clojurescript"
   :confirm nil
   :include t))
