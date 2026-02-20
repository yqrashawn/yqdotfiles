;;; .nixpkgs/.doom.d/gptel-tools/cljs.el -*- lexical-binding: t; -*-

(defun gptelt-cljs--get-cljs-repl ()
  "Get the CLJS REPL for the current workspace, safely.
Resolves from a known CLJS project buffer to ensure correct sesman session."
  (let ((cljs-buf (++workspace-get-random-cljs-buffer)))
    (if cljs-buf
        (with-current-buffer cljs-buf
          (or (cider-current-repl 'cljs)
              (error "ClojureScript nREPL is not connected")))
      (error "No ClojureScript buffer found in workspace"))))

(defun gptelt-clj-eval-cljs-in-clj-repl (clj-str)
  "Evaluate CLJ-STR in the CLJ side of the CLJS REPL connection.
WARNING: Not concurrency-safe — temporarily exits CLJS mode via :cljs/quit,
evaluates CLJ code, then re-enters shadow-cljs REPL. Overlapping calls will
corrupt the REPL state."
  (let ((repl (gptelt-cljs--get-cljs-repl)))
    (cider-nrepl-sync-request:eval ":cljs/quit" repl nil)
    (let ((rst (cider-nrepl-sync-request:eval clj-str repl nil)))
      (cider-nrepl-sync-request:eval "(shadow.cljs.devtools.api/repl :app)" repl nil)
      (nrepl-dict-get rst "status"))))

(comment
  (cider-nrepl-sync-request:eval "(js/console.log 'aaa)" jjj nil)
  (cider-nrepl-sync-request:eval (format "(shadow.cljs.devtools.api/repl :app)") jjj nil)
  (gptelt-clj-eval-cljs-in-clj-repl "System")
  (gptelt-clj-eval-cljs-in-clj-repl "(System/getenv \"DOMAIN\"")
  (gptelt-clj-eval-cljs-in-clj-repl "System")
  (gptelt-clj-eval-cljs-in-clj-repl "js/location.href")
  (gptelt-clj-eval-cljs-in-clj-repl "(prn 1)")
  (gptelt-clj-eval-cljs-in-clj-repl
   (with-current-buffer (find-file-noselect"~/.nixpkgs/env/dev/cljs_helper.clj")
     (buffer-substring (point-min) (point-max)))))

(defun gptelt-cljs-ensure-helper-loaded ()
  "Load cljs_helper.clj into the CLJ REPL via load-file.
Uses gptelt-clj--get-clj-repl for workspace-safe REPL resolution."
  (let* ((clj-repl (gptelt-clj--get-clj-repl))
         (helper-path (expand-file-name "~/.nixpkgs/env/dev/cljs_helper.clj"))
         (result (cider-nrepl-sync-request:eval
                  (format "(load-file \"%s\")" helper-path)
                  clj-repl "user")))
    (when-let ((err (nrepl-dict-get result "err")))
      (error "Failed to load cljs-helper: %s" err))))

(comment
  (gptelt-cljs-ensure-helper-loaded))

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
  (gptelt-cljs-ensure-helper-loaded)
  (gptelt-eval--clj-string
   (format "(cljs-eval %s %d %s %s)" build-id runtime-id (prin1-to-string cljs-code) ns)
   "cljs-helper" t))

(comment
  (gptel-cljs-eval-string :app 7 "js/location.href")
  (gptel-cljs-eval-string :ground 10 "js/document.body")
  (gptel-cljs-eval-string :ground 10 "js/location.hreff.f"))

;;; eval buffer
(defun gptelt-cljs-eval-buffer (buffer-name)
  "Evaluate a Clojurescript buffer by BUFFER-NAME.

Ensures the buffer exists, its file is in the current project, and evaluates it."
  (gptelt-cider--eval-buffer 'cljs buffer-name))

(comment
  (gptelt-cljs-eval-buffer "core.cljs"))

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

(comment
  (gptelt-cljs-eval-file "src/app/core.cljs"))

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

;;; get buffer namespace
(defun gptelt-cljs-get-buffer-ns (buffer-name)
  (gptelt-clojure--ensure-workspace 'cljs)
  (gptelt-clj-get-buffer-ns buffer-name))

(comment
  (gptelt-cljs-get-buffer-ns
   (find-file-noselect "../../src/ground/core.cljs"))
  (gptelt-cljs-get-buffer-ns "foo"))

;;; get file namespace
(defun gptelt-cljs-get-file-ns (file-path)
  (gptelt-clojure--ensure-workspace 'cljs)
  (gptelt-clj-get-file-ns file-path))

(comment
  (gptelt-cljs-get-file-ns "src/ground/core.cljs")
  (gptelt-cljs-get-file-ns "src/ground/core.cljss"))

;;; get namespace file url
(defun gptelt-cljs-get-ns-file-url (namespace)
  (gptelt-clojure--ensure-workspace 'cljs namespace)
  (gptelt-clj-get-ns-file-url namespace))

(comment
  (gptelt-cljs-get-ns-file-url "ground.core")
  (gptelt-cljs-get-ns-file-url "ground.coree"))

;;; read file url
(defun gptelt-cljs-read-file-url (file-url &optional limit offset)
  (gptelt-clojure--ensure-workspace 'cljs)
  (gptelt-clj-read-file-url file-url limit offset))

(comment
  (gptelt-cljs-read-file-url (gptelt-cljs-get-ns-file-url"shadow.json"))
  (gptelt-cljs-read-file-url "foo"))

;;; get shadow-cljs build status
(defun gptelt-cljs-get-build-status (&optional build-id)
  "Get the last build status, build logs, and error logs from shadow-cljs.
If BUILD-ID is provided, returns status for that build only.
Otherwise returns status for all active builds."
  (gptelt-cljs-ensure-helper-loaded)
  (gptelt-eval--clj-string
   (if build-id
       (format "(get-build-status %s)" build-id)
     "(get-build-status)")
   "cljs-helper" t))

(comment
  (gptelt-cljs-get-build-status)
  (gptelt-cljs-get-build-status ":app"))

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
   :name "get_shadow-cljs_build_status"
   :function #'gptelt-cljs-get-build-status
   :description "Get the last build status, build logs, and error logs of shadow-cljs. Returns compilation status (:completed/:failed/:compiling), duration, compiled file count, warnings (with file/line/message), and error reports. Useful for checking if a build succeeded or diagnosing compilation errors."
   :args '((:name "build_id"
            :type string
            :optional t
            :description "Optional build ID (e.g. ':app'). If omitted, returns status for all active builds."))
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
   :include t)

  (gptelt-make-tool
   :name "cljs_get_buffer_ns"
   :function #'gptelt-cljs-get-buffer-ns
   :description "Get the namespace of a cljs buffer by buffer name."
   :args '((:name "buffer_name"
            :type string
            :description "The name of the cljs buffer to get the namespace from"))
   :category "clojure"
   :confirm nil
   :include t)

  (gptelt-make-tool
   :name "cljs_get_file_ns"
   :function #'gptelt-cljs-get-file-ns
   :description "Get the namespace of a cljs source file in the current project, given the file path. File must be under project root and be a .clj, .cljc"
   :args '((:name "file_path"
            :type string
            :description
            "Path to the cljs file in the current project root (relative or absolute)"))
   :category "clojure"
   :confirm nil
   :include t)

  (gptelt-make-tool
   :name "cljs_get_ns_file_url"
   :function #'gptelt-cljs-get-ns-file-url
   :description "Get file url for a given cljs namespace"
   :args '((:name "namespace"
            :type string
            :description "The cljs namespace to get file information for"))
   :category "clojure"
   :confirm nil
   :include t)

  (gptelt-make-tool
   :name "cljs_read_file_url"
   :function #'gptelt-cljs-read-file-url
   :description
   "Read the content of file url returned by cljs_get_ns_file_url, the content will be wrapped with ␂ at the start and ␃ at the end."
   :args '((:name "file_url"
            :type string
            :description "file_url return by cljs_get_ns_file_url")
           (:name "limit"
            :type integer
            :optional t
            :description "The number of lines to read. Default to 2000. Only provide if the file is too large to read at once.")
           (:name "offset"
            :type integer
            :optional t
            :description "The line number to start reading from. Only provide if the file is too large to read at once"))
   :category "clojure"
   :confirm nil
   :include t))

;;; cljs.el ends here
