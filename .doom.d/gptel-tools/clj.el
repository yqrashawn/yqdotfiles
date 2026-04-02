;;; .nixpkgs/.doom.d/gptel-tools/clj.el -*- lexical-binding: t; -*-

(require 'cider)

(defvar gptelt-clj-sync-request-timeout 300
  "Timeout in seconds for sync nREPL requests from MCP tools.
This overrides `nrepl-sync-request-timeout' when MCP tool functions
make synchronous nREPL calls.")

(defvar gptelt-clj-emit-to-repl nil
  "When non-nil, echo eval request and response to the CIDER REPL buffer.
Set to nil to suppress REPL output when LLM tools evaluate clj/cljs code.")

;;; utilities

(defun gptelt-clj--get-clj-repl ()
  "Get the CLJ REPL for the current workspace, safely.
Resolves from a known project buffer to ensure CIDER's sesman picks the correct
session, regardless of what the current buffer is (could be CLJS, chat, scratch, etc.)."
  (let* ((proj-root (++workspace-current-project-root))
         (proj-buf (or (++workspace-get-random-clj-buffer)
                       (let ((deps (expand-file-name "deps.edn" proj-root)))
                         (when (file-exists-p deps)
                           (find-file-noselect deps))))))
    (if proj-buf
        (with-current-buffer proj-buf
          (or (cider-current-repl 'clj)
              (error "Clojure nREPL is not connected. Use M-x cider-jack-in to start a REPL.")))
      (or (cider-current-repl 'clj)
          (error "Clojure nREPL is not connected. Use M-x cider-jack-in to start a REPL.")))))

(defun gptelt-clj-ensure-helper-loaded ()
  "Load clj_helper.clj into the CLJ REPL via load-file.
Uses explicit CLJ REPL connection to avoid CIDER sesman session mismatch
when the helper file is outside the current project."
  (let* ((nrepl-sync-request-timeout gptelt-clj-sync-request-timeout)
         (clj-repl (gptelt-clj--get-clj-repl))
         (helper-path (expand-file-name "~/.nixpkgs/env/dev/clj_helper.clj"))
         (result (cider-nrepl-sync-request:eval
                  (format "(load-file \"%s\")" helper-path)
                  clj-repl "user")))
    (when-let ((err (nrepl-dict-get result "err")))
      (error "Failed to load clj-helper: %s" err))))

(defun gptelt-clojure--ensure-workspace (type &optional ns)
  (when (eq type 'cljs)
    (unless (++workspace-cljs?)
      (error "This is not a clojure project"))
    (unless (++workspace-cljs-repl-connected?)
      (error "ClojureScript nREPL is not connected. Use cljs_get_project_states to check available builds and runtimes, or M-x cider-jack-in-cljs to connect.")))

  (when (or (eq type 'clj)
            (eq type 'cljs))
    (unless (++workspace-clj?)
      (error "This is not a clojure project"))
    (unless (++workspace-clj-repl-connected?)
      (error "Clojure nREPL is not connected. Use M-x cider-jack-in to start a REPL."))
    (when ns
      ;; Use find-ns via direct eval on the CLJ REPL to check namespace.
      ;; cider-sync-request:ns-list is unreliable for dynamically loaded
      ;; namespaces (e.g. helper files outside the project).
      (let* ((nrepl-sync-request-timeout gptelt-clj-sync-request-timeout)
             (clj-repl (gptelt-clj--get-clj-repl))
             (result (cider-nrepl-sync-request:eval
                      (format "(some? (find-ns '%s))" ns)
                      clj-repl "user"))
             (value (nrepl-dict-get result "value")))
        (unless (and value (string= value "true"))
          (let ((ns-sample
                 (condition-case nil
                     (nrepl-dict-get
                      (cider-nrepl-sync-request:eval
                       "(vec (take 20 (sort (map str (all-ns)))))"
                       clj-repl "user")
                      "value")
                   (error nil))))
            (error "Namespace '%s' is not loaded.\n\nSample of loaded namespaces (use clj_list_ns for full list):\n%s"
                   ns (or ns-sample "(could not list namespaces)"))))))))

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
  (let* ((nrepl-sync-request-timeout gptelt-clj-sync-request-timeout)
         (ns-list (with-current-buffer (gptelt-clj--get-clj-repl)
                    (cider-sync-request:ns-list))))
    (if filter-regex
        (seq-filter
         (lambda (ns)
           (string-match-p
            (rxt-pcre-to-elisp filter-regex) ns))
         ns-list)
      ns-list)))

(comment
  (gptelt-clj-list-ns)
  (gptelt-clj-list-ns "nrepl")
  (gptelt-clj-list-ns "clojure.core")
  (gptelt-clj-list-ns "cljs.core"))

;;; list all classpath entries
(defun gptelt-clj-list-classpath (&optional filter-regex)
  "List all classpath entries in clj nREPL connection.
Optional FILTER-REGEX filters the classpath list before returning."
  (gptelt-clojure--ensure-workspace 'clj)
  (let* ((nrepl-sync-request-timeout gptelt-clj-sync-request-timeout)
         (classpath-list (with-current-buffer (gptelt-clj--get-clj-repl)
                           (cider-classpath-entries))))
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
(defun gptelt-eval--clj-string (clj-string &optional namespace no-confirm)
  "Evaluate CLJ-STRING in NAMESPACE (defaults to 'user') and return result or error.
Checks if clj nREPL is connected and namespace is available before evaluation.
Explicitly uses the CLJ REPL connection so this works even when called from a CLJS buffer."
  (gptelt-clojure--ensure-workspace 'clj (or namespace "user"))
  (gptelt-clj-ensure-helper-loaded)

  (let* ((nrepl-sync-request-timeout gptelt-clj-sync-request-timeout)
         (clj-repl (gptelt-clj--get-clj-repl))
         (ns (or namespace "user"))
         (no-confirm t)
         allow?)

    (with-temp-buffer
      (insert clj-string)
      (let ((win (unless no-confirm (display-buffer (current-buffer)))))
        (clojure-mode)
        (setq allow?
              (if no-confirm
                  t
                (y-or-n-p (format "Evaluate clj code in ns %s? " ns))))
        (unless no-confirm (quit-restore-window win 'bury))))

    (if allow?
        ;; Evaluate the code using explicit CLJ connection
        (condition-case err
            (let* ((result
                    (cider-nrepl-sync-request:eval clj-string clj-repl ns))
                   (out (nrepl-dict-get result "out"))
                   (value (nrepl-dict-get result "value"))
                   (err-out (nrepl-dict-get result "err")))
              (when (and gptelt-clj-emit-to-repl (buffer-live-p clj-repl))
                (with-current-buffer clj-repl
                  (cider-repl-emit-stdout clj-repl (format "%s\n" clj-string))
                  (when out
                    (cider-repl-emit-stdout clj-repl out))
                  (when value
                    (cider-repl-emit-result clj-repl value t t))
                  (when err-out
                    (cider-repl-emit-stderr clj-repl err-out))))
              (or value err-out
                  (error "Unexpected evaluation result")))
          (error (format "Error evaluating clj code: %s" (error-message-string err))))
      "User rejected the eval request")))

;;; async eval clj string (for MCP tool use — non-blocking)
(defun gptelt-eval--clj-string-async (callback clj-string &optional namespace)
  "Evaluate CLJ-STRING asynchronously via nREPL, call CALLBACK with result.
CALLBACK receives a single string: the eval value, error output, or error
message.  NAMESPACE defaults to \"user\".  Uses `nrepl-request:eval' so
the main thread is never blocked by polling.

Includes a timeout guard: if nREPL never sends a \"done\" status within
`gptelt-clj-sync-request-timeout' seconds, CALLBACK is called with a
timeout error.  The guard ensures CALLBACK is called exactly once."
  (condition-case err
      (progn
        (gptelt-clojure--ensure-workspace 'clj (or namespace "user"))
        (gptelt-clj-ensure-helper-loaded)

        (let* ((clj-repl (gptelt-clj--get-clj-repl))
               (ns (or namespace "user"))
               ;; Mutable accumulator — nREPL sends multiple messages per eval
               (response (cons 'dict nil))
               ;; Ensure callback is called exactly once (timeout vs normal)
               (called-p (cons nil nil))
               (safe-callback
                (lambda (result)
                  (unless (car called-p)
                    (setcar called-p t)
                    (funcall callback result))))
               (timer (run-with-timer
                       gptelt-clj-sync-request-timeout nil
                       (lambda ()
                         (funcall safe-callback
                                  (format "Error: nREPL evaluation timed out after %ds"
                                          gptelt-clj-sync-request-timeout))))))
          (nrepl-request:eval
           clj-string
           (lambda (resp)
             (condition-case err
                 (progn
                   (nrepl--merge response resp)
                   (when (member "done" (nrepl-dict-get response "status"))
                     (cancel-timer timer)
                     (let ((out (nrepl-dict-get response "out"))
                           (value (nrepl-dict-get response "value"))
                           (err-out (nrepl-dict-get response "err"))
                           (id (nrepl-dict-get response "id")))
                       ;; Echo to REPL buffer
                       (when (and gptelt-clj-emit-to-repl (buffer-live-p clj-repl))
                         (with-current-buffer clj-repl
                           (cider-repl-emit-stdout clj-repl (format "%s\n" clj-string))
                           (when out
                             (cider-repl-emit-stdout clj-repl out))
                           (when value
                             (cider-repl-emit-result clj-repl value t t))
                           (when err-out
                             (cider-repl-emit-stderr clj-repl err-out))))
                       ;; Mark request as completed in nREPL
                       (when id
                         (with-current-buffer clj-repl
                           (nrepl--mark-id-completed id)))
                       ;; Deliver result to MCP callback
                       (funcall safe-callback
                                (or value err-out
                                    "Evaluation completed with no output")))))
               (error
                (cancel-timer timer)
                (funcall safe-callback
                         (format "Error processing nREPL response: %s"
                                 (error-message-string err))))))
           clj-repl
           ns)))
    (error
     (funcall callback
              (format "Error: %s" (error-message-string err))))))

(defun gptelt-eval-clj-string-async (callback clj-string &optional namespace)
  "Async MCP tool wrapper for `gptelt-eval--clj-string-async'.
CALLBACK is the MCP async callback, CLJ-STRING and NAMESPACE are tool args."
  (gptelt-eval--clj-string-async callback clj-string namespace))

(comment
  (gptelt-eval-clj-string-async
   (lambda (r) (message "Async result: %s" r))
   "(+ 1 2)" "user"))

;;; get buffer namespace
(defun gptelt-clj-get-buffer-ns (buffer-name)
  "Get the namespace of the given BUFFER-NAME containing Clojure code.
Returns the namespace string if found, or nil if no namespace is detected."
  (gptelt-clojure--ensure-workspace 'clj)
  (let ((buffer (get-buffer buffer-name)))
    (unless buffer
      (let ((clj-bufs (seq-filter
                       (lambda (b)
                         (with-current-buffer b
                           (memq major-mode '(clojure-mode clojurescript-mode clojurec-mode))))
                       (buffer-list))))
        (error "Buffer '%s' not found.\n\nAvailable Clojure buffers:\n%s"
               buffer-name
               (if clj-bufs (mapconcat #'buffer-name clj-bufs "\n") "No Clojure buffers open"))))
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
                (substring-no-properties ns-name 0)
              (error "Could not determine namespace from buffer '%s'" buffer-name))))))))

(comment
  (gptelt-clj-get-buffer-ns
   (find-file-noselect "../../src/ground/core.cljs"))
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
                  (substring-no-properties ns-name 0)
                (error "Could not determine namespace from file '%s'" abs-path)))))))))

(comment
  (gptelt-clj-get-file-ns "src/user.clj")
  (gptelt-clj-get-file-ns
   (expand-file-name "../../src/ground/core.cljs")))

;;; get namespace file info
(defun gptelt-clj-get-ns-file-url (namespace)
  "Get file information for NAMESPACE.
Returns a list with: (file-path file-exists-p total-lines).
Ensures clj workspace and nREPL connection before proceeding."
  (gptelt-clojure--ensure-workspace 'clj namespace)
  (condition-case err
      (let* ((nrepl-sync-request-timeout gptelt-clj-sync-request-timeout)
             (file-path (with-current-buffer (gptelt-clj--get-clj-repl)
                          (cider-sync-request:ns-path namespace t))))
        (if (string= file-path "")
            (error "File for ns %s does not exist, this usually means this namespace is not loaded" namespace)
          file-path))
    (error (error "Error getting namespace file info: %s" (error-message-string err)))))

(comment
  (gptelt-clj-get-ns-file-url "ground.core")
  (gptelt-clj-get-ns-file-url "user")
  (gptelt-clj-get-ns-file-url "users"))

;;; get symbol documentation
(defun gptelt-clj-get-doc-for-symbol (symbol &optional namespace)
  "Get documentation for SYMBOL in optional NAMESPACE.
Returns the documentation string if available, or an error if not found.
Ensures clj workspace and nREPL connection before proceeding."
  (gptelt-clojure--ensure-workspace 'clj)
  (condition-case err
      (let* ((nrepl-sync-request-timeout gptelt-clj-sync-request-timeout)
             (qualified-symbol (if namespace
                                   (format "%s/%s" namespace symbol)
                                 symbol))
             (info (with-current-buffer (gptelt-clj--get-clj-repl)
                     (cider-var-info qualified-symbol))))
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
  (gptelt-clj-get-doc-for-symbol "defn")
  (gptelt-clj-get-doc-for-symbol "log!" "taoensso.telemere"))

;;; get symbol source code
(defun gptelt-clj-get-symbol-source-code (symbol &optional namespace)
  "Get source code for SYMBOL in optional NAMESPACE.
Returns the source code string if available, or an error if not found.
Ensures clj workspace and nREPL connection before proceeding."
  (gptelt-clojure--ensure-workspace 'clj)
  (condition-case err
      (let* ((nrepl-sync-request-timeout gptelt-clj-sync-request-timeout)
             (qualified-symbol (if namespace
                                   (format "%s/%s" namespace symbol)
                                 symbol))
             (source-code (cider-nrepl-sync-request:eval
                           (format "(with-out-str (clojure.repl/source %s))" qualified-symbol)
                           (gptelt-clj--get-clj-repl) "user")))
        (if source-code
            (let ((code (nrepl-dict-get source-code "value")))
              (if (and code (not (string= code "nil")))
                  (string-trim (substring code 1 -1))
                (error "Source code not found for symbol '%s'" qualified-symbol)))
          (error "Symbol '%s' not found or not resolved" qualified-symbol)))
    (error (error "Error getting symbol source code: %s" (error-message-string err)))))

(defun gptelt-clj-get-symbol-source-code-async (callback symbol &optional namespace)
  "Async version of `gptelt-clj-get-symbol-source-code' for MCP tool use.
CALLBACK receives the source code string or an error message."
  (condition-case err
      (let ((qualified-symbol (if namespace
                                  (format "%s/%s" namespace symbol)
                                symbol)))
        (gptelt-eval--clj-string-async
         (lambda (result)
           (if (and result (not (string= result "nil")))
               (funcall callback (string-trim (substring result 1 -1)))
             (funcall callback
                      (format "Source code not found for symbol '%s'" qualified-symbol))))
         (format "(with-out-str (clojure.repl/source %s))" qualified-symbol)
         "user"))
    (error (funcall callback
                    (format "Error getting symbol source code: %s"
                            (error-message-string err))))))

(comment
  (gptelt-clj-get-symbol-source-code "when")
  (gptelt-clj-get-symbol-source-code "log!" "taoensso.telemere"))

;;; evaluate buffer
(defun gptelt-cider--eval-buffer
    (type buffer-name &optional no-confirm no-check-project-file)
  "Evaluate a Clojure buffer by BUFFER-NAME.

Ensures the buffer exists, its file is in the current project, and evaluates it."
  (gptelt-clojure--ensure-workspace type)
  (let* ((buffer (get-buffer buffer-name))
         (shown-before (if no-confirm t (get-buffer-window buffer)))
         win)
    (unless buffer
      (let ((clj-bufs (seq-filter
                       (lambda (b)
                         (with-current-buffer b
                           (memq major-mode '(clojure-mode clojurescript-mode clojurec-mode))))
                       (buffer-list))))
        (error "Buffer '%s' not found.\n\nAvailable Clojure buffers:\n%s"
               buffer-name
               (if clj-bufs (mapconcat #'buffer-name clj-bufs "\n") "No Clojure buffers open"))))
    (let ((file-path (buffer-file-name buffer)))
      (unless file-path
        (error "Buffer '%s' is not associated with a file" buffer-name))
      (unless no-check-project-file
        (gptel-clojure--ensure-current-project-file file-path))
      (with-current-buffer buffer
        (unless (if (eq type 'cljs)
                    (or (eq major-mode 'clojurescript-mode)
                        (eq major-mode 'clojurec-mode))
                  (or (eq major-mode 'clojure-mode)
                      (eq major-mode 'clojurec-mode)))
          (error "Buffer '%s' is not a Clojure buffer (mode: %s)" buffer-name major-mode))

        (setq win (unless shown-before (display-buffer buffer)))

        (when (or
               shown-before
               (y-or-n-p
                (if (eq type 'cljs)
                    (format "Evaluate CLJS file %s? " file-path)
                  (format "Evaluate CLJ file %s? " file-path))))
          (unwind-protect
              (condition-case err
                  (progn
                    (cider-load-buffer buffer)
                    (when gptelt-clj-emit-to-repl
                      (when-let ((repl-buf (cider-current-repl type)))
                        (with-current-buffer repl-buf
                          (cider-repl-emit-stdout
                           repl-buf (format ";; eval %s\n" buffer)))))
                    (format "Successfully evaluated buffer '%s'" buffer-name))
                (error (format "Error evaluating buffer '%s': %s" buffer-name (error-message-string err))))
            (when (and win (window-live-p win))
              (quit-restore-window win 'bury))))))))

(defun gptelt-clj-eval-buffer (buffer-name)
  "Evaluate a Clojure buffer by BUFFER-NAME.

Ensures the buffer exists, its file is in the current project, and evaluates it."
  (gptelt-cider--eval-buffer 'clj buffer-name t))

(comment
  (gptelt-clj-eval-buffer "shadow-cljs-helpers.get-running-builds <.nixpkgs>")
  (gptelt-clj-eval-buffer
   (find-file-noselect "../../src/user.clj"))
  (gptelt-clj-eval-buffer
   (find-file-noselect "../../env/dev/cljs_helper.clj")))

;;; evaluate file
(defun gptelt-clj-eval-file (file-path)
  "Evaluate a Clojure source file by FILE-PATH.
Ensures the file exists, is in the current project, loads it into buffer,
shows buffer if not visible, asks for user permission, and evaluates it."
  (gptelt-clojure--ensure-workspace 'clj)
  (gptel-clojure--ensure-current-project-file file-path)
  (gptelt-clj-ensure-helper-loaded)
  (let* ((abs-path (gptel-clojure--resolve-file-path file-path))
         (buffer (find-file-noselect abs-path)))
    (gptelt-clj-eval-buffer buffer)))

(comment
  (gptelt-clj-eval-file "src/user.clj"))

;;; read file url
(defun gptelt-clj-read-file-url
    (file-url &optional limit offset)
  (gptelt-clojure--ensure-workspace 'clj)
  (gptelt-clj-ensure-helper-loaded)
  (let ((rst (read (gptelt-eval--clj-string
                    (if (or limit offset)
                        (format "(read-file-url \"%s\" {:limit %s :offset %s})"
                                file-url
                                (or limit 2000)
                                (or offset 0))
                      (format "(read-file-url \"%s\")" file-url))
                    "clj-helper"
                    t))))
    (if (string-prefix-p "Failed to read file url: " rst)
        (error rst)
      rst)))

(defun gptelt-clj-read-file-url-async (callback file-url &optional limit offset)
  "Async version of `gptelt-clj-read-file-url' for MCP tool use.
CALLBACK receives the file content string or an error message."
  (condition-case err
      (progn
        (gptelt-clojure--ensure-workspace 'clj)
        (gptelt-clj-ensure-helper-loaded)
        (gptelt-eval--clj-string-async
         (lambda (result)
           (condition-case err
               (let ((rst (read result)))
                 (if (string-prefix-p "Failed to read file url: " rst)
                     (funcall callback (format "Error: %s" rst))
                   (funcall callback rst)))
             (error (funcall callback
                             (format "Error reading file: %s" (error-message-string err))))))
         (if (or limit offset)
             (format "(read-file-url \"%s\" {:limit %s :offset %s})"
                     file-url
                     (or limit 2000)
                     (or offset 0))
           (format "(read-file-url \"%s\")" file-url))
         "clj-helper"))
    (error
     (funcall callback
              (format "Error: %s" (error-message-string err))))))

(comment
  (gptelt-clj-read-file-url
   (gptelt-clj-get-ns-file-url "clojure.core")
   100 100)
  (gptelt-clj-read-file-url (gptelt-clj-get-ns-file-url "shadow.json"))
  (gptelt-clj-read-file-url "a"))

;;; run namespace tests
(defun gptelt-clj-run-ns-test-async (callback namespaces)
  "Run tests for NAMESPACES (space-separated string) asynchronously.
Uses kaocha.repl/run if available, falls back to clojure.test/run-tests.
CALLBACK receives the test output string."
  (condition-case err
      (progn
        (gptelt-clojure--ensure-workspace 'clj)
        (let* ((ns-list (split-string (string-trim namespaces)))
               (require-forms (mapconcat (lambda (ns) (format "(require '%s)" ns)) ns-list "\n  "))
               (quoted-nss (mapconcat (lambda (ns) (format "'%s" ns)) ns-list " "))
               (clj-code (format
                          "(do\n  %s\n  (let [has-kaocha? (try (require 'kaocha.repl) true (catch Exception _ false))]\n    (with-out-str\n      (binding [clojure.test/*test-out* *out*]\n        (if has-kaocha?\n          (kaocha.repl/run %s {:color? false})\n          (clojure.test/run-tests %s))))))"
                          require-forms
                          quoted-nss
                          quoted-nss)))
          (gptelt-eval--clj-string-async
           (lambda (result)
             (condition-case err
                 (if (and result (not (string= result "nil")))
                     (funcall callback (read result))
                   (funcall callback "No test output"))
               (error (funcall callback
                               (format "Error parsing test output: %s\nRaw: %s"
                                       (error-message-string err) result)))))
           clj-code "user")))
    (error
     (funcall callback
              (format "Error: %s" (error-message-string err))))))

(comment
  (gptelt-clj-run-ns-test-async
   (lambda (r) (message "Test result:\n%s" r))
   "cchp.web.routes.llm-logs-test")
  (gptelt-clj-run-ns-test-async
   (lambda (r) (message "Test result:\n%s" r))
   "cchp.web.routes.llm-logs-test cchp.web.routes.utils-test"))

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
   :function #'gptelt-eval-clj-string-async
   :async t
   :description "Evaluate given clojure code string in given namespace, get eval result or error. Use `clojure_run_ns_test` tool to run tests for better output. or wrap test with this to get better failure/error output (with-out-str (binding [clojure.test/*test-out* *out*] (clojure.test/run-tests ...)))"
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
   :description "Get file url for a given clj namespace"
   :args '((:name "namespace"
            :type string
            :description "The clj namespace to get file information for"))
   :category "clojure"
   :confirm nil
   :include t)

  (gptelt-make-tool
   :name "clj_read_file_url"
   :function #'gptelt-clj-read-file-url-async
   :async t
   :description
   "Read the content of file url returned by cljs_get_ns_file_url, the content will be wrapped with ␂ at the start and ␃ at the end."
   :args '((:name "file_url"
            :type string
            :description "file_url return by clj_get_ns_file_url")
           (:name "limit"
            :type integer
            :optional t
            :description "The number of lines to read. Default to 2000, Only provide if the file is too large to read at once.")
           (:name "offset"
            :type integer
            :optional t
            :description "The line number to start reading from. Only provide if the file is too large to read at once"))
   :category "clojure"
   :confirm nil
   :include t)

  (gptelt-make-tool
   :name "clj_get_symbol_doc"
   :function #'gptelt-clj-get-doc-for-symbol
   :description "Use this whenever you want to check the documentation of a Clojure symbol (function, macro, var, namespace, etc.). Returns formatted documentation including arglists, type, and docstring."
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
   :name "clj_get_symbol_source_code"
   :function #'gptelt-clj-get-symbol-source-code-async
   :async t
   :description "Use this whenever you want to check the source code of a Clojure symbol (function, macro, var, etc.). Returns the complete source code definition."
   :args '((:name "symbol"
            :type string
            :description "The Clojure symbol to get source code for (e.g., 'map', 'reduce', 'when')")
           (:name "namespace"
            :type string
            :optional t
            :description "Optional namespace to qualify the symbol (e.g., 'clojure.core')"))
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
   :include t)

  (gptelt-make-tool
   :name "clojure_run_ns_test"
   :function #'gptelt-clj-run-ns-test-async
   :async t
   :description "Run tests for one or more Clojure test namespaces using clojure.test/run-tests."
   :args '((:name "namespaces"
            :type string
            :description "Space-separated list of test namespace names to run (e.g., 'cchp.web.routes.llm-logs-test cchp.web.routes.utils-test')"))
   :category "clojure"
   :confirm nil
   :include t))

;;; clj.el ends here
