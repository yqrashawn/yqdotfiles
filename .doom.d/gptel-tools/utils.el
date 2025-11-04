;;; utils.el --- GPTEL utilities -*- lexical-binding: t; -*-
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://github.com/yqrashawn/gptel-tools
;; Keywords: tools, util

;;; Commentary:
;;
;; Utility functions for GPTEL tools.
;;
;;; Code:
;;; Commentary:
;; Shared utilities for gptel edit/create tools: project/file/mode/balance logic.
;;; Code:

(require 'smartparens nil t)
(require 'lgr)

(setq gpteltl (lgr-get-logger "gptelt"))

(progn
  (lgr-reset-appenders gpteltl)
  (-> gpteltl
      (lgr-add-appender
       (-> (lgr-appender-file
            :file (expand-file-name "~/Dropbox/sync/gptelt.log"))
           (lgr-set-layout
            (lgr-layout-format
             :format "** [%t] :%L: %m %j\n#+end_src"
             :timestamp-format "%Y-%m-%d %a %H:%M %z"))))
      (lgr-set-threshold lgr-level-debug)))

(defun +json-serialize-json-false (orig-fn data &rest args)
  (apply orig-fn data
         :false-object :json-false
         :null-object :null
         args))

(defvar gptelt-log-persist-all-log t)

(defun gptelt-log-mcp-tool (tool-name tool-args tool-result)
  (letf! ((defadvice #'json-serialize :around #'+json-serialize-json-false))
    (condition-case nil
        (let ((is-error? (not (eq (alist-get 'isError tool-result) :json-false))))
          (when (or gptelt-log-persist-all-log is-error?)
            (let ((buffer-file-coding-system 'utf-8-unix)
                  (coding-system-for-read 'utf-8-unix)
                  (coding-system-for-write 'utf-8-unix))
              (lgr-debug
                  gpteltl
                (if is-error?
                    ":%s:error:\n#+begin_src json-ts\n"
                  ":%s:\n#+begin_src json-ts\n")
                tool-name
                :args tool-args
                :rst tool-result))))
      (-const nil))))

(defun gptelt-log-check ()
  (interactive)
  (-> "~/Dropbox/sync/gptelt.log"
      (expand-file-name)
      (find-file-noselect)
      (pop-to-buffer)))

(defun gptelt-log-toggle-all ()
  (interactive)
  (setq gptelt-log-persist-all-log
        (not gptelt-log-persist-all-log))
  (if gptelt-log-persist-all-log
      (message "Now persist all log")
    (message "Now persist error log")))

;;; Project/file path
(defun gptelt--get-project-root ()
  "Get project root for current context."
  (if (fboundp '++workspace-current-project-root)
      (++workspace-current-project-root)
    (when (fboundp 'project-current)
      (when-let ((project (project-current)))
        (if (fboundp 'project-root)
            (project-root project)
          (car (project-roots project)))))))

(defun gptelt--resolve-file-path (file-path)
  "Resolve FILE-PATH to absolute path. Only accepts absolute paths."
  (unless (file-name-absolute-p file-path)
    (error "file_path must be an absolute path"))
  file-path)

;;; Mode/Lisp checks
(defun gptelt--is-lisp-mode-p (mode)
  "Check if MODE is a Lisp-related mode."
  (memq mode '(emacs-lisp-mode lisp-mode clojure-mode scheme-mode
               clojurescript-mode clojurec-mode common-lisp-mode
               lisp-interaction-mode)))

(defun +check-parens ()
  (and (condition-case _
           (unless (check-parens) t)
         (error nil))
       (sp-region-ok-p (point-min) (point-max))))

(defun gptelt--attempt-llm-balance (buffer callback)
  "Attempt to balance BUFFER using LLM asynchronously.
CALLBACK is called with the result plist containing :rst and :err."
  (let ((mj-mode (buffer-local-value 'major-mode buffer))
        (code (with-current-buffer buffer (buffer-string))))
    (llm-balance-lisp-code
     code
     mj-mode
     (lambda (balanced-code)
       (funcall callback (list :rst (plist-get balanced-code :rst) :err nil)))
     (lambda (error-msg)
       (funcall callback (list :rst nil :err error-msg))))))

(defun gptelt--attempt-parinfer-balance (buffer)
  "Use parinfer-rust-mode to attempt to balance BUFFER.

Returns (t . nil) if successful, (nil . error-message) if failed.
Does nothing if parinfer-rust-mode not available."
  (if (not (featurep 'parinfer-rust-mode))
      (cons t nil)           ; No-op, treat as success if parinfer not available
    (with-current-buffer buffer
      (let ((parinfer-rust-preferred-mode "indent")
            (parinfer-rust--in-debug nil)
            (parinfer-rust--disable nil)
            (result nil)
            (error-msg nil))
        (condition-case err
            (progn
              (let ((initial (buffer-substring-no-properties (point-min) (point-max))))
                (flymake-mode 1)        ; required by parinfer
                (unless (bound-and-true-p parinfer-rust-mode)
                  (parinfer-rust-mode 1))
                (setq parinfer-rust--disable nil)
                (setq parinfer-rust--in-debug nil)
                (setq parinfer-rust--mode "indent")
                (parinfer-rust--execute)
                (let ((errval (and (boundp 'parinfer-rust--error) parinfer-rust--error)))
                  (if (not errval)
                      (setq result t)
                    (setq result nil)
                    (setq error-msg
                          (format "Parinfer failed to balance: %s"
                                  (plist-get errval :message))))))
              (cons result error-msg))
          (error (cons nil (error-message-string err))))))))

(defun gptelt--check-buffer-balanced-parens (buffer callback)
  "Check if BUFFER has balanced parentheses for Lisp modes asynchronously.
CALLBACK is called with (BALANCED-P . ERROR-MESSAGE).
Tries LLM auto-repair if unbalanced."
  (let ((mj-mode (buffer-local-value 'major-mode buffer)))
    (if (and (gptelt--is-lisp-mode-p mj-mode)
             (fboundp 'sp-region-ok-p))
        (with-current-buffer buffer
          (condition-case err
              (if (+check-parens)
                  (funcall callback (cons t nil))
                ;; Try LLM auto-repair if unbalanced (async)
                (gptelt--attempt-llm-balance
                 buffer
                 (lambda (llm-check)
                   (let* ((result (if (and (listp llm-check)
                                           (plist-get llm-check :rst))
                                      llm-check
                                    (list :rst llm-check :err nil)))
                          (balanced (plist-get result :rst))
                          (err (plist-get result :err)))
                     (if balanced
                         (with-current-buffer buffer
                           (erase-buffer)
                           (insert balanced)
                           (if (+check-parens)
                               (funcall callback (cons t nil))
                             (funcall callback
                                      (cons nil
                                            (format "The %s buffer would end up in an unbalanced state after replace. CHECK THE PARENTHESES CAREFULLY"
                                                    (symbol-name mj-mode))))))
                       (funcall callback (cons nil err)))))))
            (error (funcall callback (cons nil (error-message-string err))))))
      ;; Non-Lisp modes always pass (sync)
      (funcall callback (cons t nil)))))

(defun gptelt--check-string-balanced-parens (string major-mode-symbol)
  "Check if STRING has balanced parentheses for Lisp MAJOR-MODE-SYMBOL.

Uses temp buffer."
  (if (and (gptelt--is-lisp-mode-p major-mode-symbol)
           (fboundp 'sp-region-ok-p))
      (with-temp-buffer
        (funcall major-mode-symbol)
        (insert string)
        (gptelt--check-buffer-balanced-parens (current-buffer)))
    (cons t nil)))

(defun gptelt--replace-buffer-directly (buffer result-string &optional skip-save)
  "Apply edit to BUFFER by replacing entire buffer with RESULT-STRING.
SKIP-SAVE if non-nil, skips saving the buffer (for multi-edit operations).
Returns a message describing the result of the operation."
  (let ((original-buffer buffer)
        (original-point (with-current-buffer buffer (point))))

    ;; Apply the edit directly in the original buffer
    (with-current-buffer original-buffer
      (save-excursion
        (erase-buffer)
        (insert result-string)
        (when (and (buffer-file-name) (not skip-save))
          (+force-save-buffer))
        (goto-char (min original-point (point-max)))
        (when (and (fboundp 'lsp-format-region)
                   (bound-and-true-p lsp-mode))
          (condition-case err
              (lsp-format-region (point-min) (point-max))
            (error (message "LSP formatting failed: %s" err))))))

    (format "Successfully replaced text in %s." (buffer-name original-buffer))))

(defun gptelt-gptel-tool->mcp-server-lib-tool (tool)
  (let* ((id (gptel-tool-name tool))
         (schema (gptelt-gptel-tool-schema->mcp-server-lib-tool-schema tool)))
    (mcp-server-lib--ref-counted-unregister
     id mcp-server-lib--tools)
    (mcp-server-lib--ref-counted-register
     id
     (list
      :id id
      :description (gptel-tool-description tool)
      :schema schema)
     mcp-server-lib--tools)))

(defun gptelt-get-tool (tool-or-name)
  (if (stringp tool-or-name)
      (gptel-get-tool tool-or-name)
    tool-or-name))

(defun plist-to-alist (plist)
  "Convert a plist structure to an alist structure.
Keywords become string keys, and :json-false becomes \"json-false\"."
  (cond
   ;; Handle lists
   ((listp plist)
    (if (and plist (keywordp (car plist)) (plist-p plist))
        ;; This is a plist, convert it
        (let ((result '())
              (current plist))
          (while current
            (let* ((key (car current))
                   (value (cadr current))
                   (key-str (substring (symbol-name key) 1)) ; Remove the ":"
                   (converted-value (if (string= key-str "properties")
                                        (convert-properties-plist value)
                                      (plist-to-alist value))))
              (push (cons key-str converted-value) result)
              (setq current (cddr current))))
          (nreverse result))
      ;; Regular list, convert each element
      (mapcar #'plist-to-alist plist)))

   ;; Handle the special :json-false keyword
   ((eq plist :json-false) :json-false)

   ;; Handle other keywords (shouldn't happen in this context, but just in case)
   ((keywordp plist)
    (substring (symbol-name plist) 1))

   ;; Handle vectors (convert to lists first)
   ((vectorp plist)
    (mapcar #'plist-to-alist (append plist nil)))

   ;; Everything else (strings, numbers, etc.) pass through unchanged
   (t plist)))

(defun convert-properties-plist (plist)
  "Convert a properties plist to the special list-of-lists format.
Each property becomes a list with the property name as string followed by its alist."
  (let ((result '())
        (current plist))
    (while current
      (let* ((key (car current))
             (value (cadr current))
             (key-str (substring (symbol-name key) 1)) ; Remove the ":"
             (converted-value (plist-to-alist value))
             (property-list (cons key-str converted-value)))
        (push property-list result)
        (setq current (cddr current))))
    (nreverse result)))

(defun plist-p (obj)
  "Check if OBJ is a valid plist (property list).
A plist has an even number of elements and alternates between keywords and values."
  (and (listp obj)
       (= (mod (length obj) 2) 0)
       (let ((is-plist t)
             (current obj))
         (while (and current is-plist)
           (unless (keywordp (car current))
             (setq is-plist nil))
           (setq current (cddr current)))
         is-plist)))

(comment
  (setq original-structure
        '(:type "object"
          :properties (:file_path (:type "string"
                                   :description "Absolute path where the new file should be created (must be absolute, not relative)")
                                  :content_string (:type "string"
                                                   :description "The complete content to write to the new file"))
          ;; :required ["file_path" "content_string"]
          :required []
          :additionalProperties :json-false))
  (gptelt-parse-tool-schema original-structure))

(defun gptel-parse-tool-schema-ensure-required-array (schema)
  (if (and (clj/get schema "properties")
           (null (clj/get schema "required")))
      (clj/assoc schema "required" [])
    schema))

(defun gptelt-parse-tool-schema (tool-or-name)
  (when-let ((one-gptel-tool (gptelt-get-tool tool-or-name)))
    (let* ((tool (plist-get (clj/first
                             (gptel--parse-tools
                              gptel--openai
                              (list one-gptel-tool)))
                            :function))
           (schema (plist-get tool :parameters)))
      (if (eq schema :null)
          '((type . "object"))
        (gptel-parse-tool-schema-ensure-required-array (plist-to-alist schema))))))

(defun gptelt-mcp-register-one-gptel-tool (tool-or-name)
  (when-let ((one-gptel-tool (gptelt-get-tool tool-or-name)))
    (let ((tool (plist-get (clj/first
                            (gptel--parse-tools
                             gptel--openai
                             (list one-gptel-tool)))
                           :function)))
      (let ((id (plist-get tool :name))
            (description (plist-get tool :description))
            (schema (plist-get tool :parameters)))
        (mcp-server-lib--ref-counted-unregister
         id mcp-server-lib--tools)
        (mcp-server-lib--ref-counted-register
         id
         (list
          :id id
          :handler (gptel-tool-function one-gptel-tool)
          :gptel-tool one-gptel-tool
          :description description
          :schema
          (if (eq schema :null)
              '((type . "object"))
            (gptel-parse-tool-schema-ensure-required-array (plist-to-alist schema))))
         mcp-server-lib--tools)))))

(defun gptelt-mcp-register-all-gptel-tools ()
  (let ((tools gptel-tools))
    (seq-doseq (tool tools)
      (gptelt-mcp-register-one-gptel-tool
       (gptel-tool-name tool)))))

(defun gptelt-make-tool (&rest args)
  ;; (when (null (plist-get args :args))
  ;;   (error ":args must be a list"))
  (let* ((is-async (plist-get args :async))
         (original-fn (plist-get args :function))
         (wrapped-fn
          (if is-async
              ;; Wrap async functions to catch errors and pass to callback
              (lambda (callback &rest fn-args)
                (condition-case err
                    (apply original-fn callback fn-args)
                  (error (funcall callback (format "Error: %s" (error-message-string err))))))
            original-fn))
         (tool (apply #'gptel-make-tool (plist-put args :function wrapped-fn))))
    (gptelt-mcp-register-one-gptel-tool tool)
    tool
    ;; nil
    ))

(comment
  (gptelt-parse-tool-schema "glob")
  (gptelt-parse-tool-schema "clj_list_ns")
  (type-of (gptel-tool-function (gptelt-get-tool "visible_buffers")))
  (gptelt-get-tool "visible_buffers")


  (setq mcp-server-lib-log-io t)
  mcp-server-lib--tools)


(defun +mcp-server-lib--call-gptel-tool (tool args)
  (let ((default-directory (++workspace-current-project-root))
        (arg-values))
    (setq arg-values
          (mapcar
           (lambda (arg)
             arg
             (let ((key (intern (plist-get arg :name))))
               (alist-get key args)))
           (gptel-tool-args tool)))
    (apply (gptel-tool-function tool) arg-values)))

(defadvice! +mcp-server-lib--handle-gptel-tools-call
  (orig-fn id params method-metrics)
  :around #'mcp-server-lib--handle-tools-call
  (let* ((tool-name (alist-get 'name params))
         (tool (gethash tool-name mcp-server-lib--tools))
         (tool-args (alist-get 'arguments params)))
    (if-let ((one-gptel-tool (and tool (plist-get tool :gptel-tool))))
        (let ((context (list :id id)))
          (condition-case err
              (let*
                  ((result
                    (+mcp-server-lib--call-gptel-tool one-gptel-tool tool-args))
                   (result-text
                    (cond
                     ((null result) "")
                     (t (gptel--to-string result))))
                   ;; Wrap the handler result in the MCP format
                   (formatted-result
                    `((content
                       .
                       ,(vector
                         `((type . "text") (text . ,result-text))))
                      (isError . :json-false))))
                (gptelt-log-mcp-tool
                 tool-name tool-args formatted-result)
                (mcp-server-lib-metrics--track-tool-call tool-name)
                (mcp-server-lib--respond-with-result
                 context formatted-result))
            (error
             (mcp-server-lib-metrics--track-tool-call tool-name t)
             (cl-incf (mcp-server-lib-metrics-errors method-metrics))
             (let ((formatted-error
                    `((content
                       .
                       ,(vector
                         `((type . "text") (text . ,(gptel--to-string err)))))
                      (isError . t))))
               (mcp-server-lib--respond-with-result
                context formatted-error)))))
      (funcall orig-fn id params method-metrics))))

(defun +gptel-tool-revert-to-be-edited-buffer (buf)
  (let ((buf (or buf (current-buffer)))
        (buf-name (buffer-file-name buf)))
    (when (and buf-name
               (not (doom-visible-buffer-p buf))
               (file-exists-p buf-name)
               (not (verify-visited-file-modtime buf)))
      (with-current-buffer buf
        (revert-buffer :ignore-auto :noconfirm)))))

;;; utils.el ends here
