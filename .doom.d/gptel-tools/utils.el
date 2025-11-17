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

;; Note: JSON serialization with :json-false/:null is now handled by
;; mcp-server-lib--json-encode, no need for custom advice

(defvar gptelt-log-persist-all-log t)

(defun gptelt-log-mcp-tool (tool-name tool-args tool-result)
  ;; Use mcp-server-lib--json-encode which already handles :json-false/:null correctly
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
    (-const nil)))

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
  "Get the current project root directory."
  (or (when (fboundp '++workspace-current-project-root)
        (++workspace-current-project-root))
      (when (fboundp 'project-current)
        (when-let ((project (project-current)))
          (if (fboundp 'project-root)
              (project-root project)
            (car (project-roots project)))))
      default-directory))

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

(defun gptelt--replace-buffer-directly (buffer result-string)
  "Apply edit to BUFFER by replacing entire buffer with RESULT-STRING.

Returns a message describing the result of the operation."
  (let ((original-buffer buffer)
        (original-point (with-current-buffer buffer (point))))

    ;; Apply the edit directly in the original buffer
    (with-current-buffer original-buffer
      (save-excursion
        (erase-buffer)
        (insert result-string)
        (when (buffer-file-name)
          (+force-save-buffer))
        (goto-char (min original-point (point-max)))
        (when (and (fboundp 'lsp-format-region)
                   (bound-and-true-p lsp-mode))
          (condition-case err
              (lsp-format-region (point-min) (point-max))
            (error (message "LSP formatting failed: %s" err))))))

    (format "Successfully replaced text in %s." (buffer-name original-buffer))))

(defun gptelt-get-tool (tool-or-name)
  (if (stringp tool-or-name)
      (gptel-get-tool tool-or-name)
    tool-or-name))

(defun gptelt-make-tool (&rest args)
  (require 'mcp-server-lib)
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
                  (error (funcall callback (format "Error: %s"
                                                   (error-message-string err))))))
            original-fn))
         (tool (apply #'gptel-make-tool (plist-put args :function wrapped-fn))))
    (mcp-server-lib-register-gptel-tool tool)
    tool
    ;; nil
    ))

(defun +gptel-tool-revert-to-be-visited-buffer (buf)
  "Revert buffer BUF if its file has been modified on disk.
Reverts both visible and non-visible buffers to ensure consistency after tool edits."
  (let ((buf (or buf (current-buffer)))
        (buf-name (buffer-file-name buf)))
    (when (and buf-name
               (file-exists-p buf-name)
               (not (verify-visited-file-modtime buf)))
      (with-current-buffer buf
        (revert-buffer :ignore-auto :noconfirm t)))))

(comment
  ;; Put this in *scratch* and C-j on each line, or eval it:
  (setq debug-on-error nil)
  (setq debug-on-message
        "^Error running timer: (wrong-type-argument listp #<process gptel-curl")
  (setq debug-on-message nil))
;;; utils.el ends here
