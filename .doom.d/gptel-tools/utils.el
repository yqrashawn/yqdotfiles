;;; .nixpkgs/.doom.d/gptel-tools/utils.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Shared utilities for gptel edit/create tools: project/file/mode/balance logic.
;;; Code:

(require 'smartparens nil t)

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

(defun gptelt--attempt-parinfer-balance (buffer)
  "Use parinfer-rust-mode to attempt to balance BUFFER.

Returns (t . nil) if successful, (nil . error-message) if failed.
Does nothing if parinfer-rust-mode not available."
  (if (not (featurep 'parinfer-rust-mode))
      (cons t nil)             ; No-op, treat as success if parinfer not available
    (with-current-buffer buffer
      (let ((parinfer-rust-preferred-mode "smart")
            (parinfer-rust--in-debug nil)
            (parinfer-rust--disable nil)
            (result nil)
            (error-msg nil))
        (condition-case err
            (progn
              (let ((initial (buffer-substring-no-properties (point-min) (point-max))))
                (flymake-mode 1)          ; required by parinfer
                (unless (bound-and-true-p parinfer-rust-mode)
                  (parinfer-rust-mode 1))
                (setq parinfer-rust--disable nil)
                (setq parinfer-rust--in-debug nil)
                (setq parinfer-rust--mode "smart")
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

(defun gptelt--check-buffer-balanced-parens (buffer)
  "Check if BUFFER has balanced parentheses for Lisp modes.
Returns (BALANCED-P . ERROR-MESSAGE). Tries parinfer auto-repair if available."
  (let ((mj-mode (buffer-local-value 'major-mode buffer)))
    (if (and (gptelt--is-lisp-mode-p mj-mode)
             (fboundp 'sp-region-ok-p))
        (with-current-buffer buffer
          (condition-case err
              (if (sp-region-ok-p (point-min) (point-max))
                  (cons t nil)
                ;; Try parinfer auto-repair if unbalanced
                (gptelt--attempt-parinfer-balance buffer))
            (error (cons nil (error-message-string err)))))
      (cons t nil)))) ; Non-Lisp modes always pass

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
  "Apply edit to BUFFER by directly replacing OLD-STRING with NEW-STRING.
Returns a message describing the result of the operation."
  (let ((original-buffer buffer)
        (original-point (with-current-buffer buffer (point))))

    ;; Apply the edit directly in the original buffer
    (with-current-buffer original-buffer
      (save-excursion
        (erase-buffer)
        (insert result-string)
        (save-buffer)
        (goto-char original-point)
        (when (and (fboundp 'lsp-format-region)
                   (bound-and-true-p lsp-mode))
          (condition-case err
              (lsp-format-region replacement-start replacement-end)
            (error (message "LSP formatting failed: %s" err))))))

    (format "Successfully replaced text in %s." (buffer-name original-buffer))))

;;; utils.el ends here
