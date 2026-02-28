;;; llm/gptel-claude-code/config.el -*- lexical-binding: t; -*-

(load! "gptel-claude-code.el")

(use-package! gptel-claude-code
  :after gptel
  :config
  (gptel-make-claude-code "Claude Code"
    :models gptel--claude-code-models
    :stream t
    :mcp-port (if (boundp 'mcp-server-lib-http-port)
                  mcp-server-lib-http-port
                8080)))
(comment
  (setf (gptel-claude-code-permission-mode (alist-get "Claude Code" gptel--known-backends nil nil #'equal)) "default"))

;; Validate claude binary exists
(unless (executable-find "claude")
  (message "gptel-claude-code: 'claude' binary not found in PATH. Backend will fail."))

;; Validate gptel internals we advise
(with-eval-after-load 'gptel
  (unless (fboundp 'gptel--handle-wait)
    (warn "gptel-claude-code: gptel--handle-wait not found, incompatible gptel version")))
