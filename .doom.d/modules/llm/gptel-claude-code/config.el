;;; llm/gptel-claude-code/config.el -*- lexical-binding: t; -*-

(load! "gptel-claude-code.el")

(defconst gptel--claude-code-models
  `((haiku
     :description "Near-frontier intelligence at blazing speeds with extended thinking"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 1
     :output-cost 5
     :cutoff-date "2025-02")
    (sonnet
     :description "High-performance model with exceptional reasoning and efficiency"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 3
     :output-cost 15
     :cutoff-date "2025-07")
    (,(intern "sonnet[1m]")
     :description "High-performance model with exceptional reasoning and efficiency"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 1000
     :input-cost 3
     :output-cost 15
     :cutoff-date "2025-07")
    (opus
     :description "Most capable model for complex reasoning and advanced coding"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 5
     :output-cost 25
     :cutoff-date "2025-03")
    (,(intern "opus[1m]")
     :description "Most capable model for complex reasoning and advanced coding"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 1000
     :input-cost 5
     :output-cost 25
     :cutoff-date "2025-03")))

(use-package! gptel-claude-code
  :after gptel
  :init
  (setq! gptel-claude-code-skip-permissions t)
  :config
  (gptel-make-claude-code "Claude Code"
    :models gptel--claude-code-models
    :stream t
    :mcp-port 18684))

(comment
  (setf (gptel-claude-code-permission-mode (alist-get "Claude Code" gptel--known-backends nil nil #'equal)) "default"))

;; Validate claude binary exists
(unless (executable-find "claude")
  (message "gptel-claude-code: 'claude' binary not found in PATH. Backend will fail."))

;; Validate gptel internals we advise
(with-eval-after-load 'gptel
  (unless (fboundp 'gptel--handle-wait)
    (warn "gptel-claude-code: gptel--handle-wait not found, incompatible gptel version")))
