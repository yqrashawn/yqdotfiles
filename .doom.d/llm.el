;;; .nixpkgs/.doom.d/llm.el -*- lexical-binding: t; -*-

;;; init
(load! "models.el")

;;; helper fns
(defun +gptel-save-buffer (&rest args)
  (interactive)
  (when-let ((buf (current-buffer)))
    (with-current-buffer buf
      ;; (gptel-context-remove-all nil)
      (if buffer-file-name
          (save-buffer)
        (write-file
         (format
          (expand-file-name
           "~/Dropbox/sync/gptel/gptel-%s.org")
          (format-time-string
           "%Y%m%d-%H%M%S-%3N")))))))

(defun +gptel-kill-default-buffer ()
  (interactive)
  (when-let ((buf (get-buffer gptel-default-session)))
    (kill-buffer buf)))

(defun my/gptel-remove-headings (beg end)
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward org-heading-regexp end t)
        (forward-line 0)
        (delete-char (1+ (length (match-string 1))))
        (insert-and-inherit "*")
        (end-of-line)
        (skip-chars-backward " \t\r")
        (insert-and-inherit "*")))))

;;; gptel
(use-package! gptel
  :commands (gptel)
  :init
  (setq! gptel-api-key +open-ai-api-key
         gptel-default-mode 'org-mode
         gptel-temperature 0.8
         gptel-org-branching-context t
         gptel-track-media t)
  ;; (defadvice! +gptel-cleanup-default-buffer (&rest args)
  ;;   :before #'gptel
  ;;   (+gptel-kill-default-buffer))
  :config
  (require 'gptel-context)
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n")
  (set-popup-rule!
    (lambda (bname _action)
      (and (null gptel-display-buffer-action)
           (buffer-local-value 'gptel-mode (get-buffer bname))))
    :select t
    :side 'right
    :size 0.35
    :quit t
    :ttl nil)
  (require 'magit)
  (add-hook! 'gptel-mode-hook
    (defun +gptel-mode-setup-kill-buffer-hook ()
      (add-hook! 'kill-buffer-hook :local
        (defun +gptel-mode-reset-on-buffer-killed-h ()
          (with-current-buffer (current-buffer)
            (gptel-context-remove-all nil))))))
  (defun +gptel-context-add-buffer (b)
    (unless (llm-danger-buffer-p b)
      (let ((start (with-current-buffer b (point-min)))
            (end (with-current-buffer b (point-max))))
        (gptel-context--add-region b start end t))))

  (defadvice! +before-gptel-make-fsm (&optional args)
    :before #'gptel-send
    (require 'gptel-context)
    (gptel-context-remove-all nil)
    (+gptel-context-add-buffer (+visible-buffers-list-buffer))
    (+gptel-context-add-buffer (+magit-wip-diff-n-min-buffer 5))
    (dolist (b (seq-filter
                (lambda (b)
                  (and
                   (not (string= (buffer-name b) "*Messages*"))
                   (with-current-buffer b
                     (not gptel-mode))))
                (mapcar 'window-buffer (window-list))))
      (+gptel-context-add-buffer b))
    (when-let ((root (++workspace-current-project-root)))
      (dolist (f +llm-project-default-files)
        (when-let ((b (get-file-buffer (format "%s%s" root f))))
          (+gptel-context-add-buffer b)))))

  (setq! gptel--openrouter
         (gptel-make-openai "OpenRouter"
           :host "openrouter.ai"
           :endpoint "/api/v1/chat/completions"
           :stream t
           :key +openrouter-api-key
           :models gptel--openrouter-models))
  (setq! gptel-model 'gemini-2.5-pro)
  (setq! gptel-model 'claude-sonnet-4)
  ;; self host claude code
  (setq! gptel--gh-claude-code
         (gptel-make-openai "CCode"
           :protocol "http"
           :host "localhost:14141"
           :endpoint "/v1/chat/completions"
           :stream t
           :key "no-key-required"
           :models gptel--claude-code-models))
  ;; self host gh copilot
  (setq! gptel--gh-copilot
         (gptel-make-openai "Github Copilot"
           :protocol "http"
           :host "localhost:4141"
           :endpoint "/chat/completions"
           :stream t
           :key "no-key-required"
           :models gptel--gh-models))
  ;; gptel one
  (setq! gptel--gh-copilot (gptel-make-gh-copilot "Copilot"))
  (setq! gptel-backend gptel--openrouter)
  (setq! gptel-backend gptel--gh-copilot)
  (setq! gptel-backend gptel--gh-claude-code)
  (add-hook! 'gptel-post-response-functions '+gptel-save-buffer)
  (add-hook! 'gptel-post-response-functions #'my/gptel-remove-headings)
  (setq! gptel-log-level 'debug)
  (setq! gptel-log-level 'nil)
  (load! "gptel-tools.el")

  (defadvice! +gptel-context--insert-buffer-string (orig-fn buffer contexts)
    "add buffer file name"
    :around #'gptel-context--insert-buffer-string
    (when-let ((buf-file (buffer-file-name buffer)))
      (let ((file-project-root (with-current-buffer buffer (doom-project-root))))
        (insert "In file `%s` (file buffer name is `%s`%s):"
                buf-file
                (buffer-name buffer)
                (if file-project-root
                    (format! ", file project root is at `%s`" file-project-root)
                  ""))))
    (funcall orig-fn)))

;;; mcp
(use-package! mcp
  :after gptel
  :config
  (require 'gptel-integrations)
  (require 'mcp-hub)
  (mcp-hub-start-all-server (lambda () (gptel-mcp-connect)))
  (setq! mcp-log-level 'debug)
  (setq! mcp-log-level 'info))
