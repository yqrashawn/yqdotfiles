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
(defun +gptel-make-my-presets ()
  (gptel-make-preset 'default
    :description "default preset"
    :backend "CopilotB"
    :model 'gpt-4.1-2025-04-14
    :system (alist-get 'default gptel-directives)
    :temperature 0.8
    :tools
    (cl-mapcan
     (lambda (x)
       (let ((category (car x)))
         (seq-map 'car (alist-get category gptel--known-tools))))
     gptel--known-tools))
  (gptel--apply-preset 'default))


(use-package! gptel
  :commands (gptel)
  :init
  (setq! gptel-api-key +open-ai-api-key
         gptel-default-mode 'org-mode
         gptel-expert-commands t
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
      (let ((b (get-buffer bname)))
        (and (null gptel-display-buffer-action)
             (or (buffer-local-value 'gptel-mode b)
                 (and (buffer-file-name b)
                      (string-match-p "^/User/.*/Dropbox/sync/gptel/gptel-"
                                      (buffer-file-name (current-buffer))))))))
    :autosave t
    :select t
    :side 'right
    :size 0.35
    :quit t
    :ttl nil)
  (require 'magit)
  (require 'gitleaks)

  (add-hook! 'gptel-mode-hook
    (defun +gptel-mode-setup-kill-buffer-hook ()
      (add-hook! 'kill-buffer-hook :local
        (defun +gptel-mode-reset-on-buffer-killed-h ()
          (with-current-buffer (current-buffer)
            (gptel-context-remove-all nil))))))

  (defun +gptel-context-add-buffer (b)
    (unless (llm-danger-buffer-p b)
      (with-current-buffer b
        (gptel-context--add-region b (point-min) (point-max) t))))

  (defadvice! +before-gptel-make-fsm (&optional args)
    :before #'gptel-send
    (require 'gptel-context)
    (gptel-context-remove-all nil)
    (+gptel-context-add-buffer (+current-workspace-info-buffer))
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
  (setq! gptel--gh-copilot-local
         (gptel-make-openai "Github Copilot"
           :protocol "http"
           :host "localhost:4141"
           :endpoint "/chat/completions"
           :stream t
           :key "no-key-required"
           :models gptel--gh-models))
  ;; gptel one
  (setq! gptel--gh-copilot-individual
         (gptel-make-gh-copilot "CopilotI"
           :host "api.individual.githubcopilot.com"
           :curl-args (list "--insecure")))
  (setq! gptel--gh-copilot-business
         (gptel-make-gh-copilot "CopilotB"
           :host "api.business.githubcopilot.com"
           :models gptel--gh-copilot-business-models
           :curl-args (list "--insecure")
           :stream t))
  (setq! gptel-backend gptel--openrouter)
  (setq! gptel-backend gptel--gh-claude-code)
  (setq! gptel-backend gptel--gh-copilot-local)
  (setq! gptel-backend gptel--gh-copilot-individual)
  (setq! gptel-backend gptel--gh-copilot-business)
  (setq! gptel-model 'gemini-2.5-pro)
  (setq! gptel-model 'claude-sonnet-4)
  (setq! gptel-model 'gpt-4.1-2025-04-14)
  (add-hook! 'gptel-post-response-functions '+gptel-save-buffer)
  (add-hook! 'gptel-post-response-functions #'my/gptel-remove-headings)
  (setq! gptel-log-level 'debug)
  (setq! gptel-log-level 'nil)
  (load! "gptel-tools.el")

  (el-patch-defun gptel-context--insert-buffer-string (buffer contexts)
    "Insert at point a context string from all CONTEXTS in BUFFER."
    (let ((is-top-snippet t)
          (previous-line 1))
      (insert
       (el-patch-swap
         (format "In buffer `%s`:" (buffer-name buffer))
         (format "In buffer `%s`%s:"
                 (buffer-name buffer)
                 (if-let ((buf-file (buffer-file-name buffer)))
                     (format ", file `%s`" buf-file)
                   "")))
       "\n\n```" (gptel--strip-mode-suffix (buffer-local-value
                                            'major-mode buffer))
       "\n")
      (dolist (context contexts)
        (let* ((start (overlay-start context))
               (end (overlay-end context))
               content)
          (let (lineno column)
            (with-current-buffer buffer
              (without-restriction
                (setq lineno (line-number-at-pos start t)
                      column (save-excursion (goto-char start)
                                             (current-column))
                      content (buffer-substring-no-properties start end))))
            ;; We do not need to insert a line number indicator if we have two regions
            ;; on the same line, because the previous region should have already put the
            ;; indicator.
            (unless (= previous-line lineno)
              (unless (= lineno 1)
                (unless is-top-snippet
                  (insert "\n"))
                (insert (format "... (Line %d)\n" lineno))))
            (setq previous-line lineno)
            (unless (zerop column) (insert " ..."))
            (if is-top-snippet
                (setq is-top-snippet nil)
              (unless (= previous-line lineno) (insert "\n"))))
          (insert content)))
      (unless (>= (log/spy (overlay-end (car (last contexts))))
                  (log/spy (point-max)))
        (insert "\n..."))
      (insert "\n```")))

  (defadvice! +gptel-mcp--activate-tools (_)
    :after #'gptel-mcp--activate-tools
    (+gptel-make-my-presets)))

;;; mcp
(use-package! mcp
  :after gptel
  :config
  (require 'gptel-integrations)
  (require 'mcp-hub)
  (mcp-hub-start-all-server
   (lambda ()
     (gptel-mcp-connect)
     (+gptel-make-my-presets)

     (setq-default gptel--preset 'default)))
  (setq! mcp-log-level 'debug)
  (setq! mcp-log-level 'info))
