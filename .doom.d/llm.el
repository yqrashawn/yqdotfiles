;;; .nixpkgs/.doom.d/llm.el -*- lexical-binding: t; -*-

;;; init
(load! "models.el")
(setq! +gptel-default-preset 's)

;;; helper fns
(defun +gptel-sanitize-filename (str)
  "Clean STR for use as a filename."
  (thread-last str
               (string-trim)
               (replace-regexp-in-string "```[a-z]*" "")
               (replace-regexp-in-string "[^[:alnum:][:space:]-]" "_")
               (replace-regexp-in-string "[[:space:]]+" "_")))

(defun +gptel-save-buffer (&rest _args)
  (interactive)
  (when-let ((buf (current-buffer)))
    (with-current-buffer buf
      (cond
       ;; File on disk and modified — just save
       ((and buffer-file-name
             (file-exists-p buffer-file-name)
             (buffer-modified-p))
        (save-buffer))
       ;; No file on disk — generate title and save to Dropbox
       ((not buffer-file-name)
        (get-gptel-org-title
         (buffer-string)
         (lambda (title)
           (let ((new-title (+gptel-sanitize-filename title)))
             (with-current-buffer buf
               (let ((dir (format
                           "~/Dropbox/sync/gptel/%s/%s/%s"
                           (format-time-string "%Y")
                           (format-time-string "%m")
                           (format-time-string "%d"))))
                 (unless (file-directory-p dir)
                   (make-directory dir t))
                 (+set-org-top-header new-title)
                 (insert "\n")
                 (+set-org-title new-title)
                 (let ((filepath (expand-file-name
                                  (format
                                   "%s-%s.org"
                                   (format-time-string "%H_%M")
                                   new-title)
                                  dir)))
                   ;; Avoid write-file which calls rename-buffer
                   ;; synchronously — that breaks persp-mode window
                   ;; config when response completes in another workspace.
                   (setq buffer-file-name filepath)
                   (setq buffer-file-truename (file-truename filepath))
                   (setq default-directory (file-name-directory filepath))
                   (set-buffer-modified-p t)
                   (save-buffer)
                   ;; Rename buffer to match filename. If visible, rename
                   ;; now. Otherwise defer until displayed — remove hook
                   ;; BEFORE rename to avoid infinite loop.
                   (let ((new-name (file-name-nondirectory filepath)))
                     (if (get-buffer-window buf t)
                         (rename-buffer new-name t)
                       (let (hook-fn)
                         (setq hook-fn
                               (lambda ()
                                 (when (and (buffer-live-p buf)
                                            (get-buffer-window buf t))
                                   (remove-hook 'buffer-list-update-hook hook-fn)
                                   (with-current-buffer buf
                                     (rename-buffer new-name t)))))
                         (add-hook 'buffer-list-update-hook hook-fn)))))))))
         (lambda (e) (user-error
                      "Error setting gptel org title: %s"
                      (if (plistp e)
                          (or (plist-get e :error)
                              (plist-get e :status)
                              (format "%S" e))
                        (error-message-string e))))))))))

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

(defun +gptel-collapse-blank-lines (_beg _end)
  "Collapse blank lines in gptel org buffers.
Around @assistant and @user markers: max 1 blank line.
Everywhere else: max 2 consecutive blank lines."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^@\\(assistant\\|user\\)$" nil t)
        (let ((marker-line-start (line-beginning-position)))
          ;; Collapse blank lines above: keep at most 1
          (save-excursion
            (goto-char marker-line-start)
            (forward-line -1)
            (while (and (not (bobp))
                        (looking-at-p "^[ \t]*$"))
              (forward-line -1))
            ;; Now point is on the last non-blank line above (or bob)
            (unless (looking-at-p "^[ \t]*$") (forward-line 1))
            (let ((blank-start (point)))
              (goto-char marker-line-start)
              (when (> (count-lines blank-start marker-line-start) 1)
                (delete-region blank-start marker-line-start)
                (insert "\n"))))
          ;; Collapse blank lines below: keep at most 1
          (save-excursion
            (goto-char (line-beginning-position))
            ;; re-find since deletions above may have shifted point
            (when (re-search-forward "^@\\(assistant\\|user\\)$" (line-end-position) t)
              (forward-line 1)
              (let ((after-marker (point)))
                (while (and (not (eobp))
                            (looking-at-p "^[ \t]*$"))
                  (forward-line 1))
                (when (> (count-lines after-marker (point)) 1)
                  (delete-region after-marker (point))
                  (insert "\n")))))))
      ;; Pass 2: collapse 3+ consecutive blank lines to max 2 everywhere
      (goto-char (point-min))
      (while (re-search-forward "\n\\(\n[ \t]*\\)\\{3,\\}" nil t)
        (replace-match "\n\n\n")))))

(comment
  (defadvice! +gptel-curl--stream-insert-response
    (f response info &optional raw)
    :around #'gptel-curl--stream-insert-response
    (let* ((start-marker (plist-get info :position))
           (tracking-marker (plist-get info :tracking-marker))
           (cur-marker (or tracking-marker start-marker)))
      (with-current-buffer (marker-buffer cur-marker)
        (when (eq gptel-backend gptel--claude-code)
          (save-excursion
            (goto-char cur-marker)
            (when (or (eq (char-before) ?:)
                      (eq (char-before) ?.))
              (insert ?\n)
              (insert ?\n))))))
    (funcall f response info raw)))

;;; gptel
(defun +gptel-make-my-presets ()
  (gptel-make-preset 'default
    :description "copilot default"
    :backend "cpi"
    ;; :model 'claude-sonnet-4.5
    ;; :system (alist-get 'claude gptel-directives)
    :model 'gpt-4.1
    :system (alist-get 'gpt41 gptel-directives)
    ;; :system (alist-get 'default gptel-directives)
    :temperature 0.8
    :tools
    (cl-mapcan
     (lambda (x)
       (let ((category (car x)))
         (seq-filter
          (lambda (tool-name)
            (not (string-prefix-p "emacs_" tool-name)))
          (seq-map 'car (alist-get category gptel--known-tools)))))
     gptel--known-tools))

  (gptel-make-preset 'ha
    :description "cc haiku"
    :backend "Claude Code"
    :model 'haiku
    :system ""
    :parents '(default)
    :tools '())
  (gptel-make-preset 'so
    :description "cc sonnet"
    :backend "Claude code"
    :model 'sonnet
    :system ""
    :parents '(default)
    :tools '())
  (gptel-make-preset 'op
    :description "cc opus"
    :model 'opus
    :parents '(so))
  (gptel-make-preset 's
    :description "cchp sonnet medium"
    :backend "ccl"
    :model 'sonnet-medium
    :system (alist-get 'claude gptel-directives)
    :parents '(default)
    :tools '())
  (gptel-make-preset 'sl
    :description "cchp sonnet low"
    :model 'sonnet-low
    :parents '(s))
  (gptel-make-preset 'sm
    :description "cchp sonnet max"
    :model 'sonnet-max
    :parents '(s))
  (gptel-make-preset 'sh
    :description "cchp sonnet high"
    :model 'sonnet-high
    :parents '(s))
  (gptel-make-preset 'o
    :description "cchp opus medium"
    :model 'opus-medium
    :parents '(s))
  (gptel-make-preset 'ol
    :description "cchp opus low"
    :model 'opus-low
    :parents '(s))
  (gptel-make-preset 'om
    :description "claude code opus max"
    :model 'opus-max
    :parents '(s))
  (gptel-make-preset 'oh
    :description "claude code"
    :model 'opus-high
    :parents '(s))
  (gptel-make-preset 'h
    :description "cchp haiku"
    :model 'haiku
    :parents '(s))
  (gptel-make-preset 'sla
    :description "cchp/anthropic dev sonnet low"
    :backend "cclda"
    :model 'sonnet-low
    :parents '(s))
  (gptel-make-preset 'slg
    :description "cchp/gemini dev sonnet low"
    :backend "ccldg"
    :model 'sonnet-low
    :parents '(s))
  (gptel-make-preset 'sd
    :description "cchp dev sonnet"
    :backend "ccld"
    :model 'sonnet
    :parents '(s))
  (gptel-make-preset 'od
    :description "cchp dev opus"
    :model 'opus
    :parents '(sd))
  (gptel-make-preset 'omd
    :description "cchp dev opus"
    :model 'opus-max
    :parents '(sd))
  (gptel-make-preset 'hd
    :description "cchp dev haiku"
    :model 'haiku
    :parents '(sd))
  (gptel-make-preset 'glmd
    :description "cchp dev glm 5"
    :backend "ccld"
    :model (intern "0g-zai-org/GLM-5-FP8")
    :parents '(s))
  
  (gptel-make-preset 'ghsonnet
    :description "copilot sonnet"
    :backend "cpi"
    :parents '(default)
    :model 'claude-sonnet-4.6)

  (gptel-make-preset 'codex
    :description "codex"
    :backend "codex"
    :model 'gpt-5-codex
    :system (alist-get 'default gptel-directives)
    :parents '(default)))

;;;###autoload
(defun +gptel (arg)
  (interactive "P")
  (cond
   ((region-active-p) (call-interactively #'gptel-rewrite))
   ((not arg) (call-interactively #'gptel))
   ((eq arg 7) (call-interactively #'gptel-menu))
   ((eq arg 8) (call-interactively #'+gptel-inject-message))
   ((eq arg 88) (call-interactively #'+gptel-find-session-id))
   ((eq arg 87) (call-interactively #'+gptel-claude-here))
   ((eq arg 90)
    (progn (funcall #'gptel-context-remove-all)
           (message "gptel context removed!")))
   ((eq arg 6) (call-interactively #'gptel-context-add))))

(defvar +llm-project-default-files '())
(make-variable-buffer-local '+llm-project-default-files)
(put '+llm-project-default-files 'safe-local-variable #'listp)
(setq! +llm-global-project-default-files
       '("CLAUDE.md" "AGENTS.md" "README.md" "README.org" ".github/copilot-instructions.md" "llm.txt" "llm.md"
         "llm.org" "deps.edn" "duct.edn" "shadow-cljs.edn" "package.json"))
(setq! +llm-project-default-files +llm-global-project-default-files)

(defun +llm-get-project-default-files ()
  "Return a list of default project files.
Merge buffer-local with global default files."
  (delete-dups
   (append
    +llm-project-default-files
    +llm-global-project-default-files)))

(use-package! gptel
  :commands (gptel gptel-context-add)
  :init
  (setq! gptel-api-key +open-ai-api-key
         gptel-include-tool-results nil
         gptel-include-reasoning nil
         gptel-default-mode 'org-mode
         gptel-expert-commands t
         gptel-temperature 1.0
         gptel-org-branching-context t
         gptel-track-media t
         +gptel-disabled-tool-patterns
         '("^show_api_key$" "^capture_screenshot" "^guess_datetime")
         gptel-curl-extra-args
         (split-string
          (mapconcat
           'identity
           '("--http1.1"
             ;; "--fail-with-body"
             "--no-buffer" ;flush chunks ASAP so Emacs sees tokens as they arrive.
             ;; "--retry 3" "--retry-delay 1"
             "--connect-timeout 10"
             "--max-time 7200"
             "--no-alpn"                  ;tsl
             "-4"                         ;force ipv4
             "--insecure")
           " ")))

  ;; (defadvice! +gptel-cleanup-default-buffer (&rest args)
  ;;   :before #'gptel
  ;;   (+gptel-kill-default-buffer))
  (defun +gptel-buffer? (&optional b)
    (let ((b (if b (get-buffer b) (current-buffer))))
      (and (null gptel-display-buffer-action)
           (or (buffer-local-value 'gptel-mode b)
               (and (buffer-file-name b)
                    (string-match-p "^/User/.*/Dropbox/sync/gptel/gptel-"
                                    (buffer-file-name b))))))
    nil)
  :config
  (require 'gptel-context)
  (require 'pcre2el)
  (require 'gptel-gh)
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user: ")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant: ")
  (setq! gptel-display-buffer-action
         `(display-buffer-same-window . nil))
  (comment
    (set-popup-rule!
      (lambda (bname _action) (+gptel-buffer? bname))
      :autosave t
      :select t
      :side 'right
      :size 0.35
      :quit nil
      :ttl nil))

  ;; Dynamic slot assignment for stacking multiple gptel popup windows
  (defun +gptel-popup-windows ()
    "Return list of popup windows displaying gptel buffers."
    (cl-remove-if-not
     (lambda (w)
       (and (+gptel-buffer? (window-buffer w))
            (window-parameter w 'popup)))
     (window-list)))

  (defadvice! +gptel-popup-assign-dynamic-slot (orig-fn buffer alist)
    "Assign unique slot to gptel buffers for vertical stacking."
    :around #'+popup-buffer
    (when (and buffer (+gptel-buffer? buffer))
      (let ((existing-slots
             (mapcar (lambda (w) (or (window-parameter w 'window-slot) 0))
                     (+gptel-popup-windows))))
        ;; Find the next available slot
        (setf (alist-get 'slot alist)
              (if existing-slots
                  (1+ (apply #'max existing-slots))
                0))))
    (funcall orig-fn buffer alist))

  (defadvice! ++popup--delete-window (win)
    :before #'+popup--delete-window
    (let* ((buffer (window-buffer win))
           (buf-file
            (or (buffer-file-name buffer)
                (if-let* ((base-buffer (buffer-base-buffer buffer)))
                  (buffer-file-name base-buffer)))))
      (when (and
             buf-file
             (buffer-modified-p buffer)
             (string-prefix-p
              (file-truename "~/Dropbox/sync/gptel")
              (file-truename buf-file)))
        (with-current-buffer buffer (save-buffer)))))

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

  (defadvice! +before-gptel-make-fsm (&optional _args)
    :before #'gptel-send
    (unless simple-llm-req-p
      (require 'gptel-context)
      ;; Capture current workspace for this gptel request
      (setq-local ++gptel-request-workspace (get-current-persp))

      ;; Remove only the specific context buffers we manage, not all context
      (dolist (buf (list (+current-workspace-info-buffer)
                         (+visible-buffers-list-buffer)
                         (+current-workspace-lints-buffer)))
        (setf (alist-get buf gptel-context nil 'remove) nil))

      ;; Re-add updated context
      (+gptel-context-add-buffer (+current-workspace-info-buffer))
      (+gptel-context-add-buffer (+visible-buffers-list-buffer))
      ;; (+gptel-context-add-buffer (+magit-wip-diff-n-min-buffer 5))
      (when-let ((root (++workspace-current-project-root)))
        (dolist (f (with-current-buffer (find-file-noselect root)
                     (+llm-get-project-default-files)))
          (when-let ((file (file-truename (format "%s/%s" root f))))
            (when (f-exists-p file)
              (+gptel-context-add-buffer (find-file-noselect file))))))))

  (defadvice! +after-gptel-send (&optional _args)
    :after #'gptel-send
    (when (and (bound-and-true-p gptel-mode)
               (bound-and-true-p evil-local-mode)
               (evil-insert-state-p))
      (evil-normal-state)))

  (setq! gptel--tmp
         (gptel-make-openai "tmp"
           :host "localhost:8080"
           :endpoint "/v1/chat/completions"
           :stream t
           :key "no-key"
           :models gptel--claude-code-models))
  (setq! gptel--openrouter
         (gptel-make-openai "OpenRouter"
           :host "openrouter.ai"
           :endpoint "/api/v1/chat/completions"
           :stream t
           :key +openrouter-api-key
           :models gptel--openrouter-models))
  ;; self host claude code
  (setq! gptel--claude-code
         (gptel-make-openai "cc"
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
  (setq! gptel--codex
         (gptel-make-openai "codex"
           :protocol "http"
           :host "localhost:18683"
           :endpoint "/v1/chat/completions"
           :stream t
           :key "no-key-required"
           :models gptel--codex-models))

  ;; gptel one
  (setq! gptel--gh-copilot-individual
         (gptel-make-gh-copilot "cpi"
           :host "api.individual.githubcopilot.com"))
  (setq! +gptel-free-backend gptel--gh-copilot-individual)
  (setq! gptel--gh-copilot-business
         (gptel-make-gh-copilot "cpb"
           :host "api.business.githubcopilot.com"
           :models gptel--gh-b-models
           :stream t))
  (setq! gptel-backend gptel--openrouter)
  (setq! gptel-backend gptel--codex)
  (setq! gptel-backend gptel--claude-code)
  (setq! gptel-backend gptel--gh-copilot-local)
  (setq! gptel-backend gptel--gh-copilot-business)
  (setq! gptel--ccld
         (gptel-make-openai "ccld"
           :protocol "http"
           :host "localhost:8003"
           :endpoint "/api/v1/chat/completions"
           :stream t
           :key "no-key-required"
           :models gptel--claude-code-models))
  (setq! gptel--cclda
         (gptel-make-anthropic "cclda"
           :protocol "http"
           :host "localhost:8003"
           :stream t
           :key "no-key-required"
           :models gptel--claude-code-models))
  (setq! gptel--ccldg
         (gptel-make-gemini "ccldg"
           :protocol "http"
           :host "localhost:8003"
           :stream t
           :key "no-key-required"
           :models gptel--claude-code-models))
  (setq! gptel-backend gptel--ccld)

  (setq! gptel--ccl
         (gptel-make-openai "ccl"
           :protocol "http"
           :host "localhost:8033"
           :endpoint "/api/v1/chat/completions"
           :stream t
           :key "no-key-required"
           :models gptel--claude-code-models))

  (setq! gptel-backend gptel--ccl)
  (+gptel-make-my-presets)
  (gptel--apply-preset +gptel-default-preset)

  (add-hook! 'gptel-post-response-functions '+gptel-save-buffer)
  (add-hook! 'gptel-post-response-functions #'my/gptel-remove-headings)
  ;; (add-hook! 'gptel-post-response-functions #'+gptel-collapse-blank-lines)
  (defun +gptel-toggle-debug ()
    (interactive)
    (if gptel-log-level
        (setq! gptel-log-level nil
               mcp-server-lib-http-log-requests nil)
      (setq! gptel-log-level 'debug
             mcp-server-lib-http-log-requests t)))
  (setq! gptel-log-level 'nil)

  (add-hook! 'gptel-post-response-functions
    (defun +gptel-notify-done (beg end)
      (when (> (float-time (or (current-idle-time) 0)) 60)
        (let* ((response-text (string-trim (buffer-substring-no-properties beg end)))
               (truncated (if (> (length response-text) 500)
                              (concat (substring response-text 0 500) "...")
                            response-text)))
          (pushover-send
           "GPTEL Done" truncated :sound "magic")))))

  (defadvice! +gptel-filter-tools-before-request (orig-fn &rest args)
    :around #'gptel-request
    (let* ((clj-workspace? (++workspace-clojure?))
           (proj-root (++workspace-current-project-root))
           (dot-proj? (string-suffix-p ".nixpkgs" proj-root))
           (gptel-tools
            (cl-remove-if
             (lambda (tool)
               (let ((tool-name (gptel-tool-name tool)))
                 (or (cl-some (lambda (pattern)
                                (string-match-p pattern tool-name))
                              +gptel-disabled-tool-patterns)
                     (and (not clj-workspace?)
                          (string-match-p "^cljs?_" tool-name))
                     (and (not dot-proj?)
                          (or (string-match-p "^elisp?_" tool-name)
                              (string-match-p "^run_ert" tool-name))))))
             gptel-tools)))
      (apply orig-fn args)))

  (defadvice! +gptel-add-request-timeout (orig-fn &rest args)
    :around #'gptel-request
    (let* ((fsm (apply orig-fn args))
           (timeout-seconds 7200)
           (timer (run-with-timer
                   timeout-seconds nil
                   (lambda (fsm)
                     (when-let* ((proc (cl-find-if
                                        (lambda (entry)
                                          (eq (car entry) nil))
                                        gptel--request-alist
                                        :key #'cdr)))
                       (message "gptel request timed out after %d seconds" timeout-seconds)
                       (gptel-abort (plist-get (gptel-fsm-info fsm) :buffer))))
                   fsm)))
      (add-hook 'gptel-post-response-functions
                (lambda (&rest _)
                  (when (timerp timer)
                    (cancel-timer timer)))
                nil t)
      fsm))

  ;; Strip `gptel' text property from yanked text in gptel-mode buffers.
  ;; Prevents assistant response text from being treated as a separate message
  ;; when pasted into a user message.
  (add-hook! 'gptel-mode-hook
    (defun +gptel-exclude-gptel-prop-on-yank ()
      (setq-local yank-excluded-properties
                  (cl-adjoin 'gptel yank-excluded-properties))))

  (after! pabbrev
    (add-hook! 'buffer-list-update-hook
      (defun +pabbrev-scavenge-all-visible-buffers-for-gptel ()
        (when (bound-and-true-p gptel-mode)
          (+pabbrev-scavenge-all-visible-buffers))))))

;;; mcp
(load! "mcp.el")

;;; llm fns
(load! "gptel-llm-fns.el")

(use-package! gptel-magit
  :defer t
  :config
  (defadvice! +gptel-magit-strip-code-fences (orig-fn message)
    "Strip markdown code fences from LLM-generated commit messages."
    :around #'gptel-magit--format-commit-message
    (funcall orig-fn
             (replace-regexp-in-string
              "\\`[ \t\n]*```[a-z]*\n\\(\\(?:.\\|\n\\)*?\\)\n```[ \t\n]*\\'"
              "\\1"
              message)))
  (defadvice! +gptel-magit-preserve-directory (orig-fn callback)
    "Capture `default-directory' so async callback runs in the correct repo.
Without this, magit-commit-create runs in the wrong directory after the
LLM response arrives and throws \"Nothing staged (or unstaged)\"."
    :around #'gptel-magit--generate
    (let ((dir default-directory))
      (funcall orig-fn (lambda (message)
                         (let ((default-directory dir))
                           (funcall callback message))))))
  (setq!
   ;; gptel-magit-model 'gpt-4.1
   ;; gptel-magit-model 'sonnet
   gptel-magit-model 'sonnet-low
   ;; gptel-magit-backend gptel--gh-copilot-business
   ;; gptel-magit-backend +gptel-free-backend
   ;; gptel-magit-backend gptel-claude-code-backend
   gptel-magit-backend gptel--ccl))

;;; Workspace persistence and isolation
;; Store and restore gptel-context per workspace
(after! gptel
  ;; Store gptel-context when switching away
  (add-hook! 'persp-before-deactivate-functions
    (defun +gptel-store-context-in-persp (_)
      (set-persp-parameter 'gptel-context gptel-context)))

  ;; Restore gptel-context when switching to a perspective
  (add-hook! 'persp-activated-functions
    (defun +gptel-restore-context-from-persp (_)
      (setq gptel-context (persp-parameter 'gptel-context)))))

;; Workspace isolation during LLM requests
;; The ++gptel-request-workspace variable is captured when gptel-send is called
;; and persists throughout the async tool execution. This allows:
;; 1. User to freely switch workspaces while LLM is working
;; 2. All tool calls use the original workspace's project root and settings
;; 3. Context buffers (lints, git diffs, etc.) are generated from the correct workspace
;;
;; Implementation:
;; 1. +gptel-request-capture-workspace: Captures workspace when gptel-request is called,
;;    stores it in the FSM INFO plist (:workspace key)
;; 2. +gptel--handle-tool-use-with-workspace: Binds ++gptel-request-workspace dynamically
;;    during tool execution (both sync and async tools)
;; 3. +mcp-server-lib--call-gptel-tool: Preserves workspace binding for MCP server tools
;; 4. ++workspace-current-project-root: Checks ++gptel-request-workspace first before
;;    falling back to current workspace

;; Capture workspace in gptel request INFO plist
(defadvice! +gptel-request-capture-workspace (orig-fn &rest args)
  "Capture current workspace in the request INFO plist."
  :around #'gptel-request
  (let* ((result (apply orig-fn args))
         (info (gptel-fsm-info result)))
    (plist-put info :workspace (get-current-persp))
    result))

;; Advice gptel's tool execution to bind the captured workspace
(defadvice! +gptel--handle-tool-use-with-workspace (orig-fn fsm)
  "Wrap tool execution to use the captured workspace from FSM's info."
  :around #'gptel--handle-tool-use
  (let* ((info (gptel-fsm-info fsm))
         (++gptel-request-workspace (plist-get info :workspace)))
    (funcall orig-fn fsm)))

(defadvice! +gptel-handle-invalid-tool-calls (orig-fn fsm)
  "Send error message to LLM when it calls non-existent tools.
Skips Claude Code backends since they handle all tool execution internally."
  :around #'gptel--handle-tool-use
  (let* ((info (gptel-fsm-info fsm))
         (backend (plist-get info :backend)))
    ;; Claude Code handles all tool execution internally (including MCP tools).
    ;; The FSM should never reach TOOL state for this backend, but if it does,
    ;; clear :tool-use and transition directly to DONE (skip all tool handling).
    (if (and (fboundp 'gptel-claude-code-p) (gptel-claude-code-p backend))
        (progn
          (plist-put info :tool-use nil)
          (gptel--fsm-transition fsm))
      (let* ((tool-use (cl-remove-if (lambda (tc) (plist-get tc :result))
                                     (plist-get info :tool-use)))
             (invalid-tools
              (cl-remove-if-not
               (lambda (tool-call)
                 (let ((name (plist-get tool-call :name)))
                   (and (not (equal name gptel--ersatz-json-tool))
                        (null (cl-find-if
                               (lambda (ts) (equal (gptel-tool-name ts) name))
                               (plist-get info :tools))))))
               tool-use)))
        (when invalid-tools
          ;; Handle invalid tool calls
          (with-current-buffer (plist-get info :buffer)
            (dolist (tool-call invalid-tools)
              (let ((error-msg (format "Error: Tool '%s' does not exist. Please check the tool name CAREFULLY against the list of available tools and try again with the correct tool name."
                                       (plist-get tool-call :name))))
                (plist-put tool-call :result error-msg)
                (message "Invalid tool call: %s" (plist-get tool-call :name))))
            ;; Mark at least one tool as successful to trigger state transition
            (plist-put info :tool-success t)
            ;; Inject error messages back to LLM
            (gptel--inject-prompt
             backend (plist-get info :data)
             (gptel--parse-tool-results backend (plist-get info :tool-use)))
            (funcall (plist-get info :callback)
                     (cons 'tool-error invalid-tools) info)
            (gptel--fsm-transition fsm)))
        ;; Always call original function to handle valid tool calls
        (funcall orig-fn fsm)))))


(defun +gptel-markdown-to-org (&optional input-string)
  "Convert markdown to org-mode format using gptel's converter.
When called interactively, converts the latest kill ring entry and puts
the result back into the kill ring.
When called non-interactively with INPUT-STRING, converts and returns
the result."
  (interactive)
  (if (called-interactively-p 'any)
      (let* ((markdown-input (current-kill 0))
             (org-output (gptel--convert-markdown->org markdown-input)))
        (kill-new org-output)
        (message "Converted markdown to org and saved to kill ring"))
    (gptel--convert-markdown->org input-string)))

(setq! gptel-log-level 'debug)
(setq! gptel-log-level nil)
(setq! mcp-log-level 'debug)
(setq! mcp-log-level 'info)
(setq! mcp-server-lib-log-io t)
(setq! mcp-server-lib-log-io nil)

(load! "gptel-extra.el")

;;; other agent/llm in emacs pkgs
(use-package agent-shell
  :defer t
  :ensure-system-package
  ((claude-code-acp . "pnpm install -g @zed-industries/claude-code-acp")
   (codex-acp . "pnpm install -g @zed-industries/codex-acp"))
  :config
  (setq!
   agent-shell-anthropic-claude-environment
   (agent-shell-make-environment-variables :inherit-env t)
   agent-shell-anthropic-authentication
   (agent-shell-anthropic-make-authentication :login t)))

(use-package! agent-shell-sidebar
  :after agent-shell)

(use-package! agent-shell-manager
  :after agent-shell)

(use-package claude-code
  :defer t
  ;; :bind-keymap
  ;; ("C-c c" . claude-code-command-map)
  ;; or your preferred key
  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  ;; :bind
  ;; (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode))
  :init
  (setq! claude-code-display-window-fn #'display-buffer)
  :config
  ;; optional IDE integration with Monet
  (add-hook! 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)
  (claude-code-mode))

(use-package! claude-code-ide
  :defer t
  :init
  (setq! claude-code-ide-terminal-backend 'eat
         claude-code-ide-use-side-window nil)
  :config
  (claude-code-ide-emacs-tools-setup))
