;;; .nixpkgs/.doom.d/llm.el -*- lexical-binding: t; -*-

;;; init
(load! "models.el")

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
      (if buffer-file-name
          (save-buffer)
        (progn
          (get-gptel-org-title
           (buffer-string)
           (lambda (title)
             (let ((new-title (+gptel-sanitize-filename title)))
               (with-current-buffer buf
                 (let ((dir (format
                             "~/Dropbox/sync/gptel/%s/%s"
                             (format-time-string "%Y")
                             (format-time-string "%m"))))
                   (unless (file-directory-p dir)
                     (make-directory dir t))
                   (+set-org-top-header new-title)
                   (insert "\n")
                   (+set-org-title new-title)
                   ;; (insert "\n")
                   (write-file
                    (expand-file-name
                     (format
                      "%s-%s-%s.org"
                      (format-time-string "%d")
                      (format-time-string "%H_%M")
                      new-title)
                     dir))))))
           (lambda (e) (user-error
                        "Error setting gptel org title: %s"
                        (error-message-string e))))
          t)))))

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
  (funcall f response info raw))

(defun my/claude-code-message-separator ()
  (when (eq gptel-backend gptel--claude-code)
    (unless (eq (char-before) ?\n)
      (call-interactively '+org/return))))

;;; gptel
(defun +gptel-make-my-presets ()
  (gptel-make-preset 'default
    :description "default preset"
    :backend "cpb"
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

  (gptel-make-preset 'cob
    :description "preset"
    :backend "cpb"
    :parents '(default)
    :model 'claude-3.7-sonnet
    :system (alist-get 'claude gptel-directives))

  (gptel-make-preset 'claude
    :description "claude code"
    :backend "cc"
    :model 'opusplan
    :system (alist-get 'claude gptel-directives)
    :parents '(default)
    :tools '())

  (gptel-make-preset 'codex
    :description "codex"
    :backend "codex"
    :model 'gpt-5-codex
    :system (alist-get 'default gptel-directives)
    :parents '(default))
  (gptel--apply-preset 'cob)
  (gptel--apply-preset 'codex)
  (gptel--apply-preset 'default)
  (gptel--apply-preset 'claude))

;;;###autoload
(defun +gptel (arg)
  (interactive "P")
  (cond
   ((region-active-p) (call-interactively #'gptel-rewrite))
   ((not arg) (call-interactively #'gptel))
   ((eq arg 7) (call-interactively #'gptel-menu))
   ((eq arg 8)
    (if (eq gptel-model 'gpt-4.1)
        (setq gptel-model 'claude-sonnet-4.5)
      (setq gptel-model 'gpt-4.1)))
   ((eq arg 90)
    (progn (funcall #'gptel-context-remove-all)
           (message "gptel context removed!")))
   ((eq arg 6) (call-interactively #'gptel-context-add))))

(defvar +llm-project-default-files '())
(make-variable-buffer-local '+llm-project-default-files)
(put '+llm-project-default-files 'safe-local-variable #'listp)
(setq! +llm-global-project-default-files
       '("CLAUDE.md" "AGENTS.md" "README.md" "README.org" ".github/copilot-instructions.md" "llm.txt"
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
         gptel-default-mode 'org-mode
         gptel-expert-commands t
         gptel-temperature 1.0
         gptel-org-branching-context t
         gptel-track-media t
         +gptel-disabled-tool-patterns
         '("^show_api_key$" "^capture_screenshot" "^guess_datetime"))

  ;; (defadvice! +gptel-cleanup-default-buffer (&rest args)
  ;;   :before #'gptel
  ;;   (+gptel-kill-default-buffer))
  :config
  (require 'gptel-context)
  (require 'pcre2el)
  (require 'gptel-gh)
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n")
  (set-popup-rule!
    (lambda (bname _action)
      (let ((b (get-buffer bname)))
        (and (null gptel-display-buffer-action)
             (or (buffer-local-value 'gptel-mode b)
                 (and (buffer-file-name b)
                      (string-match-p "^/User/.*/Dropbox/sync/gptel/gptel-"
                                      (buffer-file-name b)))))))
    :autosave t
    :select t
    :side 'right
    :size 0.35
    :quit t
    :ttl nil)

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
                         (+magit-wip-diff-n-min-buffer 5)
                         (+current-workspace-lints-buffer)))
        (setf (alist-get buf gptel-context nil 'remove) nil))
      
      ;; Re-add updated context
      (+gptel-context-add-buffer (+current-workspace-info-buffer))
      (+gptel-context-add-buffer (+visible-buffers-list-buffer))
      (+gptel-context-add-buffer (+magit-wip-diff-n-min-buffer 5))
      ;; ;; add visible buffers to context
      ;; ;; let llm decides based on the visible buffer list
      ;; (dolist (b (seq-filter
      ;;             (lambda (b)
      ;;               (and
      ;;                (not (string= (buffer-name b) "*Messages*"))
      ;;                (with-current-buffer b
      ;;                  (not gptel-mode))))
      ;;             (mapcar 'window-buffer (window-list))))
      ;;   (+gptel-context-add-buffer b))
      (when-let ((root (++workspace-current-project-root)))
        (dolist (f (+llm-get-project-default-files))
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
  (setq! gptel--gh-copilot-business
         (gptel-make-gh-copilot "cpb"
           :host "api.business.githubcopilot.com"
           :models gptel--gh-b-models
           :stream t))
  (setq! gptel-backend gptel--openrouter)
  (setq! gptel-backend gptel--codex)
  (setq! gptel-backend gptel--claude-code)
  ;; (setq! gptel-backend gptel--gh-copilot-local)
  (setq! gptel-backend gptel--gh-copilot-business)
  (setq! gptel-backend gptel--gh-copilot-individual)
  (setq! gptel-model 'gemini-2.5-pro)
  (setq! gptel-model 'claude-sonnet-4)
  (setq! gptel-model 'gpt-5-codex)
  (setq! gptel-model 'claude-sonnet-4.5)
  (setq! gptel-model 'gpt-4.1)
  (add-hook! 'gptel-post-response-functions '+gptel-save-buffer)
  (add-hook! 'gptel-post-response-functions #'my/gptel-remove-headings)
  ;; (add-hook! 'gptel-pre-response-hook 'my/claude-code-message-separator)
  (setq! gptel-log-level 'debug)
  (defun +gptel-toggle-debug ()
    (interactive)
    (if gptel-log-level
        (setq! gptel-log-level 'debug)
      (setq! gptel-log-level nil)))
  (setq! gptel-log-level 'nil)

  ;; (el-patch-defun gptel-context--insert-buffer-string (buffer contexts)
  ;;   "Insert at point a context string from all CONTEXTS in BUFFER."
  ;;   (let ((is-top-snippet t)
  ;;         (previous-line 1))
  ;;     (insert
  ;;      (el-patch-swap
  ;;        (format "In buffer `%s`:" (buffer-name buffer))
  ;;        (format "In buffer `%s`%s:"
  ;;                (buffer-name buffer)
  ;;                (if-let ((buf-file (buffer-file-name buffer)))
  ;;                    (format ", file `%s`" buf-file)
  ;;                  "")))
  ;;      "\n\n```" (gptel--strip-mode-suffix (buffer-local-value
  ;;                                           'major-mode buffer))
  ;;      "\n")
  ;;     (dolist (context contexts)
  ;;       (let* ((start (overlay-start context))
  ;;              (end (overlay-end context))
  ;;              content)
  ;;         (let (lineno column)
  ;;           (with-current-buffer buffer
  ;;             (without-restriction
  ;;               (setq lineno (line-number-at-pos start t)
  ;;                     column (save-excursion (goto-char start)
  ;;                                            (current-column))
  ;;                     content (buffer-substring-no-properties start end))))
  ;;           ;; We do not need to insert a line number indicator if we have two regions
  ;;           ;; on the same line, because the previous region should have already put the
  ;;           ;; indicator.
  ;;           (unless (= previous-line lineno)
  ;;             (unless (= lineno 1)
  ;;               (unless is-top-snippet
  ;;                 (insert "\n"))
  ;;               (insert (format "... (Line %d)\n" lineno))))
  ;;           (setq previous-line lineno)
  ;;           (unless (zerop column) (insert " ..."))
  ;;           (if is-top-snippet
  ;;               (setq is-top-snippet nil)
  ;;             (unless (= previous-line lineno) (insert "\n"))))
  ;;         (insert content)))
  ;;     (unless (>= (overlay-end (car (last contexts)))
  ;;                 (point-max))
  ;;       (insert "\n..."))
  ;;     (insert "\n```")))

  (defadvice! +gptel-mcp--activate-tools (_)
    :after #'gptel-mcp--activate-tools
    (+gptel-make-my-presets))

  (defadvice! +gptel-make-tool (&rest _args)
    :after #'gptel-make-tool
    (+gptel-make-my-presets))

  (add-hook! 'gptel-post-response-functions
    (defun +gptel-notify-done (&rest _args)
      (when (> (float-time (or (current-idle-time) 0)) 60)
        (pushover-send
         "GPTEL Done" "GPTEL Done" :sound "magic"))))

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
           (timeout-seconds 600)
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

  (after! pabbrev
    (add-hook! 'buffer-list-update-hook
      (defun +pabbrev-scavenge-all-visible-buffers-for-gptel ()
        (when (bound-and-true-p gptel-mode)
          (+pabbrev-scavenge-all-visible-buffers))))))

;;; mcp
(use-package! mcp
  :after gptel
  :config
  (require 'gptel-integrations)
  (require 'mcp-hub)
  (load! "gptel-tools.el")
  (mcp-hub-start-all-server
   (lambda ()
     (gptel-mcp-connect)
     (+gptel-make-my-presets)
     ;; at http://localhost:18684/mcp/v1/messages
     (mcp-server-lib-http-start :port 18684)


     (setq-default gptel--preset 'default)))
  (setq! mcp-log-level 'debug)
  (setq! mcp-log-level 'info)

  (defadvice! +mcp-make-text-tool (orig-fn name tool-name &optional asyncp)
    :around #'mcp-make-text-tool
    (plist-put (funcall orig-fn name tool-name asyncp) :include t)))

(use-package! mcp-server-lib
  :defer t
  :init
  (unless (file-exists-p
           (concat (expand-file-name user-emacs-directory)
                   "emacs-mcp-stdio.sh"))
    (mcp-server-lib-install))
  :config
  ;; (mcp-server-lib-http-stop)
  (comment
    (mcp-server-lib-stop))
  (unless (and (boundp 'mcp-server-lib--running) mcp-server-lib--running)
    (mcp-server-lib-start)))

(defvar simple-llm-req-p nil)

(defun simple-llm-req (prompt &rest args)
  (let ((simple-llm-req-p t)
        (gptel-backend (plist-get args :backend))
        (gptel-model (plist-get args :model))
        (gptel-temperature (clj/get args :temperature gptel-temperature))
        (gptel--system-message (clj/get args :system ""))
        (gptel-max-tokens (clj/get args :max-token gptel-max-tokens))
        (gptel-cache (clj/get args :cache t))
        (gptel--num-messages-to-send 1)
        (gptel-include-reasoning nil)
        (gptel-track-media nil)
        (gptel-use-context nil)
        (gptel-stream nil)
        (on-finish (clj/get args :cb 'clj/identity))
        (on-error (clj/get args :error 'clj/identity)))
    (gptel-request prompt
      :stream nil
      :callback
      (lambda (response info)
        (if response
            (funcall on-finish response)
          (funcall on-error info))))))

(defun simple-llm-req-sync (prompt &rest args)
  (await-callback
   (lambda (resolve reject)
     (simple-llm-req
      prompt
      :backend (plist-get args :backend)
      :model (plist-get args :model)
      :temperature (plist-get args :temperature)
      :system (plist-get args :system)
      :max-token (plist-get args :max-token)
      :cache (plist-get args :cache)
      :cb (lambda (response)
            (funcall resolve response))
      :error (lambda (error)
               (funcall reject error))))
   (or (plist-get args :timeout) 60)))

(defun get-gptel-org-title (&optional chat-content on-title on-error)
  (simple-llm-req
   (format "```\n%s```\n\nGenerate a file title for the above conversation with llm"
           (or chat-content
               (with-current-buffer (current-buffer) (buffer-string))))
   :backend gptel--openrouter
   :model 'google/gemini-2.5-flash
   :temperature 0.5
   :max-token 20
   :cb (or on-title 'print)
   :error (or on-error 'print)
   :system "You are an expert chat titling AI. Your sole purpose is to read the beginning of a chat conversation and generate a concise, descriptive title for it. This title will be used as a filename or an HTML page title.

RULES:
1.  Directly output the title text and NOTHING ELSE.
2.  Do NOT use quotation marks or any other formatting.
3.  Do NOT include prefixes like \"Title:\" or \"Chat about:\".
4.  Do NOT add any explanation or commentary.
5.  The title should be brief, typically 3-7 words.
6.  Capture the core subject or the user's primary intent from the provided text.

EXAMPLES:
- User asks for a Python function to sort a list -> Title: Python List Sorting Function
- User asks for ideas for a sci-fi story -> Title: Sci-Fi Story Ideas
- User asks \"What were the main causes of World War 1?\" -> Title: Main Causes of WWI

The user's chat will now follow. Generate the title."))

;;;###autoload
(defun +gptel-format-known-tools-to-markdown ()
  "Format gptel--known-tools to human-readable markdown and copy to kill-ring."
  (interactive)
  (let ((md-output (with-temp-buffer
                     (insert "# Available GPTEL Tools\n\n")
                     (dolist (category gptel--known-tools)
                       (let ((category-name (car category))
                             (tools (cdr category)))
                         (insert (format "## %s\n\n" category-name))
                         (dolist (tool-entry tools)
                           (let* ((tool-name (car tool-entry))
                                  (tool (cdr tool-entry))
                                  (description (gptel-tool-description tool))
                                  (args (gptel-tool-args tool)))
                             (insert (format "### %s\n\n" tool-name))
                             (when description
                               (insert (format "%s\n\n" description)))
                             (when args
                               (insert "**Parameters:**\n\n")
                               (dolist (arg args)
                                 (let ((arg-name (plist-get arg :name))
                                       (arg-type (plist-get arg :type))
                                       (arg-desc (plist-get arg :description))
                                       (arg-optional (plist-get arg :optional)))
                                   (insert (format "- `%s` (%s)%s: %s\n"
                                                   arg-name
                                                   arg-type
                                                   (if arg-optional " *optional*" "")
                                                   (or arg-desc "No description")))))))
                           (insert "\n"))))
                     (buffer-string))))
    (if (called-interactively-p 'any)
        (progn
          (kill-new md-output)
          (message "Formatted %d tool categories to kill-ring"
                   (length gptel--known-tools)))
      md-output)))

(comment
  (get-gptel-org-title)
  (simple-llm-req
   "hi"
   :backend gptel--openrouter
   :model 'google/gemini-2.5-flash
   :cb 'print
   :error 'print))

;;; lisp balancer
(setq llm-lisp-balancer-system-message
      (with-file-contents!
          (expand-file-name "~/Dropbox/sync/gptel-system-message/lisp-balancer.md")
        (buffer-string)))

(defun llm-balance-lisp-code--exrtract-md-fence (response)
  (let ((rst) (err))
    (when (string-match "^```\\(txt\\|[a-z]+\\)\n\\(\\(?:.\\|\n\\)*?\\)\n```" response)
      (let ((fence-lang (match-string 1 response))
            (fence-content (match-string 2 response)))
        (if (string= fence-lang "txt")
            (setq err fence-content)
          (setq rst fence-content))))
    (list :rst rst :err err)))

(defun llm-balance-lisp-code (code lang-mode &optional on-ok on-error)
  (let* ((async-p (and on-ok on-error))
         (lang (cond
                ((eq lang-mode 'emacs-lisp-mode) "elisp")
                ((eq lang-mode 'clojure-mode) "clojure")
                ((eq lang-mode 'clojurescript-mode) "clojure")
                ((eq lang-mode 'common-lisp-mode) "lisp")
                ((eq lang-mode 'scheme-mode) "scheme")
                ((eq lang-mode 'racket-mode) "racket")
                (t "lisp")))
         (f (if async-p #'simple-llm-req #'simple-llm-req-sync))
         (response
          (funcall
           f
           (format "```%s\n%s\n```" lang code)
           ;; :backend gptel--gh-copilot-business
           :backend gptel--gh-copilot-business
           :model 'gpt-4.1
           ;; :model 'gpt-4o
           ;; :model 'gpt-4o-mini
           ;; :model 'gpt-5-mini
           :temperature 0.5
           :system llm-lisp-balancer-system-message
           :timeout 60
           :cb (lambda (response)
                 (when on-ok
                   (funcall on-ok
                            (llm-balance-lisp-code--exrtract-md-fence response))))
           :error (or on-error 'print)))
         (rst)
         (err))
    (when (and (not async-p)
               (string-match "^```\\(txt\\|[a-z]+\\)\n\\(\\(?:.\\|\n\\)*?\\)\n```" response))
      (let ((fence-lang (match-string 1 response))
            (fence-content (match-string 2 response)))
        (if (string= fence-lang "txt")
            (setq err fence-content)
          (setq rst fence-content))
        (list :rst rst :err err)))))

;;;###autoload
(defun llm-compress-buffer-conversation (&optional buffer async)
  "Compress conversation in BUFFER (defaults to current buffer).
Save compressed output to kill-ring.
If ASYNC is non-nil, run asynchronously."
  (interactive (list (current-buffer) current-prefix-arg))
  (unless (buffer-live-p buffer)
    (user-error "Buffer %s is not live" buffer))
  
  (with-current-buffer buffer
    (unless gptel-mode
      (user-error "Buffer must be in gptel-mode"))
    
    (let* ((conversation (gptel--parse-buffer gptel-backend))
           (compression-prompt
            "You are a precision compression engine. Convert the entire prior conversation into a concise \"Context Memo\" we can reuse without exceeding the context window.

Hard rules:
- No speculation or invention. Use only information stated in the chat. Mark unknowns as \"UNKNOWN\".
- Normalize and deduplicate; collapse repeated ideas; keep canonical names/IDs/dates/URLs.
- Preserve only what helps continue the task.

Keep (in priority order):
1) User goals/intents & success criteria
2) Final decisions made + rationale in one short sentence each
3) Active constraints (APIs, stack, versions, budgets, deadlines, formats)
4) User preferences (style, tools, tone) that affect future replies
5) Open questions/blockers
6) Next actions/TODOs (who/what/when)
7) Key facts/figures/IDs/links/artifacts (filenames, endpoints, env vars)
8) Domain terms/acronyms defined in this chat

Drop:
- Pleasantries, verbose reasoning, dead ends unless they explain a decision.
- Long code; keep only names, signatures, or file paths unless a snippet is absolutely essential (<=10 lines)."))
      
      (if async
          (simple-llm-req
           (format "%s\n\nConversation to compress:\n%s"
                   compression-prompt
                   (prin1-to-string conversation))
           :backend gptel--gh-copilot-business
           :model 'gpt-4.1
           :temperature 0.3
           :cb (lambda (response)
                 (kill-new response)
                 (message "Compressed conversation saved to kill-ring"))
           :error (lambda (err)
                    (message "Compression failed: %S" err)))
        
        (let ((response
               (simple-llm-req-sync
                (format "%s\n\nConversation to compress:\n%s"
                        compression-prompt
                        (prin1-to-string conversation))
                :backend gptel--openrouter
                :model 'google/gemini-2.5-flash
                :temperature 0.3)))
          (kill-new (gptel--convert-markdown->org response))
          (message "Compressed conversation saved to kill-ring")
          response)))))

(comment
  (kill-new "abc")
  (let ((str) (gptel--convert-markdown->org (buffer-string)))
    str)
  (with-current-buffer (current-buffer)
    (gptel--parse-buffer gptel-backend)))

(use-package! gptel-magit
  :defer t
  :config
  (setq!
   gptel-magit-model 'gpt-4.1
   gptel-magit-backend gptel--gh-copilot-business
   gptel-magit-commit-prompt
   "You are an expert at writing Git commits compliant with @commitlint/config-conventional.

STRUCTURE:
<type>(<optional scope>): <description>

[optional body]

[optional footer(s)]

IMPORTANT REQUIRED RULES (errors):
1. type: MUST be one of: build, chore, ci, docs, feat, fix, perf, refactor, revert, style, test
2. type: MUST be lowercase
3. type: MUST NOT be empty
4. description: MUST NOT be empty
5. description: MUST start with lowercase (never sentence-case, start-case, pascal-case, or upper-case)
6. description: MUST NOT end with a period
7. header (type + scope + description): MUST be 100 characters or less
8. body: each line MUST be 100 characters or less
9. footer: each line MUST be 100 characters or less

WARNINGS (should follow):
- body: MUST have a leading blank line before it
- footer: MUST have a leading blank line before it

CONVENTIONS:
- Use imperative mood (\"add feature\" not \"added feature\")
- scope: optional, describes codebase section, e.g., fix(parser):
- body: provide additional context, wrap at 100 chars per line
- footer: use for BREAKING CHANGE or issue references

TYPE DEFINITIONS:
- build: Changes to build system or dependencies (example scopes: gulp, broccoli, npm)
- chore: Other changes that don't modify src or test files
- ci: Changes to CI configuration files and scripts (example scopes: Travis, Circle, BrowserStack, SauceLabs)
- docs: Documentation only changes
- feat: A new feature
- fix: A bug fix
- perf: A code change that improves performance
- refactor: A code change that neither fixes a bug nor adds a feature
- revert: Reverts a previous commit
- style: Changes that do not affect the meaning of the code (white-space, formatting, missing semi-colons, etc)
- test: Adding missing tests or correcting existing tests

BREAKING CHANGES:
- Mark breaking changes with ! before the colon: feat!: remove deprecated API
- OR include BREAKING CHANGE: in footer with description
- When both are used, they MUST be consistent (either both present or both absent)

EXAMPLES:
feat(api): add user authentication endpoint

fix: resolve memory leak in cache manager

docs(readme): update installation instructions

refactor(core): simplify error handling logic

This improves maintainability and reduces complexity.

feat!: remove support for Node 12

BREAKING CHANGE: Node 12 is no longer supported

fix(parser): handle edge case with empty strings

Previously, empty strings would cause a crash. This commit adds
proper validation and returns early with a default value.

Closes #123"))

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

(comment
  (progn
    (require 'acp)
    (setq! gptel--claude-code-acp
           (gptel-make-acp "claude-code-acp"
                           :command (executable-find "claude-code-acp")
                           :models '(claude-sonnet-4-5)
                           :host "claude code via acp"
                           :mcp-servers
                           '(((name . "emacs")
                              (command .
                                       "~/.emacs.d/.local/cache/emacs-mcp-stdio.sh")
                              (args . ["--init-function=elisp-dev-mcp-enable"
                                       "--stop-function=elisp-dev-mcp-disable"])
                              (env . [((name . "EMACS_MCP_DEBUG_LOG")
                                       (value . "/Users/yqrashawn/mcp-lib.log"))])))))
    (simple-llm-req
     "hi"
     :backend gptel--claude-code-acp
     :model 'claude-sonnet-4-5
     :cb 'message           
     :error 'message)
    nil))

(defadvice! +gptel-handle-invalid-tool-calls (orig-fn fsm)
  "Send error message to LLM when it calls non-existent tools."
  :around #'gptel--handle-tool-use
  (let* ((info (gptel-fsm-info fsm))
         (backend (plist-get info :backend))
         (tool-use (cl-remove-if (lambda (tc) (plist-get tc :result))
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
    (funcall orig-fn fsm)))


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

(comment
  (setq gptel--tmp-backend
        (gptel-make-openai "tmp"
          :host "localhost:11434"
          :endpoint "/v1/chat/completions"
          :stream t
          :key "no key"
          :models gptel--claude-code-models))

  (simple-llm-req
   "hi"
   :backend gptel--tmp-backend
   :model 'opusplan
   :cb (lambda (data) (message "DATA: %s" data)) 
   :error (lambda (err)
            (when (plist-p err)
              (message "ERROR: %s\nHTTP-STATUS: %s\nSTATUS: %s"
                       (plist-get err :error)
                       (plist-get err :http-status)
                       (plist-get err :status))))))

(setq!
 mcp-hub-servers
 `(
   ;; ("palywright" .
   ;;   (:command "npx"
   ;;     :args
   ;;     ("@playwright/mcp@latest"
   ;;       "--storage-state=/Users/yqrashawn/.cache/playwrite-mcp/storage.json")))

   ("context7" .
    (:command "bunx"
     :args
     ("@upstash/context7-mcp"
      "--api-key"
      ,+context-7-api-key)))

   ;; ("nextjs-devtools" .
   ;;   (:command "bunx"
   ;;     :args
   ;;     ("next-devtools-mcp@latest")))

   ("exa" .
    (:command "bun"
     :args ("run"
            ,(-> "~/Dropbox/sync/exa-mcp-server/.smithery/stdio/index.cjs"
                 file-truename))))

   ;; ("perplexity" .
   ;;  (:command "npx"
   ;;   :args ("-y" "perplexity-mcp")
   ;;   :env
   ;;   (:PERPLEXITY_API_KEY ,+perplexity-api-key
   ;;    :PERPLEXITY_TIMEOUT_MS "600000")))

   ;; ("jina_search" .
   ;;  (:url "http://localhost:18682/mcp"))

   ("jina_search" .
    (:command "npx"
     :args
     ("-y"
      "mcp-remote"
      "http://localhost:18682/mcp")))

   ("emacs" .
    ;; (:url "http://localhost:18684/mcp/v1/messages")
    (:command ,(concat
                (expand-file-name user-emacs-directory)
                "emacs-mcp-stdio.sh")
              ;; "/Users/yqrashawn/.emacs.d/.local/cache/emacs-mcp-stdio.sh"
              ;; :args ("--init-function=elisp-dev-mcp-enable"
              ;;        "--stop-function=elisp-dev-mcp-disable")
              ))

   ;; ("desktop-commander" . (:command "bunx"
   ;;                         :args ("@wonderwhy-er/desktop-commander")))
   ;; ("clojure-mcp-miniser" . (:command "clojure-mcp"
   ;;                           :args ("--port" "8002")))
   ;; ("fetch" .
   ;;  (:command "uvx" :args ("mcp-server-fetch")))

   ;; ("ramcp-macmini" .
   ;;  (:url "http://macmini.local:9897/api/mcp/sse"))

   ;; ("ramcp-local" .
   ;;  (:url "http://localhost:7999/api/mcp/sse"))

   ;; ("mcp-filesystem-server" .
   ;;  (:command "mcp-filesystem-server"
   ;;   :args ("/Users/yqrashawn/")))

   ;; ("markitdown" .
   ;;   (:command "uvx"
   ;;     :args ("markitdown-mcp")))

   ;; ("figma" .
   ;;   (:command "npx"
   ;;     :args ("-y"
   ;;             "figma-developer-mcp"
   ;;             ,(concat "--figma-api-key=" +figma-access-token)
   ;;             "--stdio")))
   ))

(setq! gptel-log-level 'debug)
(setq! gptel-log-level nil)
(setq! mcp-log-level 'debug)
(setq! mcp-log-level 'info)
(setq! mcp-server-lib-log-io t)
(setq! mcp-server-lib-log-io nil)

(after! docker
  (when (-> "hostname"
            shell-command-to-string
            s-trim
            (string= "studio.local"))
    (setenv "DOCKER_HOST" "tcp://macmini.local:2375")))

(use-package! agent-shell-sidebar
  :after agent-shell)
(use-package! agent-shell-manager
  :after agent-shell)

(comment
  (simple-llm-req-sync
   "hi"
   :backend gptel--tmp))

(use-package claude-code
  :defer t
  ;; :bind-keymap
  ;; ("C-c c" . claude-code-command-map)
  ;; or your preferred key
  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  ;; :bind
  ;; (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode))
  :init
  (setq! claude-code-display-window-fn #'display-buffer-same-window)
  :config
  ;; optional IDE integration with Monet
  (add-hook! 'claude-code-process-environment-functions #'monet-start-server-function)
  
  (monet-mode 1)
  (claude-code-mode))
