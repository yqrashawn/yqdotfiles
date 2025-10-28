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
        (progn
          (get-gptel-org-title
           (buffer-string)
           (lambda (title)
             (let* ((new-title (string-replace "\n" "_" title))
                    (new-title (string-replace "```" "" new-title)))
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
                   (insert "\n")
                   (write-file
                    (expand-file-name
                     (format
                      "%s-%s-%s.org"
                      (format-time-string "%d")
                      (format-time-string "%H_%M")
                      new-title)
                     dir))))))
           (lambda (e) (user-error "Error setting gptel org title: %s" e)))
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
    :backend "CopilotI"
    :model 'claude-sonnet-4.5
    ;; :system (alist-get 'default gptel-directives)
    :system (alist-get 'claude gptel-directives)
    :temperature 0.8
    :tools
    (cl-mapcan
     (lambda (x)
       (let ((category (car x)))
         (seq-map 'car (alist-get category gptel--known-tools))))
     gptel--known-tools))

  (gptel-make-preset 'cob
    :description "preset"
    :backend "CopilotB"
    :parents '(default)
    :model 'claude-3.7-sonnet
    :system (alist-get 'claude gptel-directives))

  (gptel-make-preset 'claude
    :description "claude code"
    :backend "CCode"
    :model 'sonnet
    :system (alist-get 'claude gptel-directives)
    :parents '(default)
    :tools '())
  (gptel--apply-preset 'claude)
  (gptel--apply-preset 'cob)
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
           (buf-file (or (buffer-file-name buffer)
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

  (defadvice! +before-gptel-make-fsm (&optional args)
    :before #'gptel-send
    (unless simple-llm-req-p
      (require 'gptel-context)
      ;; Remove only the specific context buffers we manage, not all context
      (dolist (buf (list (+current-workspace-info-buffer)
                         (+visible-buffers-list-buffer)
                         (+magit-wip-diff-n-min-buffer 5)))
        (setf (alist-get buf gptel-context nil 'remove) nil))
      
      ;; Re-add updated context
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
          (when-let ((b (get-file-buffer (file-truename (format "%s/%s" root f)))))
            (+gptel-context-add-buffer b))))))

  (setq! gptel--openrouter
         (gptel-make-openai "OpenRouter"
           :host "openrouter.ai"
           :endpoint "/api/v1/chat/completions"
           :stream t
           :key +openrouter-api-key
           :models gptel--openrouter-models))
  ;; self host claude code
  (setq! gptel--claude-code
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
  (setq! gptel-backend gptel--claude-code)
  (setq! gptel-backend gptel--gh-copilot-local)
  (setq! gptel-backend gptel--gh-copilot-business)
  (setq! gptel-backend gptel--gh-copilot-individual)
  (setq! gptel-model 'gemini-2.5-pro)
  (setq! gptel-model 'claude-sonnet-4)
  (setq! gptel-model 'gpt-5-codex)
  (setq! gptel-model 'gpt-4.1)
  (setq! gptel-model 'claude-sonnet-4.5)
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

  (defadvice! +gptel-make-tool (&rest args)
    :after #'gptel-make-tool
    (+gptel-make-my-presets)))

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

     (setq-default gptel--preset 'default)))
  (setq! mcp-log-level 'debug)
  (setq! mcp-log-level 'info))

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
      :callback (lambda (response info)
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
          (setq rst fence-content))
        (list :rst rst :err err)))))

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
           :backend gptel--gh-copilot-business
           ;; :backend gptel--gh-copilot-individual
           :model 'gpt-4.1
           ;; :model 'gpt-4o
           ;; :model 'gpt-4o-mini
           ;; :model 'gpt-5-mini
           :temperature 0.5
           :system llm-lisp-balancer-system-message
           :timeout 60
           :cb (lambda (response)
                 (when on-ok
                   (on-ok (llm-balance-lisp-code--exrtract-md-fence response))))
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

(comment
  (llm-balance-lisp-code
   (with-file-contents!
       (expand-file-name "~/Downloads/unbalancedexample")
     (buffer-string))
   'clojure-mode))
