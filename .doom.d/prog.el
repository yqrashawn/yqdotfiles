;;; prog.el -*- lexical-binding: t; -*-

(setq! projectile-project-search-path '("~/workspace/office" "~/workspace/home" "~/workspace/third"))

(after! dash-docs
  (setq! dash-docs-docsets-path
         (let ((original-dash-path (expand-file-name "~/Library/Application Support/Dash/DocSets")))
           (if (and (string-equal system-type 'darwin)
                    (file-directory-p original-dash-path))
               original-dash-path
             dash-docs))))

(use-package! copy-as-format :defer t)
(use-package! separedit :defer t)

;; (after! format-all
;;   ;; don't use shfmt for zsh
;;   (defadvice! +format-all--probe (orig-fn &rest args)
;;     :around #'format-all--probe
;;     (if (s-ends-with? ".zsh" buffer-file-name)
;;         '(nil nil)
;;       (funcall orig-fn)))

;;   ;; (define-format-all-formatter zprint
;;   ;;   (:executable "zprint")
;;   ;;   (:install)
;;   ;;   (:modes clojure-mode clojurec-mode clojurescript-mode)
;;   ;;   (:format (format-all--buffer-easy executable "-w")))
;;   (setq +format-on-save-enabled-modes
;;         '(not
;;           emacs-lisp-mode               ; elisp's mechanisms are good enough
;;           sql-mode                      ; sqlformat is currently broken
;;           tex-mode                      ; latexindent is broken
;;           nix-mode
;;           go-mode
;;           latex-mode
;;           org-msg-edit-mode
;;           rjsx-mode
;;           js2-mode
;;           js-mode
;;           js3-mode
;;           clojurec-mode
;;           clojurescript-mode
;;           clojure-mode))
;;   ;; (defadvice! +format-all--formatter-executable (orig-fn formatter)
;;   ;;   :around #'format-all--formatter-executable
;;   ;;   (let* ((home (concat (getenv "HOME") "/"))
;;   ;;          (root (doom-project-root))
;;   ;;          (root (if (or (not root) (string= home root)) (expand-file-name "~/.config/yarn/global/") root)))
;;   ;;     (if (file-executable-p (concat root "node_modules/" ".bin/" (symbol-name formatter)))
;;   ;;         (concat root "node_modules/" ".bin/" (symbol-name formatter))
;;   ;;       (apply orig-fn formatter))))

;;   ;; run prettier after lsp format (eslint)
;;   ;; (defadvice! ++format/region-or-buffer (orig-fn)
;;   ;;   :around #'+format/region-or-buffer
;;   ;;   (ignore-errors (call-interactively orig-fn))
;;   ;;   (when (memq major-mode '(rjsx-mode js-mode js2-mode typescript-mode))
;;   ;;    (let ((+format-with-lsp nil))
;;   ;;      (ignore-errors (call-interactively orig-fn)))))
;;   )

(use-package! yaml-imenu
  :hook (yaml-mode . yaml-imenu-enable)
  :init
  (defun which-function-from-imenu-index ()
    "Call the imenu-index part in `which-function'.

It is a fallback for when which-func-functions and `add-log-current-defun' return nil."
    (let (which-func-functions)
      (cl-letf (((symbol-function 'add-log-current-defun)
                 (lambda () nil)))
        (which-function))))

  ;; `add-log-current-defun' returns a not so meaningful result in some
  ;; major modes when the default `add-log-current-defun-function'
  ;; happens to match a random line that is not really a function
  ;; definition.  It is often much more desirable to find a function
  ;; name from an imenu index in those modes.  Results are also used by
  ;; `which-function-mode'.
  (defun enable-add-log-current-defun-using-which-function ()
    (setq-local add-log-current-defun-function 'which-function-from-imenu-index))

  (add-hook! 'yaml-mode-hook 'enable-add-log-current-defun-using-which-function))

;; imenu sidebar
;; (use-package! side-hustle
;;   :defer t
;;   :config
;;   (defadvice! ++fold/toggle (orig-fn)
;;     :around #'+fold/toggle
;;     (if (eq major-mode 'side-hustle-mode)
;;         (call-interactively #'side-hustle-show-item)
;;       (call-interactively orig-fn))))

;; (defadvice! ++syntax-init-popups-h (orig-fn)
;;   :around #'+syntax-init-popups-h
;;   (unless (and (bound-and-true-p lsp-ui-mode)
;;                lsp-ui-sideline-enable)
;;     (flycheck-popup-tip-mode +1)))

;; (use-package! evil-textobj-tree-sitter
;;   :after evil)

(use-package! smerge-mode :defer t)

(after! flycheck
  (setq! flycheck-global-modes
         '(not
           smerge-mode
           elfeed-search-mode
           outline-mode
           diff-mode
           shell-mode
           eshell-mode
           vterm-mode
           notmuch-search-mode))
  (when global-flycheck-mode
    (global-flycheck-mode -1))
  (add-hook! 'prog-mode-hook (cmd! (flycheck-mode 1))))

(defun ar/ediff-dir-content-size ()
  "Diff all subdirectories (sizes only) in two directories."
  (interactive)
  (require 'f)
  (let* ((dir1-path (read-directory-name "Dir 1: "))
         (dir2-path (read-directory-name "Dir 2: "))
         (buf1 (get-buffer-create (format "*Dir 1 (%s)*" (f-base dir1-path))))
         (buf2 (get-buffer-create (format "*Dir 2 (%s)*" (f-base dir2-path)))))
    (with-current-buffer buf1
      (erase-buffer))
    (with-current-buffer buf2
      (erase-buffer))
    (shell-command (format "cd %s; find . -type d | sort | du -h" dir1-path) buf1)
    (shell-command (format "cd %s; find . -type d | sort | du -h" dir2-path) buf2)
    (ediff-buffers buf1 buf2)))

;; tmp https://github.com/hlissner/doom-emacs/pull/5401
(defadvice! ++fold--ts-fold-p (orig)
  :around #'+fold--ts-fold-p
  nil)

(use-package! eshell-follow
  :after eshell
  :after-call eshell-follow-global-mode
  :config
  (eshell-follow-global-mode t))

(use-package! eat
  :commands (eat)
  :init
  (setq! eat-kill-buffer-on-exit t
         eat-very-visible-cursor-type '(t nil nil)
         eat-enable-yank-to-terminal t
         eat-enable-blinking-text nil
         process-adaptive-read-buffering nil
         read-process-output-max (* 4 1024 1024))
  :config
  (defun +eat-deleted-window-after-kill-buffer ()
    (if (featurep 'evil) (evil-window-delete) (delete-window)))
  (defun +eat-setup ()
    (add-hook! 'kill-buffer-hook :local '+eat-deleted-window-after-kill-buffer))
  (add-hook! 'eat-mode-hook '+eat-setup)
  (pushnew! evil-emacs-state-modes 'eat-mode))

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook ((go-mode
          rjsx-mode
          js2-mode
          js-ts-mode
          typescript-mode
          tsx-ts-mode
          jtsx-tsx-mode
          jtsx-jsx-mode
          clojure-mode
          clojurescript-mode
          clojurec-mode) . copilot-mode)
  :init
  (setq! copilot-max-char -1
         copilot-idle-delay 10
         copilot-indent-offset-warning-disable t
         ;; copilot-lsp-settings
         ;; '(:github (:copilot (:selectedCompletionModel "gpt-4o-copilot")))
         ;; copilot-lsp-settings
         ;; nil
         ;; copilot-server-executable (executable-find "copilot-language-server")
         )
  :config
  (pushnew! copilot-indentation-alist
            '(jtsx-tsx-mode
              jtsx-jsx-mode
              tsx-ts-mode typescript-ts-mode-indent-offset)
            '(typescript-ts-mode typescript-ts-mode-indent-offset)))

(use-package! copilot-chat
  :defer t
  :init
  (setq!
   copilot-chat-model "gemini-2.5-pro"
   ;; copilot-chat-model "o3-mini"
   copilot-chat-frontend 'org)
  (add-hook! '(copilot-chat-mode-hook copilot-chat-prompt-mode-hook)
    (defun +turn-off-languagetool-for-copilot-chat-buffers ()
      (languagetool-server-mode -1)))
  :config
  (pushnew! doom-unreal-buffer-functions
            (lambda (buf) (with-current-buffer buf copilot-chat-org-poly-mode)))
  (pushnew! doom-unreal-buffer-functions
            (lambda (buf) (with-current-buffer buf copilot-chat-prompt-mode)))
  (defadvice! +copilot-chat--auth ()
    :before #'copilot-chat--auth
    ;; it's possible that the token is available but do not provide copilot chat
    (unless (alist-get 'expires_at
                       (copilot-chat-connection-token copilot-chat--connection))
      (copilot-chat-clear-auth-cache)))

  (defadvice! +copilot-chat--display (instance)
    :after #'copilot-chat--display
    (with-current-buffer (copilot-chat--get-buffer (copilot-chat--current-instance))
      (with-current-buffer (pm-get-buffer-of-mode 'copilot-chat-org-prompt-mode)
        (setq-local +word-wrap-extra-indent 2))))

  (require 'magit)
  (defadvice! +copilot-chat-prompt-send ()
    :before #'copilot-chat-prompt-send
    (let ((i (copilot-chat--current-instance)))

      (copilot-chat--add-buffer i (+magit-wip-diff-n-min-buffer 5))

      (dolist (b (mapcar 'window-buffer (window-list)))
        (copilot-chat--add-buffer i b))

      (dolist (f +llm-project-default-files)
        (when-let ((b (get-file-buffer (file-truename (format "%s%s" root f)))))
          (copilot-chat--add-buffer i b)))

      (dolist (b (+magit-wip-buffer-changed-within-n-min 5))
        (with-current-buffer b
          (copilot-chat--add-buffer i b))))))

;; (use-package! ollama
;;   :defer t
;;   :init
;;   (setq! ollama:model "phind-codellama"))

(use-package! jarchive
  :hook (doom-after-init . jarchive-setup))

(after! prog-mode
  (global-corfu-mode 1))

(use-package! imake :defer t)
(set-file-template! "\.orun$" :trigger "__orun" :mode 'emacs-lisp-mode :project t)

(use-package! verb
  :defer t
  :init
  (set-popup-rule! "^\\*HTTP Response.*" :side 'right :size 0.4 :vslot 97 :quit t))

(use-package! leetcode
  :defer t
  :init
  (setq! leetcode-prefer-language "javascript"
         leetcode-save-solutions t))

(defvar minuet-openai-compatible-options
  `(:end-point ,(concat +openrouter-url "/chat/completions")
    :api-key ,(cl-constantly +openrouter-api-key)
    ;; :model "qwen/qwen-2.5-coder-32b-instruct"
    :model "google/gemini-2.0-flash-001"
    :system
    (:template minuet-default-system-template
     :prompt minuet-default-prompt
     :guidelines minuet-default-guidelines
     :n-completions-template minuet-default-n-completion-template)
    :fewshots minuet-default-fewshots
    :chat-input
    (:template minuet-default-chat-input-template
     :language-and-tab minuet--default-chat-input-language-and-tab-function
     :context-before-cursor minuet--default-chat-input-before-cursor-function
     :context-after-cursor minuet--default-chat-input-after-cursor-function)
    :optional nil)
  "Config options for Minuet OpenAI compatible provider.")

(use-package! minuet
  :defer t
  :init
  (setq! minuet-context-window 512)
  :config
  (setq! minuet-provider 'openai-compatible))

;;;###autoload
(defun +ejc-capf-setup ()
  "Add `+ejc-completion-at-point' to `completion-at-point-functions'."
  (add-hook 'completion-at-point-functions #'+ejc-completion-at-point nil t))

(defun +ejc-completion-at-point ()
  "Completion-at-point function for `ejc-sql-mode`.
This can be added to `completion-at-point-functions`."
  (when (bound-and-true-p ejc-sql-mode)
    (let ((prefix-info (ejc-company-backend 'prefix)))
      (when prefix-info
        (let* ((prefix (if (consp prefix-info) (car prefix-info) prefix-info))
               (beg (- (point) (length prefix)))
               (end (point))
               (candidates (ejc-company-backend 'candidates prefix)))
          (when candidates
            (list beg end candidates
                  :annotation-function
                  (lambda (c) (ejc-company-backend 'annotation c))
                  :company-doc-buffer
                  (lambda (c) (ejc-company-backend 'doc-buffer c)))))))))

(use-package! ejc-sql
  :defer t
  :init
  (setq! clomacs-httpd-default-port 8595
         clomacs-allow-other-repl t
         ejc-result-table-impl 'orgtbl-mode)
  (add-hook! 'ejc-sql-minor-mode-hook (lambda () (ejc-eldoc-setup)))
  (add-hook! 'ejc-sql-minor-mode-hook #'+ejc-capf-setup)
  :config
  (ejc-set-column-width-limit nil)
  (defadvice! +ejc-eval-org-snippet (orig-fn &optional orig-fun body params)
    :around #'ejc-eval-org-snippet
    (if (and params (assq :engine params))
        (funcall orig-fun body params)
      (funcall orig-fn orig-fun body params))))
