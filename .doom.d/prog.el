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
  :disabled
  :commands (eat)
  :init
  (setq! eat-kill-buffer-on-exit t
         eat-very-visible-cursor-type '(t nil nil)
         eat-term-terminfo-directory (expand-file-name "~/.emacs.d/.local/straight/repos/eat/terminfo")
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
          typescript-mode
          tsx-ts-mode
          clojure-mode
          clojurescript-mode
          clojurec-mode) . copilot-mode)
  :init
  (setq! copilot-max-char -1
         copilot-idle-delay 10
         copilot-indent-offset-warning-disable t
         ;; copilot-server-executable (executable-find "copilot-language-server")
         )
  :config
  (pushnew! copilot-indentation-alist
            '(tsx-ts-mode typescript-ts-mode-indent-offset)
            '(typescript-ts-mode typescript-ts-mode-indent-offset)))

(defun +magit-wip-diff-n-min-buffer (n)
  (let ((b (get-buffer-create (format! "*changes-with-%d-minutes*" n))))
    (with-current-buffer b
      (erase-buffer)
      (insert
       (concat
        (format "These are diffs of changes I made within %d minutes\n\n" n)
        (shell-command-to-string
         (format!
          "git reflog --since=\"30 minutes ago\" --oneline -p refs/wip/wtree/refs/heads/%s"
          (magit-get-current-branch))))))
    b))

(defun +project-files-buffers (file-list)
  "Return a list of buffers for FILE-LIST, where FILE-LIST is a list of relative file paths.
Each file is opened (if not already) with `find-file-noselect` relative to
 the current project root."
  (let ((project-root (doom-project-root))
        buffers)
    (setq buffers (mapcar (lambda (file)
                            (find-file-noselect (expand-file-name file project-root)))
                          file-list))
    buffers))

(defun +magit-wip-buffer-changed-within-n-min (n)
  (when (magit-git-repo-p (doom-project-root))
    (thread-last
      (shell-command-to-string

       (format!
        "git reflog --since=\"%d minutes ago\" --name-only --pretty=format: refs/wip/wtree/refs/heads/%s | grep -v '^$' | sort -u"
        n
        (magit-get-current-branch)))
      s-lines
      (seq-filter (lambda (s) (not (string-empty-p s))))
      +project-files-buffers)))

(use-package! copilot-chat
  :defer t
  :init
  (setq!
   copilot-chat-model "claude-3.7-sonnet-thought"
   ;; copilot-chat-model "o3-mini"
   copilot-chat-frontend 'org)
  (add-hook! '(copilot-chat-mode-hook copilot-chat-prompt-mode-hook)
    (defun +turn-off-languagetool-for-copilot-chat-buffers ()
      (languagetool-server-mode -1)))
  (set-popup-rules!
    '(;; ("^\\*Copilot Chat "
      ;;  :slot 89
      ;;  :side right
      ;;  :width 0.38
      ;;  :select t
      ;;  :ttl nil
      ;;  :quit t)
      ("^\\*Copilot-chat-list\\*$"
       :slot 10
       :side bottom
       :height 0.2
       :select nil
       :ttl nil
       :quit t)))
  (defadvice! +copilot-chat-display (orig-fn)
    :around #'copilot-chat-display
    (copilot-chat-list-clear-buffers)
    (let* ((region-str (and (region-active-p)
                            (buffer-substring-no-properties
                             (region-beginning)
                             (region-end)))))
      (funcall orig-fn)
      ;; (when region-str
      ;;   (with-current-buffer (copilot-chat--get-buffer)
      ;;     (goto-char (point-max))
      ;;     (insert region-str)
      ;;     (insert "\n")
      ;;     (goto-char (point-max))))
      (copilot-chat-add-current-buffer)))
  (defvar +copilot-chat-project-default-files '())
  :config
  (require 'magit)
  (defadvice! +copilot-chat-prompt-send (&rest args)
    :before #'copilot-chat-prompt-send
    (with-current-buffer (+magit-wip-diff-n-min-buffer 5)
      (copilot-chat-add-current-buffer))
    (copilot-chat-add-buffers-in-current-window)
    (dolist (b +copilot-chat-project-default-files)
      (with-current-buffer b
        (copilot-chat-add-current-buffer)))
    (dolist (b (+magit-wip-buffer-changed-within-n-min 5))
      (with-current-buffer b
        (copilot-chat-add-current-buffer)))))

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
