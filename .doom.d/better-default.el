;;; better-defaults.el -*- lexical-binding: t; -*-

(pushnew! global-hl-line-modes 'dired-mode)
(delq! 'prog-mode global-hl-line-modes)
(delq! 'text-mode global-hl-line-modes)
(setq! kmacro-ring-max 8
       save-silently t
       echo-keystrokes 1e-6
       split-window-keep-point t
       recentf-keep '(recentf-keep-default-predicate tramp-tramp-file-p)
       recentf-max-saved-items 2000
       require-final-newline nil
       mode-require-final-newline nil
       auto-window-vscroll nil
       confirm-kill-processes nil

       ;; scroll
       hscroll-margin 5
       hscroll-step 0
       ;; Emacs spends too much effort recentering the screen if you scroll the
       ;; cursor more than N lines past window edges (where N is the settings of
       ;; `scroll-conservatively'). This is especially slow in larger files
       ;; during large-scale scrolling commands. If kept over 100, the window is
       ;; never automatically recentered.
       scroll-conservatively 0
       scroll-margin 0
       scroll-preserve-screen-position nil
       ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
       ;; for tall lines.
       auto-window-vscroll nil
       ;; mouse
       mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
       mouse-wheel-scroll-amount-horizontal 2
       confirm-kill-emacs nil
       url-proxy-services
       '(("http" . "127.0.0.1:6152")
         ("https" . "127.0.0.1:6153"))
       url-proxy-services nil)

(pushnew! auto-mode-alist '("\\.gitconfig.*\\'" . gitconfig-mode))
(pushnew! auto-mode-alist '("\\.gitignore.*\\'" . gitignore-mode))
(pushnew! auto-mode-alist '("\\.git/info/exclude\\'" . gitignore-mode))

(add-hook! 'delete-terminal-functions (lambda (&rest a) (recentf-save-list)))

(use-package! git-link
  :commands (git-link git-link-commit git-link-homepage)
  :init
  (setq! git-link-use-commit t))

(use-package! rg
  :commands (rg rg-literal rg-dwim rg-project))

(setq! rotate-text-words
       '(("with" "height")
         ("left" "right" "top" "bottom")
         ("next" "previous")
         ("enabled" "disabled")
         ("enable" "disable")
         ("max" "min")
         ("bind" "unbind")
         ("local" "global")
         ("increment" "decrement")
         ("yes" "no")
         ("true" "false")
         ("add" "delete" "create" "remove" "get")
         ("safe" "unsafe" "danger")))

(use-package! string-inflection
  :commands (string-inflection-all-cycle))

;; TODO: check autoinsert and doom templates
;; maybe use yasnippet with it

(use-package side-notes :defer t :init (setq! side-notes-file "notes.side.org"))

(use-package explain-pause-mode :defer t)

(after! with-editor (shell-command-with-editor-mode))

(use-package! help-fns+ :defer t :commands (describe-keymap))

(use-package! outline
  :hook (prog-mode . outline-minor-mode)
  :hook (text-mode . outline-minor-mode)
  :hook (conf-mode . outline-minor-mode)
  :init
  (defun +outline-chomp (str)
    "Chomp leading and trailing whitespace from STR."
    (save-excursion
      (save-match-data
        (while (string-match
                "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                str)
          (setq str (replace-match "" t t str)))
        str)))
  :config
  (defun +outline-minor-mode-disable-evil-tab ()
    (define-key! evil-motion-state-map "TAB" nil))
  (defun +outline-minor-mode-setup-regexp ()
    (if (or (and (boundp 'lispy-mode) lispy-mode) (and (boundp 'lispyville-mode) lispyville-mode))
        (setq-local outline-regexp +emacs-lisp-outline-regexp)
      (progn
        (setq-local +outline-regexp-start (+outline-chomp (or comment-start "#")))
        (setq-local +outline-regexp-body (concat "\\(\\(" +outline-regexp-start "\\)" "+\\|" "\s?\\(#\\|;\\|\*\\)+" "\\)"))
        (make-local-variable 'outline-regexp)
        (setq outline-regexp (concat "[ \t]*" +outline-regexp-start +outline-regexp-body (+outline-chomp comment-end) " [^ \t\n]")))))
  (add-hook! outline-minor-mode '+outline-minor-mode-setup-regexp '+outline-minor-mode-disable-evil-tab))

(defadvice! +doom/switch-to-scratch-buffer (orig-fn &optional arg project-p)
  :around #'doom/switch-to-scratch-buffer
  (apply orig-fn (not arg) project-p))

(defvar +word-wrap-disabled-modes
  '(fundamental-mode so-long-mode)
  "Major-modes where `+global-word-wrap-mode' should not enable
`+word-wrap-mode'.")
(+global-word-wrap-mode +1)
