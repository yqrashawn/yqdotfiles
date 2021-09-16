;;; better-defaults.el -*- lexical-binding: t; -*-

(pushnew! global-hl-line-modes 'dired-mode)
(delq! 'prog-mode global-hl-line-modes)
(setq! kmacro-ring-max 8
       save-interprogram-paste-before-kill nil
       save-silently t
       echo-keystrokes 1e-6
       split-window-keep-point t
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
       url-proxy-services nil
       blink-matching-paren t
       ;; blink-matching-paren 'jump
       blink-matching--overlay (let ((ol (make-overlay (point) (point) nil t)))
                                 (overlay-put ol 'face 'custom-invalid)
                                 (delete-overlay ol)
                                 ol)
       ispell-dictionary "en_US"
       ispell-personal-dictionary "~/.config/personal_dict")

(after! recentf
  (setq! recentf-keep '(recentf-keep-default-predicate tramp-tramp-file-p)
         recentf-max-saved-items 2000))

(pushnew! auto-mode-alist '("\\.gitconfig.*\\'" . gitconfig-mode))
(pushnew! auto-mode-alist '("\\.gitignore.*\\'" . gitignore-mode))
(pushnew! auto-mode-alist '("\\.git/info/exclude\\'" . gitignore-mode))

(add-hook! 'delete-terminal-functions (lambda (&rest a) (recentf-save-list)))

(use-package! git-link
  :commands (git-link git-link-commit git-link-homepage)
  :init
  (setq! git-link-use-commit t))

(use-package! rg
  :commands (rg rg-literal rg-dwim rg-project)
  :config
  (pushnew! rg-custom-type-aliases '("js" . "*.js *.jsx *.ts *.tsx *.mjs *.cjs"))
  (defadvice! +rg (&rest _)
    :after #'rg (switch-to-buffer-other-window "*rg*")))

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
    (unless (or (eq major-mode 'org-mode) (derived-mode-p 'org-mode))
      (if (or (and (boundp 'lispy-mode) lispy-mode) (and (boundp 'lispyville-mode) lispyville-mode))
          (setq-local outline-regexp +emacs-lisp-outline-regexp)
        (progn
          (setq-local +outline-regexp-start (+outline-chomp (or comment-start "#")))
          (setq-local +outline-regexp-body (concat "\\(\\(" +outline-regexp-start "\\)" "+\\|" "\s?\\(#\\|;\\|\*\\)+" "\\)"))
          (make-local-variable 'outline-regexp)
          (setq outline-regexp (concat "[ \t]*" +outline-regexp-start +outline-regexp-body (+outline-chomp comment-end) " [^ \t\n]"))))))
  (add-hook! outline-minor-mode '+outline-minor-mode-setup-regexp '+outline-minor-mode-disable-evil-tab))

(defadvice! +doom/switch-to-scratch-buffer (orig-fn &optional arg project-p)
  :around #'doom/switch-to-scratch-buffer
  (apply orig-fn (not arg) project-p))


(setq-default +word-wrap-disabled-modes '(fundamental-mode so-long-mode prog-mode))

(setq-default +word-wrap-visual-modes '(org-mode))

(setq-default +word-wrap-text-modes
  '(text-mode markdown-mode markdown-view-mode gfm-mode gfm-view-mode rst-mode
    latex-mode LaTeX-mode))

(+global-word-wrap-mode +1)


(set-popup-rules!
  '(("^\\*[Hh]elp" :slot 2 :side right :vslot -8 :size 0.35 :select t :quit current)
    ("^\\*info\\*$" :slot 2 :vslot 2 :side right :size 0.45 :select t :quit nil)
    ;; ("^\\*eww\\*$" :slot 2 :vslot 2 :side right :size 0.45 :select t :quit nil)
    ("^\\*Messages\\*$" :vslot -2 :size 0.5 :autosave t :quit t :ttl nil)
    ("^\\*Completions" :ignore t)
    ("^\\*Local variables\\*$" :vslot -1 :slot 1 :size +popup-shrink-to-fit)
    ("^\\*\\(?:[Cc]ompil\\(?:ation\\|e-Log\\)\\|Messages\\)" :vslot -2 :size 0.3 :autosave t :quit t :ttl nil)
    ("^\\*\\(?:doom \\|Pp E\\)"           ; transient buffers (no interaction required)
     :vslot -3 :size +popup-shrink-to-fit :autosave t :select ignore :quit t :ttl 0)
    ("^\\*doom:"                         ; editing buffers (interaction required)
     :vslot -4 :size 0.35 :autosave t :select t :modeline t :quit nil :ttl t)
    ("^\\*doom:\\(?:v?term\\|e?shell\\)-popup" ; editing buffers (interaction required)
     :vslot -5 :size 0.35 :select t :modeline nil :quit nil :ttl nil)
    ("^\\*\\(?:Wo\\)?Man " :vslot -6 :size 0.45 :select t :quit t :ttl 0)
    ("^\\*Calc" :vslot -7 :side bottom :size 0.4 :select t :quit nil :ttl 0)
    ("^\\*Customize" :slot 2 :side right :size 0.5 :select t :quit nil)
    ("^ \\*undo-tree\\*" :slot 2 :side left :size 20 :select t :quit t)
    ("^\\*Apropos" :slot 2 :vslot -8 :size 0.35 :select t)))

(use-package! ix :commands (ix))

;; (use-package! fancy-dabbre
;;   :hook (doom-first-input . global-fancy-dabbrev-mode)
;;   :init
;;   (setq! fancy-dabbrev-preview-delay 0
;;          fancy-dabbrev-preview-context 'everywhere
;;          fancy-dabbrev-expansion-on-preview-only t))

(after! eww
  (add-hook! 'eww-after-render-hook #'eww-readable))

(symbol-name major-mode)

(use-package! thing-edit :defer t)

(use-package! wucuo
  :defer t
  :hook (prod-mode . wucuo-start))

(defadvice! +kill-buffer (orig arg)
  #'kill-buffer
  (when (or (memq major-mode '(jest-mode comint-mode))
            (derived-mode-p 'comint-mode))
    (ignore-errors (comint-interrupt-subjob)))
  (apply orig arg))

;; these fns are used for impl things that triggerd or passed under double C-g/ESC
(defun +doom/escape-just-called-cancel ()
  (setq +doom/escape-just-called nil))
(+doom/escape-just-called-cancel)

(defadvice! +doom/escape (orig-fn &optional interactive)
  :around #'doom/escape
  (unless +doom/escape-just-called
      (setq +doom/escape-just-called t)
      (run-at-time 0.4 #'+doom/escape-just-called-cancel))
  (funcall orig-fn interactive))

(defun +doom/just-escaped-p (&rest _)
  (when +doom/escape-just-called
    (+doom/escape-just-called-cancel)
    t))


(defun sticky-window-keep-window-visible ()
  "Insure the buffer associated with the current window stays visible.
This is handy for ERC buffers where you would like to see the
conversation while you work in other windows within the frame.
This is intended to be used with `sticky-window-delete-window'.
A prefix arg reverses this operation."
  (interactive)
  (set-window-dedicated-p (selected-window) (not current-prefix-arg)))