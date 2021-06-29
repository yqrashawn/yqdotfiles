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
       confirm-kill-processes nil)

(+global-word-wrap-mode +1)

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

(use-package side-notes :init (setq! side-notes-file "notes.side.org"))

(use-package explain-pause-mode :defer t)

(after! with-editor (shell-command-with-editor-mode))