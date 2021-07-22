;;; navigation.el -*- lexical-binding: t; -*-

(use-package! iflipb
  :commands (iflipb-next-buffer iflipb-previous-buffer)
  :init
  (setq! iflipb-always-ignore-buffers
         (lambda (name)
           (or (string-match-p "^magit" name)
               (string-match-p "^\*" name)
               (string-match-p "^ " name)))))

(after! ivy
  (setq! ivy-magic-tilde nil
         ivy-height 10
         ivy-use-virtual-buffers t
         ivy-re-builders-alist '((forge-create-pullreq . ivy--regex-fuzzy)
                                 (counsel-git . ivy--regex-fuzzy)
                                 (t . orderless-ivy-re-builder))))

(after! counsel
  (setq! counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never %s %s"
         ;; counsel-find-file-occur-cmd "ls | grep -i -E '%s' | gxargs -d '\n' ls"
         ))

(use-package! counsel-tramp :commands (counsel-tramp))

(after! projectile
  (setq! projectile-verbose t
         projectile-enable-idle-timer t
         projectile-idle-timer-hook '(projectile-discover-projects-in-search-path)
         projectile-idle-timer-seconds 180)
  (pushnew! projectile-globally-ignored-directories "node_modules" ".shadow-cljs" ".lsp" ".storybook")
  (pushnew! projectile-project-root-files ".tabnine_root" "yarn.lock" ".yarnrc" ".eslintcache")
  (pushnew! projectile-globally-ignored-file-suffixes ".min.js" ".min.css" ".map"))
(after! dired
  (setq! dired-recursive-deletes 'always
         dired-kill-when-opening-new-dired-buffer t))
(after! dired-aux
  (setq! dired-create-destination-dirs 'always
         dired-isearch-filenames 'dwim))

(after! wdired
  (setq! wdired-allow-to-change-permissions t))

(use-package! dired-quick-sort
  :hook (dired-mode . dired-quick-sort-setup))

(after! avy
  (setq! avy-timeout-seconds 0.3
         avy-indent-line-overlay t
         avy-background t
         avy-highlight-first t))

(use-package! loccur :commands loccur-current)
(use-package! double-saber
  :init
  (add-hook! ivy-occur-grep-mode (cmd! (double-saber-mode 1) (setq-local double-saber-start-line 5)))
  (defadvice ivy-wgrep-change-to-wgrep-mode (after ivy-wgrep-change-to-wgrep-mode-double-sabber-advice activate)
    "disable `double-saber-mode' when enter wgrep mode"
    (interactive)
    (double-saber-mode -1))

  (defadvice wgrep-finish-edit (after ivy-wgrep-change-to-wgrep-mode-double-sabber-advice activate)
    "enable `double-saber-mode' when leave wgrep mode"
    (interactive)
    (double-saber-mode 1))

  (defadvice wgrep-abort-changes (after ivy-wgrep-change-to-wgrep-mode-double-sabber-advice activate)
    "enable `double-saber-mode' when leave wgrep mode"
    (interactive)
    (double-saber-mode 1)))

(use-package! bicycle
  :defer t
  :bind (:map outline-minor-mode-map
              ([tab] . bicycle-cycle)
              ([S-tab] . bicycle-cycle)
              ("C-i" . bicycle-cycle)
              ("<backtab>" . bicycle-cycle-global)))

(use-package! zoxide
  :defer t
  :init
  (add-hook! 'find-file-hook #'zoxide-add))

(use-package! reveal-in-osx-finder :defer t)

(use-package! orderless
  :init
  (add-hook! doom-first-input-hook (cmd! (require 'orderless)))
  (setq! completion-styles '(orderless)
         orderless-component-separator "[ &]"))

(use-package! corfu
  :hook (doom-first-input . corfu-global-mode))