;;; navigation.el -*- lexical-binding: t; -*-

(use-package! iflipb
  :commands (iflipb-next-buffer iflipb-previous-buffer)
  :init
  (setq! iflipb-always-ignore-buffers
         (lambda (name)
           (or (string-match-p "^magit" name)
               (string-match-p "^\*" name)
               (string-match-p "^ " name)))))

(after! vertico
  (setq! vertico-count 10
         vertico-cycle t))
(after! ivy
  (setq! ivy-magic-tilde nil
         ivy-height 10
         ivy-use-virtual-buffers t
         ivy-re-builders-alist '((forge-create-pullreq . ivy--regex-fuzzy)
                                 (counsel-git . ivy--regex-fuzzy)
                                 (t . ivy--regex-plus))))
                                 ;; (t . orderless-ivy-re-builder)
                                 

(after! counsel
  (setq! counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never %s %s"))
         ;; counsel-find-file-occur-cmd "ls | grep -i -E '%s' | gxargs -d '\n' ls"
         

;; (use-package! counsel-tramp :commands (counsel-tramp))

(after! projectile
  (setq! projectile-verbose t
         projectile-enable-idle-timer t
         projectile-idle-timer-hook '(projectile-discover-projects-in-search-path)
         projectile-idle-timer-seconds 180)
  (pushnew! projectile-globally-ignored-directories "node_modules" ".shadow-cljs" ".lsp" ".storybook")
  (pushnew! projectile-project-root-files ".tabnine_root" "yarn.lock" ".yarnrc" ".eslintcache" ".node-version")
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
         avy-all-windows t
         avy-indent-line-overlay t
         avy-single-candidate-jump t
         avy-background t
         avy-keys '(?f ?j ?d ?k ?a ?s ?l ?g ?h ?e ?i ?r ?u ?w ?o ?v ?m)
         avy-highlight-first t))

(use-package! loccur :commands loccur-current)
(use-package! double-saber
  :init
  (add-hook! occur-mode (cmd! (double-saber-mode 1) (setq-local double-saber-start-line 5)))
  (defadvice ivy-wgrep-change-to-wgrep-mode (after ivy-wgrep-change-to-wgrep-mode-double-sabber-advice activate)
    "disable `double-saber-mode' when enter wgrep mode"
    (interactive)
    (double-saber-mode -1))

  (defadvice wgrep-finish-edit (after ivy-wgrep-change-to-wgrep-mode-double-sabber-advice activate)
    "enable `double-saber-mode' when leave wgrep mode"
    (interactive)
    (with-current-buffer (current-buffer)
        (double-saber-mode 1)))

  (defadvice wgrep-abort-changes (after ivy-wgrep-change-to-wgrep-mode-double-sabber-advice activate)
    "enable `double-saber-mode' when leave wgrep mode"
    (interactive)
    (with-current-buffer (current-buffer)
      (double-saber-mode 1))))

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
  (defun +zoxide-add (&optional path &rest _)
    "Add PATH to zoxide database.  This function is called asynchronously."
    (interactive "Dpath: ")
    (unless path
      (setq path default-directory))
    (zoxide-run t "add" path)

    (let ((b (buffer-file-name)))
      (unless (or (s-ends-with? ".git" b)
                (s-contains? "/.git/" b))
        (zoxide-run t "add" b))))
  (add-hook! 'find-file-hook #'+zoxide-add)
  (defvar consult-dir--source-zoxide
    `(:name "Zoxide dirs"
      :narrow ?z
      :category file
      :face consult-file
      :history file-name-history
      :enabled ,(lambda () (executable-find "zoxide"))
      :items ,#'zoxide-query)
    "Zoxide directory source for `consult-dir'.")
  (after! consult-dir
    (pushnew! consult-dir-sources 'consult-dir--source-zoxide)))

(use-package! reveal-in-osx-finder :defer t)

;; (use-package! corfu
;;   :hook (doom-first-input . corfu-global-mode)
;;   :init
;;   (setq! corfu-auto t))

;; (use-package affe
;;   :defer t
;;   :config
;;   ;; Configure Orderless
;;   (setq! affe-regexp-function #'orderless-pattern-compiler
;;          affe-highlight-function #'orderless--highlight)

;;   ;; Manual preview key for `affe-grep'
;;   (consult-customize affe-grep :preview-key (kbd "M-.")))

(use-package! spacehammer
  :commands (spacehammer/edit-with-emacs spacehammer/fix-frame spacehammer/activate-capture-frame spacehammer/switch-to-app)
  :load-path "~/.hammerspoon/")

(setq! ranger-return-to-ranger t
       ranger-show-hidden 'format
       ranger-persistent-sort t)

(setq-default
  isearch-lazy-count t
  search-ring-max 200
  regexp-search-ring-max 200
  isearch-regexp-lax-whitespace t
  search-whitespace-regexp ".*?")

(use-package! isearch-mb
  :hook (doom-first-input . isearch-mb-mode))



;;; orderless-flex for some commands
(defun ++flex! (fn &rest args)
  (let ((orderless-matching-styles '(orderless-flex)))
    (apply fn args)))
(advice-add 'magit-completing-read :around #'++flex!)
(advice-add 'magit-completing-read-multiple :around #'++flex!)
(advice-add 'magit-completing-read-multiple* :around #'++flex!)

;; (use-package! dogears
;;   :hook (doom-first-input . dogears-mode)
;;   :config
;;   (after! savehist
;;     (pushnew! savehist-additional-variables 'dogears-list)))
