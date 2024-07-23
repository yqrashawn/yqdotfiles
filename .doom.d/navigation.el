;;; navigation.el -*- lexical-binding: t; -*-

(use-package! iflipb
  :commands (iflipb-next-buffer iflipb-previous-buffer)
  :init
  ;; restrict buffer list by workspace
  (setq! iflipb-buffer-list-function #'doom-project-buffer-list)

  (defun +iflipb-always-ignore-buffers (name)
    "return `t' if should be excluded"
    ;; (log/spy '+iflipb-always-ignore-buffers)
    (let* ((_b (get-buffer name))
           (rst (cond
                 ((s-starts-with? " " name) t)
                 ((string-match "^\\*\\(Async-native-compile\\|helpful\\|Native-compile\\)" name) t)
                 ((string-match "^\\*\\(doom\\|Messages\\|envrc\\|zoxide\\)\\*$" name) t)
                 (t nil))))
      rst))
  (setq! iflipb-always-ignore-buffers '+iflipb-always-ignore-buffers)

  (defun +iflipb-ignore-buffers (name)
    "return `t' if should be excluded"
    ;; (log/spy '+iflipb-ignore-buffers)
    (let* ((b (get-buffer name))
           (rst (cond
                 ((eq b (current-buffer)) t)
                 ((string-match "^\\(magit-process:\\|Messages\\)" name) t)
                 ((memq (buffer-local-value 'major-mode b) '(dired-mode)) t)
                 (t nil))))
      rst))
  (setq! iflipb-ignore-buffers '+iflipb-ignore-buffers))

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
  (setq! projectile-verbose nil
         projectile-sort-order 'recentf
         projectile-enable-idle-timer t
         projectile-idle-timer-hook '(projectile-discover-projects-in-search-path)
         projectile-idle-timer-seconds 180)
  (pushnew! projectile-globally-ignored-directories "node_modules" ".shadow-cljs" ".lsp" ".storybook")
  (pushnew! projectile-project-root-files ".tabnine_root" "yarn.lock" ".yarnrc" ".eslintcache" ".node-version")
  (pushnew! projectile-globally-ignored-file-suffixes ".min.js" ".min.css" ".map")
  (defadvice! +projectile-keep-project-p (orig-fn project)
    :around #'projectile-keep-project-p
    (cond
     ((file-remote-p project nil t) (file-readable-p project))
     ;; ((file-remote-p project))
     ((not (file-remote-p project)) (file-readable-p project))))
  (defun +projectile-status-mobile-project-p ()
    (require 's)
    (s-ends-with? "status-mobile/" (doom-project-root)))
  (projectile-register-project-type 'status-mobile '+projectile-status-mobile-project-p
                                    :project-file "shadow-cljs.edn"
                                    :compile "make run-clojure"
                                    :src-dir "src"
                                    :test "make test"
                                    :test-suffix "_test.cljs"))

(after! dired
  (setq! dired-recursive-deletes 'always
         dired-kill-when-opening-new-dired-buffer nil)
  (cl-defun dwim-shell-command-on-marked-files (buffer-name script &key utils extensions shell-util shell-args shell-pipe post-process-template on-completion)
    "Execute SCRIPT, using BUFFER-NAME.
See `dwim-shell-command-execute-script' for all other params."
    (dwim-shell-command-execute-script buffer-name script
                                       :files (dwim-shell-command--marked-files)
                                       :utils utils
                                       :extensions extensions
                                       :shell-util shell-util
                                       :shell-args shell-args
                                       :shell-pipe shell-pipe
                                       :post-process-template post-process-template
                                       :on-completion on-completion))

  (defun dwim-shell-command--marked-files ()
    "Return buffer file (if available) or marked files for a `dired' buffer."
    (if (buffer-file-name)
        (list (buffer-file-name))
      (dired-get-marked-files))))
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
  :defer t
  :init
  (add-hook! occur-mode
    (double-saber-mode 1)
    (setq-local double-saber-start-line 5))
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

    (require 's)
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

;; (use-package affe
;;   :defer t
;;   :config
;;   ;; Configure Orderless
;;   (setq! affe-regexp-function #'orderless-pattern-compiler
;;          affe-highlight-function #'orderless--highlight)

;;   ;; Manual preview key for `affe-grep'
;;   (consult-customize affe-grep :preview-key (kbd "M-.")))

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

(after! embark
  (defadvice! +embark-swiper () :after #'embark-isearch (swiper-isearch-toggle))
  (defadvice! +embark-insert-relative-path (orig-fn &optional file)
    :around #'embark-insert-relative-path
    (let ((default-directory (or (doom-project-root) default-directory)))
      (funcall-interactively orig-fn file)))
  (defadvice! +embark-save-relative-path (orig-fn &optional file)
    :around #'embark-save-relative-path
    (let ((default-directory (or (doom-project-root) default-directory)))
      (funcall-interactively orig-fn file))))

(use-package! projectile
  :commands (projectile-switch-project-by-name)
  :init
  (setq! +project-name-alises '(("status-mobile" . "stm")
                                ("status-go" . "stg")
                                ("status-desktop" . "std")
                                ("frontends" . "scf")
                                ("new-token-list" . "ntl")))
  (defun +projectile-project-name (project-root)
    (let* ((default-name (projectile-default-project-name project-root))
           (alias (alist-get default-name +project-name-alises nil nil 'string=)))
      (or alias default-name)))
  (setq! projectile-project-name-function '+projectile-project-name))

(defadvice! ++workspace/close-window-or-workspace (orig-fn)
  "Don't delete workspace if only visible window is magit-status buffer"
  :around #'+workspace/close-window-or-workspace
  (if (and (memq major-mode '(magit-status-mode))
           (not (cdr (doom-visible-windows))))
      nil
    (funcall orig-fn)))

(setq! +workspaces-on-switch-project-behavior t)

(after! fd-dired
  (setq! fd-dired-ls-option '("| xargs -0 ls -ld --quoting-style=literal | uniq" . "-ld")))
