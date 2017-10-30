;; -*- mode: emacs-lisp -*-
;; vim:filetype=lisp
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.
(setq scroll-bar-background nil)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . 'dark)) ; or 'dark, to switch to white title text

;; (setq package-check-signature nil)
(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs-base
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-frame-title-format "%I@%S"
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     artist
     yq-mode-line
     ;; parinfer
     ;; nlinum
     bm
     github
     ;; command-log
     spacemacs-evil
     spacemacs-editing
     spacemacs-editing-visual
     spacemacs-completion
     spacemacs-org
     spacemacs-visual
     spacemacs-layouts
     ;; spacemacs-purpose
     spacemacs-misc
     spacemacs-navigation
     (ibuffer :variables ibuffer-group-buffers-by 'projects)
     ruby
     ivy
     fasd
     ;; (c-c++ :variables
     ;;        c-c++-default-mode-for-headers 'c++-mode
     ;;        c-c++-enable-clang-support t)
     imenu-list
     pdf-tools
     auto-completion
     better-defaults
     navigation
     (typescript :variables
                 typescript-fmt-on-save t)
     ;; undohist
     (mu4e :variables mu4e-account-alist t)
     emacs-lisp
     git
     osx
     indium
     (markdown :variables
               markdown-live-preview-engine 'vmd
               markdown-mmm-auto-modes '("c" "c++" "python" "scala" ("elisp" "emacs-lisp") ("javascript" "js2-mode")))
     ;; node
     (org :variables org-projectile-file "plans.org")
     (shell :variables
            shell-default-height 30
            ;; shell-default-position 'right
            shell-default-position 'bottom
            shell-default-term-shell "/bin/zsh")
     ;; spell-checking
     evil-snipe
     prodigy
     (restclient :variables restclient-use-org t)
     syntax-checking
     (spell-checking :variables
                     spell-checking-enable-by-default nil
                     spell-checking-enable-auto-dictionary nil
                     enale-flyspell-auto-completion t)
     version-control
     html
     react
     ;; docker
     ;; vimscript
     javascript
     ;; (python :variables python-enable-yapf-format-on-save t)
     )
   dotspacemacs-additional-packages '(
                                      ;; butler
                                      anti-zenburn-theme
                                      company-flx
                                      circe
                                      circe-notifications
                                      ;; smart-mode-line
                                      ;; org-mind-map
                                      ;; cheat-sh
                                      ;; docker-tramp
                                      dired-subtree
                                      dired+
                                      dired-quick-sort
                                      dired-narrow
                                      dired-details+
                                      ;; elmacro
                                      evil-textobj-anyblock
                                      evil-textobj-column
                                      glsl-mode
                                      gruvbox-theme
                                      golden-ratio-scroll-screen
                                      hackernews
                                      ivy-dired-history
                                      imenu-anywhere
                                      ;; key-chord
                                      keyfreq
                                      noccur
                                      phi-search
                                      quickrun
                                      ;; vlf
                                      nodejs-repl
                                      sunburn-theme
                                      ;; visual-ascii-mode
                                      webpaste
                                      )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https nil
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-light sunburn anti-zenburn gruvbox-light-soft gruvbox-light-medium)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '(;; "Anonymous Pro for Powerline"
                               "InconsolataG for Powerline"
                               ;; "Source Code Pro for Powerline"
                               :size 16
                               :weight normal
                               :width normal
                               :powerline-scale 1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text t
   dotspacemacs-ex-substitute-global t
   dotspacemacs-default-layout-name "General"
   dotspacemacs-display-default-layout t
   dotspacemacs-auto-resume-layouts t
   dotspacemacs-auto-generate-layout-names t
   ;; Size (in MB)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native t
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 10
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling nil
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis t
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg' `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup t
   spell-checking-enable-by-default nil
   syntax-checking-enable-tooltips nil
   auto-completion-complete-with-key-sequence-delay 0.01
   auto-completion-enable-snippets-in-popup t
   auto-completion-enable-sort-by-usage t
   auto-completion-private-snippets-directory "~/.emacs.d/private/snippets"
   auto-completion-enable-help-tooltip nil
   org-enable-bootstrap-support t
   org-enable-github-support t
   better-defaults-move-to-end-of-code-first t
   evil-snipe-enable-alternate-f-and-t-behaviors t
   dotspacemacs-switch-to-buffer-prefers-purpose t
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  (setq custom-file (concat spacemacs-cache-directory ".custom-settings"))
  (setq exec-path-from-shell-check-startup-files nil) ;; only from .zshenv
  (setq exec-path-from-shell-arguments '("-l"))  ;; remove -i read form .zshenv
  (add-to-list 'package-archives
               '("melpa-china" . "http://elpa.emacs-china.org/melpa/"))
  ;; (add-to-list 'package-archives
  ;;              '("melpa" . "https://melpa.org/packages/"))
  (setq configuration-layer--elpa-archives
        '(
          ;; ("melpa" . "melpa.org/packages/")
          ;; ("org"   . "orgmode.org/elpa/")
          ;; ("gnu"   . "elpa.gnu.org/packages/")
          ("melpa-cn" . "http://elpa.emacs-china.org/melpa/")
          ("org-cn"   . "http://elpa.emacs-china.org/org/")
          ("gnu-cn"   . "http://elpa.emacs-china.org/gnu/")
          ))
  (setq insert-directory-program "/usr/local/opt/coreutils/bin/gls")
  (setq dired-listing-switches "-aBhl --group-directories-first")
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist
        `(("." . ,(concat user-home-directory "Dropbox/sync/undo"))))
  (unless (file-exists-p (concat user-home-directory "Dropbox/sync/undo"))
    (make-directory (concat user-home-directory "Dropbox/sync/undo")))
  ;; (when (memq window-system '(mac ns))
  ;;   (exec-path-from-shell-copy-env "LC_ALL")
  ;;   (exec-path-from-shell-copy-env "TERM")
  ;;   (exec-path-from-shell-initialize))
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  (setq source-directory (concat user-home-directory "emacs/src"))
  (global-hl-line-mode -1)
  (setq send-mail-function 'mailclient-send-it)
  (setq password-cache-expiry 3600)
  (setq recentf-auto-cleanup 300)
  (setq rm-whitelist (quote ("haha")))
  (setq evil-want-Y-yank-to-eol t)
  (setq keyfreq-file (concat spacemacs-cache-directory "emacs.keyfreq"))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  ;; (sml/setup)
  ;; (setq sml/theme 'respectful)
  ;; (setq sml/show-trailing-N 'nil)
  (setq mac-frame-tabbing t)
  (add-hook 'markdown-mode-hook 'auto-fill-mode)
  ;; rg for swiper
  (setq counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  ;; (when (executable-find "remacsclient")
  ;;   (setq with-editor-emacsclient-executable (executable-find "remacsclient")))
  ;; fix buffer gc problem
  (defun my-minibuffer-setup-hook ()
    (setq gc-cons-threshold most-positive-fixnum))
  (defun my-minibuffer-exit-hook ()
    ;; (setq gc-cons-threshold 1000000)
    ;; DEBUG
    (setq gc-cons-threshold (* 10 1024 1024)))
  (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
  (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

  ;; http://www.wilfred.me.uk/.emacs.d/init.html#orgd2d7f0f
  (setq minibuffer-depth-indicate-mode 1)

  ;; fix shell color
  (setq system-uses-terminfo nil)
  (eval-after-load "projectile"
    '(progn (setq magit-repo-dirs (mapcar (lambda (dir)
                                            (substring dir 0 -1))
                                          (remove-if-not (lambda (project)
                                                           (file-directory-p (concat project "/.git/")))
                                                         (projectile-relevant-known-projects)))
                  magit-repo-dirs-depth 3)))
  ;;;;;;;;;;;;;;;;;;;;; global ;;;;;;;;;;;;;;;;;;;;
  (setq mc/always-run-for-all t)
  (global-company-mode)
  (setq eshell-aliases-file "/Users/rashawnzhang/.my_emacs/aliases")
  (defun minibuffer-inactive-mode-hook-setup ()
    ;; make `try-expand-dabbrev' from `hippie-expand' work in mini-buffer
    ;; @see `he-dabbrev-beg', so we need re-define syntax for '/'
    (set-syntax-table (let* ((table (make-syntax-table)))
                        (modify-syntax-entry ?/ "." table)
                        table)))
  (add-hook 'minibuffer-inactive-mode-hook 'minibuffer-inactive-mode-hook-setup)
  ;; (setq projectile-tags-command "ctags -Re --languages=javascript --exclude=.git --exclude=log --exclude=build --exclude=sampleModels --exclude=study --exclude=node_modules --exclude=release --exclude=\\*.min.\\* %s %s .")
  (setq projectile-tags-command nil)
  (setq tramp-default-method "ssh")
  ;; (ws-butler-global-mode)
  (setq dumb-jump-prefer-searcher 'rg)
  (setq mouse-wheel-scroll-amount '(0.001))
  ;; (define-global-minor-mode global-golden-ratio-mode golden-ratio-mode
  ;; (lambda () (golden-ratio-mode 1)))
  ;; (key-chord-mode 1) ;; if you're not already enabling key-chord-mode
  ;; (require 'vlf-setup)

  (with-eval-after-load 'company
    (company-flx-mode +1))
  (global-evil-mc-mode)
  (show-smartparens-global-mode 0)
  (show-paren-mode 1)

  ;;;;;;;;;;;;;;;;;;;;;;;;;; load-file ;;;;;;;;;;;;;;;;;;;;;;;;
  (load-file "~/.my_emacs/el.el")
  (load-file "~/.my_emacs/funcs.el")
  (load-file "~/.my_emacs/aliases.el")
  (load-file "~/.emacs.d/private/local/hide-comnt.el")
  ;; (load-file "~/.my_emacs/keychord.el")
  (load-file "~/.my_emacs/keymap.el")
  (load-file "~/.my_emacs/mail.el")
  (load-file "~/.my_emacs/org.el")
  (load-file "~/.my_emacs/javascript.el")
  (load-file "~/.my_emacs/popwin.el")
  (load-file "~/.my_emacs/dired.el")
  (load-file "~/.my_emacs/prodigy.el")
  ;; (load-file "~/.my_emacs/modeline.el")
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;; settings ;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq company-idle-delay 0.15)
  (setq diary-file "~/Dropbox/org/diary")
  (setq dired-hide-details-hide-information-lines nil)
  (setq dired-hide-details-hide-symlink-targets nil)
  (setq dired-use-ls-dired nil)
  (setq evil-esc-delay 0)
  (setq evil-escape-unordered-key-sequence t)
  ;; (setq evil-ex-hl-update-delay 0.01)
  (setq evil-vsplit-window-right nil)
  (setq evil-want-C-i-jump t)
  ;; (setq mac-allow-anti-aliasing nil)
  (setq mac-allow-anti-aliasing t)
  ;; (setq evil-want-fine-undo t)
  (setq flycheck-check-syntax-automatically (quote (save mode-enabled)))
  (setq flycheck-checker-error-threshold 1000)
  (setq flycheck-display-errors-delay 0.1)
  (setq flycheck-standard-error-navigation t)

  (setq golden-ratio-scroll-highlight-delay (quote (0.07 . 0.03)))
  (setq golden-ratio-scroll-highlight-flag (quote (quote nil)))
  (evil-define-motion evil-goto-definition ()
    "Go to definition or first occurrence of symbol under point."
    :jump t
    :type exclusive
    (let* ((string (evil-find-symbol t))
           (search (format "\\_<%s\\_>" (regexp-quote string)))
           ientry ipos)
      ;; load imenu if available
      (unless (featurep 'imenu)
        (condition-case nil
            (require 'imenu)
          (error nil)))
      (if (null string)
          (user-error "No symbol under cursor")
        (setq isearch-forward t)
        ;; if imenu is available, try it
        (cond
         ((fboundp 'imenu--make-index-alist)
          (condition-case nil
              (setq ientry (imenu--make-index-alist))
            (error nil))
          (setq ientry (assoc string ientry))
          (setq ipos (cdr ientry))
          (when (and (markerp ipos)
                     (eq (marker-buffer ipos) (current-buffer)))
            (setq ipos (marker-position ipos)))
          (cond
           ;; imenu found a position, so go there and
           ;; highlight the occurrence
           ((numberp ipos)
            (evil-search search t t ipos))
           (t
            (evil-search search t t (point-min)))))
         ;; otherwise just go to first occurrence in buffer
         (t
          (evil-search search t t (point-min)))))))
  (setq idle-update-delay 0.2)
  ;; (setq large-file-warning-threshold 1048576)
  (setq magit-popup-show-common-commands t)
  (setq locate-command "mdfind -onlyin /Users/rashawnzhang -name ")
  (defun disable-magit-highlight-in-buffer ()
    (face-remap-add-relative 'magit-item-highlight '()))
  ;; (add-hook 'magit-status-mode-hook 'disable-magit-highlight-in-buffer)
  ;; (setq magit-commit-show-diff t
  ;; magit-revert-buffers 1)
  (setq evil-move-cursor-back nil) ;;cursor don't go back
  (setq save-place-file "~/.emacs.d/saveplace") ;;remeber cursor location on reoppening

  (setq edit-server-url-major-mode-alist
        '(("github\\.com" . org-mode)))
  (setq spacemacs-theme-org-highlight t)
  (setq user-full-name "yqrashawn")
  ;; (setq vlf-batch-size 314572)
  (setq which-key-allow-imprecise-window-fit t)
  (setq which-key-popup-type (quote minibuffer))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;; lisp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq hl-todo-keyword-faces
        (quote
         (("HOLD" . "#d0bf8f")
          ("TODO" . "#cc9393")
          ("NEXT" . "#dca3a3")
          ("THEM" . "#dc8cc3")
          ("PROG" . "#7cb8bb")
          ("OKAY" . "#7cb8bb")
          ("DONT" . "#5f7f5f")
          ("FAIL" . "#8c5353")
          ("FIX" . "#8c5353")
          ("DONE" . "#afd8af")
          ("NOTE" . "#d0bf8f")
          ("KLUDGE" . "#d0bf8f")
          ("HACK" . "#d0bf8f")
          ("FIXME" . "#cc9393")
          ("XXX" . "#cc9393")
          ("XXXX" . "#cc9393")
          ("???" . "#cc9393")
          ("DEBUG" . "#ff0000")))))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
  )
