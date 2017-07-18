;; -*- mode: emacs-lisp -*-
;; vim:filetype=lisp
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;; (setq package-check-signature nil)
(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     ivy
     imenu-list
     auto-completion
     better-defaults
     emacs-lisp
     git
     osx
     markdown
     org
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom
            shell-default-term-shell "/bin/zsh")
     ;; spell-checking
     evil-snipe
     prodigy
     (restclient :variables restclient-use-org t)
     syntax-checking
     version-control
     html
     lua
     clojure
     docker
     vimscript
     (typescript :variables
                 typescript-fmt-on-save t
                 typescript-fmt-tool 'typescript-formatter)
     javascript
     python
     (python :variables python-enable-yapf-format-on-save t)
     )
   dotspacemacs-additional-packages '(alect-themes
                                      auto-yasnippet
                                      butler
                                      company-flx
                                      docker-tramp
                                      dired+
                                      dired-quick-sort
                                      dired-narrow
                                      editorconfig
                                      elmacro
                                      cheat-sh
                                      eslint-fix
                                      evil-lion
                                      evil-textobj-anyblock
                                      evil-textobj-column
                                      evil-visual-mark-mode
                                      fzf
                                      golden-ratio-scroll-screen
                                      ibuffer-vc
                                      indium
                                      ivy-dired-history
                                      imenu-anywhere
                                      jscs
                                      key-chord
                                      noccur
                                      phi-search
                                      swiper
                                      saveplace
                                      vlf
                                      vue-mode
                                      webpaste
                                      zenburn-theme)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(
                                    speedbar
                                    stickyfunc-enhance
                                    rainbow-delimiters
                                    uuidgen
                                    vi-tilde-fringe
                                    ido-vertical-mode
                                    flx-ido
                                    )
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
   dotspacemacs-themes '(zenburn
                         spacemacs-light)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro for Powerline"
                               :size 15
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
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout t
   dotspacemacs-auto-resume-layouts t
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
   dotspacemacs-enable-paste-transient-state t
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
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
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
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
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
   org-enable-bootstrap-support t
   org-enable-github-support t
   better-defaults-move-to-end-of-code-first t
   evil-snipe-enable-alternate-f-and-t-behaviors t
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/"))
  ;; (package-initialize)
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
  (defun my-minibuffer-setup-hook ()
    (setq gc-cons-threshold most-positive-fixnum))
  (defun my-minibuffer-exit-hook ()
    (setq gc-cons-threshold 800000))

  (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
  (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)
  (eval-after-load "projectile"
    '(progn (setq magit-repo-dirs (mapcar (lambda (dir)
                                            (substring dir 0 -1))
                                          (remove-if-not (lambda (project)
                                                           (file-directory-p (concat project "/.git/")))
                                                         (projectile-relevant-known-projects)))

                  magit-repo-dirs-depth 3)))
  ;;;;;;;;;;;;;;;;;;;;; global ;;;;;;;;;;;;;;;;;;;;
  (defun minibuffer-inactive-mode-hook-setup ()
    ;; make `try-expand-dabbrev' from `hippie-expand' work in mini-buffer
    ;; @see `he-dabbrev-beg', so we need re-define syntax for '/'
    (set-syntax-table (let* ((table (make-syntax-table)))
                        (modify-syntax-entry ?/ "." table)
                        table)))
  (add-hook 'minibuffer-inactive-mode-hook 'minibuffer-inactive-mode-hook-setup)
  (setq tramp-default-method "ssh")
  (require 'dired-quick-sort)
  (ws-butler-global-mode)
  (dired-quick-sort-setup)
  (setq dumb-jump-prefer-searcher 'rg)
  ;; (which-function-mode)
  (setq mouse-wheel-scroll-amount '(0.001))
  (define-global-minor-mode global-golden-ratio-mode golden-ratio-mode
    (lambda () (golden-ratio-mode 1)))
  (spacemacs/toggle-mode-line-minor-modes-off)
  ;; (spacemacs/toggle-mode-line-point-position-on)
  (add-hook 'prog-mode-hook 'fci-mode)
  ;; (add-hook 'prog-mode-hook 'paredit-mode)
  (key-chord-mode 1) ;; if you're not already enabling key-chord-mode
  (require 'vlf-setup)

  (with-eval-after-load 'company
    (company-flx-mode +1))
  (evil-visual-mark-mode 1)
  (global-evil-mc-mode 1)
  ;; (global-golden-ratio-mode)
  ;; (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
  ;; (require 'ycmd)
  ;; (add-hook 'js2-mode-hook #'ycmd-mode)
  ;; (set-variable 'ycmd-server-command (cons "python2" (cons (expand-file-name "~/.vim/plugged/YouCompleteMe/third_party/ycmd/ycmd") nil)))
  ;; (set-variable 'ycmd-global-config "~/.ycm_extra_conf.py")
  ;; (setq ycmd-force-semantic-completion t)
  ;; (global-ycmd-mode)
  ;; (require 'company-ycmd)
  ;; (company-ycmd-setup)
  (show-smartparens-global-mode 0)
  (show-paren-mode 1)

  ;;;;;;;;;;;;;;;;;;;;;;;;;; load-file ;;;;;;;;;;;;;;;;;;;;;;;;
  (load-file "~/.my_emacs/funcs.el")
  (load-file "~/.emacs.d/private/local/hide-comnt.el")
  (load-file "~/.my_emacs/keychord.el")
  (load-file "~/.my_emacs/keymap.el")
  (load-file "~/.my_emacs/org.el")
  (load-file "~/.my_emacs/javascript.el")
  (load-file "~/.my_emacs/ibuffer.el")
  (load-file "~/.my_emacs/popwin.el")
  (load-file "~/.my_emacs/modeline.el")
  (load-file "~/.my_emacs/untabify.el")
  (load-file "~/.my_emacs/dired.el")
  (load-file "~/.my_emacs/prodigy.el")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;; settings ;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; (setq helm-ff-auto-update-initial-value t)
  (setq company-idle-delay 0.01)
  (setq diary-file "~/Dropbox/org/diary")
  (setq dired-hide-details-hide-information-lines nil)
  (setq dired-hide-details-hide-symlink-targets nil)
  (setq dired-use-ls-dired nil)
  (setq evil-esc-delay 0)
  (setq evil-escape-unordered-key-sequence t)
  (setq evil-ex-hl-update-delay 0.01)
  (setq evil-vsplit-window-right t)
  (setq evil-want-C-i-jump t)
  ;; (setq evil-want-fine-undo t)
  (setq flycheck-check-syntax-automatically (quote (save mode-enabled)))
  (setq flycheck-checker-error-threshold 1000)
  (setq flycheck-disabled-checkers (quote (javascript-jshint javascript-jscs)))
  (setq flycheck-display-errors-delay 0)
  (setq flycheck-standard-error-navigation t)
  ;; (setq flycheck-highlighting-mode (quote symbols))
  (setq golden-ratio-scroll-highlight-delay (quote (0.07 . 0.03)))
  (setq golden-ratio-scroll-highlight-flag (quote (quote nil)))
  (setq ibuffer-mode-hook (quote (ibuffer-vc-set-filter-groups-by-vc-root)))
  (setq idle-update-delay 0.03)
  (setq js2-mode-show-parse-errors t)
  (setq js2-mode-show-strict-warnings nil)
  (setq magit-popup-show-common-commands t)
  ;; (setq large-file-warning-threshold 1048576)
  (setq locate-command "mdfind -onlyin /Users/rashawnzhang -name ")
  (defun disable-magit-highlight-in-buffer ()
    (face-remap-add-relative 'magit-item-highlight '()))
  (add-hook 'magit-status-mode-hook 'disable-magit-highlight-in-buffer)
  (setq magit-commit-show-diff t
        magit-revert-buffers 1)
  (setq evil-move-cursor-back nil) ;;cursor don't go back
  (setq save-place-file "~/.emacs.d/saveplace") ;;remeber cursor location on reoppening

  ;; (setq auth-source-netrc-use-gpg-tokens (quote (t gpg)))
  (setq edit-server-url-major-mode-alist
        '(("github\\.com" . org-mode)))
  (setq org-capture-templates
        (quote
         (("l" "Capture from the Internet with link" entry
           (file "~/Dropbox/org/notes.org")
           "* %? %^L      %^G
%c
Entered on %U")
          ("n" "notes" entry
           (file "~/Dropbox/org/notes.org")
           "* %?
Entered on %U")
          ("f" "file TODOs" entry
           (file "~/Dropbox/org/gtd.org")
           "* TODO %?           %^G
 %a
 %U")
          ("t" "TODOs" entry
           (file "~/Dropbox/org/gtd.org")
           "* TODO %?           %^G
 %U"))))
  (setq org-projectile:allow-tramp-projects t)
  (setq org-projectile:capture-template "*** TODO %?
%a")
  (setq org-projectile:projects-file "~/Dropbox/org/projects.org")
  (setq spacemacs-theme-org-highlight t)
  (setq user-full-name "yqrashawn")
  (setq vlf-batch-size 314572)
  (setq which-key-allow-imprecise-window-fit t)
  (setq which-key-popup-type (quote minibuffer))
  ;; (setq auto-indent-indent-style 'conservative)
  ;; (setq auto-indent-style 'conservative)
  ;; (setq auto-indent-on-visit-file t)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;; lisp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; (setq inferior-lisp-program "/usr/local/bin/sbcl")
  ;; (setq slime-contribs '(slime-fancy))
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
          ("DONE" . "#afd8af")
          ("NOTE" . "#d0bf8f")
          ("KLUDGE" . "#d0bf8f")
          ("HACK" . "#d0bf8f")
          ("FIXME" . "#cc9393")
          ("XXX" . "#cc9393")
          ("XXXX" . "#cc9393")
          ("???" . "#cc9393")
          ("DEBUG" . "#ff0000"))))
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a" "6fc0ae7cc2abd82d8add1140874ccf8773feaaae73a704981d52fdf357341038" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" default)))
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(emms-mode-line-icon-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #1fb3b3\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" };")))
 '(evil-want-Y-yank-to-eol t)
 '(fci-rule-color "#383838")
 '(gnus-logo-colors (quote ("#528d8d" "#c0c0c0")) t)
 '(gnus-mode-line-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #1fb3b3\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };")) t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (cheat-sh treemacs-evil treemacs gited git yaml-mode ivy-dired-history seoul256-theme ob-restclient ob-http company-restclient know-your-http-well dockerfile-mode docker tablist butler dired-k editorconfig csv-mode docker-tramp indium websocket prodigy vue-mode xref-js2 seq clojure-snippets clj-refactor inflections edn paredit peg cider-eval-sexp-fu cider queue clojure-mode slime-company slime common-lisp-snippets dired-hacks-utils dired-narrow dired+ winum unfill fuzzy webpaste restclient evil-lion elmacro dired-quick-sort ssass-mode vue-html-mode yapfify xterm-color ws-butler window-numbering which-key wgrep web-mode web-beautify volatile-highlights vlf vimrc-mode use-package toc-org tide typescript-mode tagedit spacemacs-theme spaceline powerline smex smeargle slim-mode shell-pop scss-mode sass-mode reveal-in-osx-finder restart-emacs pyvenv pytest pyenv-mode py-isort pug-mode popwin pip-requirements phi-search persp-mode pcre2el pbcopy paradox spinner ox-twbs ox-gfm osx-trash osx-dictionary orgit org-projectile org-present org org-pomodoro alert log4e gntp org-plus-contrib org-download org-bullets open-junk-file nodejs-repl noccur neotree mwim multi-term move-text mmm-mode matlab-mode markdown-toc markdown-mode magit-gitflow macrostep lua-mode lorem-ipsum livid-mode skewer-mode simple-httpd live-py-mode linum-relative link-hint less-css-mode launchctl key-chord json-mode json-snatcher json-reformat jscs js2-refactor multiple-cursors js2-mode js-doc ivy-hydra info+ indent-guide imenu-list imenu-anywhere ibuffer-vc hydra hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-make helm helm-core haml-mode google-translate golden-ratio-scroll-screen golden-ratio gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gh-md fzf flyspell-correct-ivy flyspell-correct flycheck-ycmd flycheck-pos-tip pos-tip flycheck fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-column names evil-textobj-anyblock evil-surround evil-snipe evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit magit magit-popup with-editor evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight eslint-fix eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav dumb-jump diminish diff-hl dactyl-mode cython-mode counsel-projectile projectile counsel swiper ivy company-ycmd pkg-info request-deferred request deferred epl company-web web-completion-data company-tern dash-functional tern company-statistics company-flx flx company-anaconda company column-enforce-mode coffee-mode clean-aindent-mode bind-map bind-key auto-yasnippet yasnippet auto-highlight-symbol auto-dictionary auto-compile packed async anaconda-mode pythonic f dash s alect-themes aggressive-indent adaptive-wrap ace-window ace-link avy ac-ispell auto-complete popup quelpa package-build zenburn-theme)))
 '(password-cache-expiry 3600)
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro for Powerline" :foundry "nil" :slant normal :weight normal :height 120 :width normal))))
 '(evil-search-highlight-persist-highlight-face ((t (:inherit lazy-highlight :underline "turquoise1" :weight ultra-bold)))))
