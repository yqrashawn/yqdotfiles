;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     windows-scripts
     prodigy
     rcirc
     imenu-list
     ivy
     auto-completion
     better-defaults
     emacs-lisp
     git
     markdown
     org
     osx
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     ;; spell-checking
     syntax-checking
     version-control
     scheme
     html
     javascript
     (javascript :variables javascript-disable-tern-port-files nil)
     ;; python
     ;; (python :variables python-enable-yapf-format-on-save t)
     (evil-snipe :variables evil-snipe-enable-alternate-f-and-t-behaviors t)
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(buffer-flip yasnippet saveplace zoom-window phi-search js2-highlight-vars babel-repl jscs vlf eslint-fix slime js-comint)
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
  ;; TODO: there'll be better way to do this.
  ;; See https://github.com/magit/magit/issues/2559
  (setq magit-auto-revert-mode nil)
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 30
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
   dotspacemacs-scratch-mode 'js2-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-light
                         spacemacs-dark)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
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
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text t
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global t
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts t
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
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
   dotspacemacs-which-key-delay 0.1
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
   dotspacemacs-maximized-at-startup nil
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
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  (package-initialize)
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-copy-env "LC_ALL")
    (exec-path-from-shell-copy-env "TERM")
    (exec-path-from-shell-initialize))
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  (setq url-proxy-services
        '(("http" . "127.0.0.1:6152")
          ("https" . "127.0.0.1:6152")))

  ;; (setq debug-on-error t)
  ;; hack for GC, no freeze anymore
  (defun my-minibuffer-setup-hook ()
    (setq gc-cons-threshold most-positive-fixnum))

  (defun my-minibuffer-exit-hook ()
    (setq gc-cons-threshold 800000))

  (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
  (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

  ;;;;;;;;;;;;;;;;;;;;; prodigy service ;;;;;;;;;;;;;;;;;;;;
  ;; (prodigy-define-service
  ;;  :name "Build BIMSDK"
  ;;  :command "webpack"
  ;;  :cwd "/Users/Rashawn/workspace/chenzhou/MixSDK/BIMSDK"
  ;;  :args '("--config webpack.bimsdk.js" "-w" "--lint" "--notify")
  ;;  :tags '(work))

  ;; (prodigy-define-service
  ;;  :name "Build MixSDK"
  ;;  :command "npm"
  ;;  :args '("run" "build")
  ;;  :cwd "/Users/Rashawn/workspace/chenzhou/MixSDK"
  ;;  :tags '(work))

  ;; (prodigy-define-service
  ;;  :name "BIMSDK http-server"
  ;;  :command "http-server"
  ;;  :args '("-p" "2323")
  ;;  :port 2323
  ;;  :cwd "/Users/Rashawn/workspace/chenzhou/MixSDK/BIMSDK"
  ;;  :tags '(work))

  ;; (prodigy-define-service
  ;;  :name "BIMSDK http-server"
  ;;  :command "http-server"
  ;;  :args '("-p" "2121")
  ;;  :port 2121
  ;;  :cwd "/Users/Rashawn/workspace/chenzhou/MixSDK"
  ;;  :tags '(work))


  ;;;;;;;;;;;;;;;;;;;;; global ;;;;;;;;;;;;;;;;;;;;
  (which-key-mode 0)
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (require 'vlf-setup)
  (yas-global-mode 1)
  (global-company-mode)
  (global-evil-mc-mode 1)
  (global-centered-cursor-mode  1)
  (add-hook 'after-init-hook #'global-flycheck-mode) ;; turn on flychecking globally
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;; keymap ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (global-set-key (kbd "M-%") 'phi-replace-query)
  (global-set-key (kbd "C-s") 'phi-search)
  (spacemacs/declare-prefix "c" "my-git-prefix")
  (spacemacs/set-leader-keys "gcc" 'magit-commit)
  (spacemacs/set-leader-keys "thV" 'js2-highlight-vars-mode)
  (spacemacs/set-leader-keys "gcs" 'magit-commit-squash)
  (spacemacs/set-leader-keys "gcS" 'magit-commit-instant-squash)
  (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)
  (global-set-key (kbd "S-s-<mouse-1>") 'mc/add-cursor-on-click)
  (evil-global-set-key 'normal (kbd "C-w z") 'zoom-window-zoom)
  (define-key evil-insert-state-map (kbd "C-v") 'forward-word)
  (define-key evil-insert-state-map (kbd "C-k") 'backward-word)
  (define-key evil-insert-state-map (kbd "C-d") 'delete-forward-char)
  (define-key evil-insert-state-map (kbd "C-n") 'mc/mark-next-like-this-word)
  (define-key evil-insert-state-map (kbd "C-p") 'mc/mark-previous-like-this-word)
  (define-key evil-insert-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
  (define-key evil-insert-state-map (kbd "C-a") 'mwim-beginning-of-code-or-line)
  (define-key evil-visual-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
  (define-key evil-visual-state-map (kbd "C-a") 'mwim-beginning-of-code-or-line)
  (define-key evil-insert-state-map (kbd "C-l") 'evil-complete-next)
  (define-key evil-insert-state-map (kbd "C-S-n") 'mc/skip-to-next-like-this)
  (define-key evil-insert-state-map (kbd "C-S-p") 'mc/skip-to-previous-like-this)
  (define-key evil-insert-state-map [(control return)] 'mc/mark-all-dwim)
  (global-set-key (kbd "C-SPC") 'swiper)
  (global-set-key (kbd "^@") 'swiper)
  ;;;;;C-h
  (define-key evil-normal-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
  (define-key evil-normal-state-map "e" 'evil-avy-goto-char-2)
  (keyboard-translate ?\C-h ?\C-?)
  (global-set-key [(control ?h)] 'delete-backward-char)
  ;;;;;; jzz kzz
  ;; (evil-define-motion evil-jzz (count)
  ;;   "j to jzz"
  ;;   :type line
  ;;   (evil-next-line (or count 1))
  ;;   (evil-scroll-line-to-center nil))
  ;; (evil-define-motion evil-kzz (count)
  ;;   "k to kzz"
  ;;   :type line
  ;;   (evil-previous-line (or count 1))
  ;;   (evil-scroll-line-to-center nil))
  ;; (define-key evil-normal-state-map (kbd "j") 'evil-jzz)
  ;; (define-key evil-normal-state-map (kbd "k") 'evil-kzz)
  (define-key evil-normal-state-map "zl" 'hs-hide-level)
  (define-key evil-insert-state-map (kbd "C-j") 'evil-ret-and-indent)
  (define-key evil-visual-state-map (kbd "g<") 'mc/edit-beginnings-of-lines)
  (define-key evil-visual-state-map (kbd "g>") 'mc/edit-ends-of-lines)

  (with-eval-after-load 'helm
    (dolist (keymap (list helm-find-files-map helm-read-file-map))
      (define-key keymap (kbd "<DEL>") 'helm-find-files-up-one-level)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;; lisp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (setq slime-contribs '(slime-fancy))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;; org ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq-default dotspacemacs-configuration-layers
                '((org :variables org-projectile-file "plans.org")))
  ;; (setq org-projectile-file '("plans.org"))
  (with-eval-after-load 'org-agenda
    (require 'org-projectile)
    (setq org-agenda-files (append org-agenda-files (org-projectile:todo-files))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;; javascript ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; (eval-after-load 'js2-mode
  ;;   '(add-hook 'js2-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix))))
  (setq babel-repl-cli-program "~/.npm-packages/bin/babel-node")
  (defun myfunc/js2-which-function ()
    ;; clean the imenu cache
    ;; @see http://stackoverflow.com/questions/13426564/how-to-force-a-rescan-in-imenu-by-a-function
    (setq imenu--index-alist nil)
    (which-function-mode t)
    (which-function))

  (defun my-js2-mode-hook ()
    (lambda ()
      (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
      (local-set-key (kbd "C-M-x") 'js-send-last-sexp-and-go)
      (local-set-key (kbd "C-c b") 'js-send-buffer)
      (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go)
      (local-set-key (kbd "C-c l") 'js-load-file-and-go)
      ))

  (add-hook 'js2-mode-hook 'my-js2-mode-hook)
  (add-hook 'js2-mode-hook #'jscs-indent-apply)
  ;; (add-hook 'js2-mode-hook #'jscs-fix-run-before-save)
  (setq inferior-js-program-command "node")
  (setq inferior-js-program-arguments '("--interactive"))
  (setq inferior-js-mode-hook
        (lambda ()
          ;; We like nice colors
          (ansi-color-for-comint-mode-on)))

  (add-hook 'js2-mode-hook
            (lambda ()
              (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
              (local-set-key (kbd "C-M-x") 'js-send-last-sexp-and-go)
              (local-set-key (kbd "C-c b") 'js-send-buffer)
              (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go)
              (local-set-key (kbd "C-c l") 'js-load-file-and-go)
              ))


  ;;;;;;;;;;;;; web-mode ;;;;;;;;;;;;;
  ;; adjust indents for web-mode to 2 spaces
  (defun my-web-mode-hook ()
    "Hooks for Web mode. Adjust indents"
  ;;; http://web-mode.org/
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2))
  (add-hook 'web-mode-hook  'my-web-mode-hook)
  ;; for better jsx syntax-highlighting in web-mode
  ;; - courtesy of Patrick @halbtuerke
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it))

  ;;;;;;;;;;;;; flycheck ;;;;;;;;;;;;;
  (setq flycheck-eslint-rules-directories '("/Users/Rashawn"))
  (setq-default save-place t)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-add-mode 'javascript-eslint 'js-mode)
  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck")
  ;;;;;;;;;;;; settings ;;;;;;;;;;;;;;
  (setq-default js2-basic-offset 2)
  (setq-default js-indent-level 2)
  ;; http://rejeep.github.io/emacs/javascript/js2-mode/yasnippet/2009/06/14/js2-mode-and-yasnippet.html
  (eval-after-load 'js2-mode
    '(progn
       (define-key js2-mode-map (kbd "TAB")
         (lambda()
           (interactive)
           (let ((yas/fallback-behavior 'return-nil))
             (unless (yas/expand)
               (indent-for-tab-command)
               (if (looking-back "^\s*")
                   (back-to-indentation))))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;; settings ;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq evil-move-cursor-back nil) ;;cursor don't go back
  (setq save-place-file "~/.emacs.d/saveplace") ;;remeber cursor location on reoppening
  (setq-default evil-escape-key-sequence "kj")
  (setq-default dotspacemacs-persistent-server t)
  (setq auto-indent-indent-style 'conservative)
  (setq auto-indent-style 'conservative)

  (setq org-directory '("~/Dropbox/org/*.org"))
  (setq org-agenda-files '("~/Dropbox/org"))
  (setq org-default-notes-file '("~/Dropbox/org/notes.org"))
  (setq auto-indent-on-visit-file t)
  ;; TODO
  (setq-default dotspacemacs-configuration-layers
                '((shell :variables shell-default-term-shell "/bin/zsh")))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;; others ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; uncomment these to enable flx
  ;; (setq ivy-re-builders-alist
  ;;       '((t . ivy--regex-fuzzy)))
  (key-chord-mode 1) ;; if you're not already enabling key-chord-mode
  (buffer-flip-mode)
  (buffer-flip-set-keys 'buffer-flip-keys "u8*")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;; irc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (load-file "~/Dropbox/org/auth.el")
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-backends
   (quote
    (company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-cmake company-capf company-files
                  (company-dabbrev-code company-gtags company-etags company-keywords)
                  company-oddmuse company-dabbrev)))
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "938d8c186c4cb9ec4a8d8bc159285e0d0f07bad46edf20aa469a89d0d2a586ea" "14f0fbf6f7851bfa60bf1f30347003e2348bf7a1005570fd758133c87dafe08f" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(debug-on-quit nil)
 '(diary-file "~/Dropbox/org/diary")
 '(dired-hide-details-hide-information-lines nil)
 '(dired-hide-details-hide-symlink-targets nil)
 '(dired-use-ls-dired nil)
 '(evil-escape-unordered-key-sequence t)
 '(evil-want-Y-yank-to-eol t)
 '(flycheck-check-syntax-automatically (quote (save mode-enabled)))
 '(flycheck-disabled-checkers (quote (javascript-jshint javascript-jscs)))
 '(gc-cons-threshold 800000)
 '(guide-key/guide-key-sequence (quote ("t")))
 '(helm-M-x-fuzzy-match t)
 '(helm-etags-fuzzy-match t)
 '(js2-mode-show-strict-warnings nil)
 '(jscs-fix-show-errors nil)
 '(large-file-warning-threshold 1000000)
 '(magit-diff-highlight-trailing nil)
 '(magit-display-buffer-function (quote magit-display-buffer-fullcolumn-most-v1))
 '(magit-popup-show-common-commands t)
 '(markdown-command "multimarkdown")
 '(org-capture-templates
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
      "* TODO %?
 %a
 %U")
     ("t" "TODOs" entry
      (file "~/Dropbox/org/gtd.org")
      "* TODO %?           %^G
 %U"))))
 '(org-datetree-add-timestamp (quote inactive))
 '(org-projectile:allow-tramp-projects t)
 '(org-projectile:capture-template "*** TODO %?
%a")
 '(org-projectile:projects-file "~/Dropbox/org/projects.org")
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (geiser ample-theme zenburn-theme fzf buffer-flip rcirc-notify rcirc-color eslint-fix babel-repl slime which-key wgrep smex ivy-hydra counsel-projectile counsel swiper ivy prodigy imenu-list mu4e-maildirs-extension mu4e-alert ht js2-highlight-vars jss jscs phi-search anything all-ext operate-on-number slim-mode elisp-slime-nav zoom-window js-comint go-guru go-eldoc company-go go-mode powershell solarized-theme reveal-in-osx-finder pbcopy osx-trash osx-dictionary launchctl evil-snipe yapfify xterm-color web-mode web-beautify vlf tagedit smeargle shell-pop scss-mode sass-mode pyvenv pytest pyenv-mode py-isort pug-mode pip-requirements orgit org-projectile org-present org org-pomodoro alert log4e gntp org-download mwim multi-term mmm-mode markdown-toc markdown-mode magit-gitflow livid-mode skewer-mode simple-httpd live-py-mode less-css-mode json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc hy-mode htmlize helm-pydoc helm-gitignore helm-css-scss helm-company helm-c-yasnippet haml-mode gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gh-md flyspell-correct-helm flyspell-correct flycheck-pos-tip pos-tip flycheck evil-magit magit magit-popup git-commit with-editor eshell-z eshell-prompt-extras esh-help emmet-mode diff-hl cython-mode company-web web-completion-data company-tern dash-functional tern company-statistics company-anaconda company coffee-mode auto-yasnippet yasnippet auto-dictionary anaconda-mode pythonic ac-ispell auto-complete spinner adaptive-wrap ws-butler window-numbering volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint info+ indent-guide ido-vertical-mode hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight dumb-jump f s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed dash aggressive-indent ace-window ace-link ace-jump-helm-line helm avy helm-core popup async quelpa package-build spacemacs-theme)))
 '(rcirc-default-full-name "yqrashawn")
 '(rcirc-default-nick "yqrashawn")
 '(rcirc-default-user-name "yqrashawn")
 '(undo-tree-auto-save-history t)
 '(undo-tree-history-directory-alist (quote (("." . "~/.emacs.d/.cache/undo"))))
 '(user-full-name "yqrashawn")
 '(visible-bell nil)
 '(vlf-application (quote dont-ask))
 '(which-key-allow-imprecise-window-fit t)
 '(which-key-dont-use-unicode t)
 '(which-key-echo-keystrokes 0.01)
 '(which-key-idle-delay 0.02)
 '(which-key-mode nil)
 '(which-key-popup-type (quote minibuffer))
 '(which-key-show-prefix (quote bottom))
 '(which-key-sort-uppercase-first t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
