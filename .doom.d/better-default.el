;;; better-defaults.el -*- lexical-binding: t; -*-

(pushnew! global-hl-line-modes 'dired-mode 'occur-mode 'grep-mode)
(cl-callf2 delq 'prog-mode global-hl-line-modes)
(setq! kmacro-ring-max 8
       gc-cons-percentage 0.2
       garbage-collection-messages t
       use-short-answers t
       max-specpdl-size 10000
       max-lisp-eval-depth 10000
       save-interprogram-paste-before-kill nil
       save-silently t
       echo-keystrokes 1e-6
       split-window-keep-point t
       require-final-newline nil
       mode-require-final-newline nil
       confirm-kill-processes nil
       browse-url-generic-program "open"
       ;; browse-url-browser-function #'eww-browse-url
       ;; browse-url-secondary-browser-function #'browse-url-default-browser
       browse-url-secondary-browser-function #'eww-browse-url
       make-backup-files nil
       mac-command-modifier 'super
       mac-right-command-modifier 'super
       mac-option-modifier 'meta
       mac-right-option-modifier 'meta

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
       ;; url-proxy-services
       ;; '(("http" . "127.0.0.1:6152")
       ;;   ("https" . "127.0.0.1:6153"))
       url-proxy-services nil
       blink-matching-paren t
       ;; blink-matching-paren 'jump
       blink-matching--overlay (let ((ol (make-overlay (point) (point) nil t)))
                                 (overlay-put ol 'face 'custom-invalid)
                                 (delete-overlay ol)
                                 ol)
       ispell-dictionary "en_US"
       ispell-personal-dictionary (if (file-exists-p "~/Dropbox/sync/personal_dict")
                                      "~/Dropbox/sync/personal_dict")
       dired-quick-sort-suppress-setup-warning t
       insert-directory-program "/run/current-system/sw/bin/ls"
       comint-buffer-maximum-size 100000
       package-native-compile t)

(after! recentf
  (setq! recentf-keep '(recentf-keep-default-predicate tramp-tramp-file-p)
         recentf-max-saved-items 2000))

(pushnew! auto-mode-alist '("\\.gitconfig.*\\'" . gitconfig-mode))
(pushnew! auto-mode-alist '("\\.gitignore.*\\'" . gitignore-mode))
(pushnew! auto-mode-alist '("\\.git/info/exclude\\'" . gitignore-mode))

(add-hook! 'delete-terminal-functions (recentf-save-list))

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

(use-package side-notes
  :defer t
  :init
  (setq! side-notes-file "notes.side.org"
         side-notes-display-alist '((side . right)
                                    (window-width . 1000))))

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
      (if (or (and (boundp 'lispy-mode) lispy-mode)
              (and (boundp 'lispyville-mode) lispyville-mode))
          (setq-local outline-regexp +emacs-lisp-outline-regexp)
        (progn
          (setq-local
           +outline-regexp-start (+outline-chomp (or comment-start "#"))
           +outline-regexp-body
           (concat "\\(\\(" +outline-regexp-start "\\)" "+\\|" "\s?\\(#\\|;\\|\*\\)+" "\\)"))
          (make-local-variable 'outline-regexp)
          (setq outline-regexp
                (concat
                 "[ \t]*" +outline-regexp-start +outline-regexp-body
                 (+outline-chomp comment-end)
                 " [^ \t\n]"))))))
  (add-hook! outline-minor-mode
             '+outline-minor-mode-setup-regexp
             '+outline-minor-mode-disable-evil-tab))

(defadvice! +doom/switch-to-scratch-buffer (orig-fn &optional arg project-p)
  :around #'doom/switch-to-scratch-buffer
  (apply orig-fn (not arg) project-p))


;;; +word-wrap
(setq! +word-wrap-fill-style 'soft
       ;; for line number
       ;; https://codeberg.org/joostkremers/visual-fill-column/issues/4#issuecomment-416571
       visual-fill-column-width 90
       fill-column 80)
(pushnew! +word-wrap-disabled-modes 'minibuffer-mode)

(+global-word-wrap-mode +1)

;; (defadvice! ++word-wrap--enable-global-mode (orig-fn)
;;   :around #'+word-wrap--enable-global-mode
;;   (unless (derived-mode-p 'prog-mode)
;;     (funcall orig-fn)))

(set-popup-rules!
  '(("^\\*[Hh]elp" :slot 2 :side right :vslot -8 :size 0.35 :select t :quit current)
    ("^\\*info\\*$" :slot 2 :vslot 2 :side right :size 0.45 :select t :quit nil)
    ;; ("^\\*eww\\*$" :slot 2 :vslot 2 :side right :size 0.45 :select t :quit nil)
    ("^\\*Messages\\*$" :vslot -2 :size 0.5 :autosave t :quit t :ttl nil)
    ("^\\*Completions" :ignore t)
    ("^\\*Local variables\\*$" :vslot -1 :slot 1 :size +popup-shrink-to-fit)
    ("^\\*\\(?:[Cc]ompil\\(?:ation\\|e-Log\\)\\|Messages\\)" :vslot -2 :size 0.3 :autosave t :quit t :ttl nil)
    ("^\\*\\(?:doom \\|Pp E\\)"    ; transient buffers (no interaction required)
     :vslot -3 :size +popup-shrink-to-fit :autosave t :select ignore :quit t :ttl 0)
    ("^\\*doom:"                        ; editing buffers (interaction required)
     :vslot -4 :size 0.35 :autosave t :select t :modeline t :quit nil :ttl t)
    ("^\\*doom:\\(?:v?term\\|e?shell\\)-popup" ; editing buffers (interaction required)
     :vslot -5 :size 0.35 :select t :modeline nil :quit nil :ttl nil)
    ("^\\*\\(?:Wo\\)?Man " :vslot -6 :size 0.45 :select t :quit t :ttl 0)
    ("^\\*Calc" :vslot -7 :side bottom :size 0.4 :select t :quit nil :ttl 0)
    ("^\\*Customize" :slot 2 :side right :size 0.5 :select t :quit nil)
    ("^ \\*undo-tree\\*" :slot 2 :side left :size 20 :select t :quit t)
    ("^\\*Apropos" :slot 2 :vslot -8 :size 0.35 :select t)
    ("^\\*declutter\*" :ignore t)
    ("^\\*Error\\*" :select nil :quit t :side bottom :size 0.3)
    ("^\\*wclock\\*" :select t :quit t :side bottom :size 0.3)))

(use-package! ix
  :commands (ix)
  :config
  (setq! ix-user "yq"
         ix-token (-> (auth-source-search :host "ix.io"
                                          :user "yq")
                      car
                      (plist-get :secret))))

;; (use-package! fancy-dabbre
;;   :hook (doom-first-input . global-fancy-dabbrev-mode)
;;   :init
;;   (setq! fancy-dabbrev-preview-delay 0
;;          fancy-dabbrev-preview-context 'everywhere
;;          fancy-dabbrev-expansion-on-preview-only t))

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

;; double C-g
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

(after! wgrep
  (advice-remove #'wgrep-abort-changes #'+popup-close-a)
  (advice-remove #'wgrep-finish-edit #'+popup-close-a))

;; (setq! xref-search-program 'rg)
(setq! xref-search-program 'ugrep)

(add-hook! 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(setq frame-title-format '("%b"))

(use-package! proced-narrow :after proced)

(defadvice! +doom-init-all-the-icons-fonts-h (_)
  :around #'doom-init-all-the-icons-fonts-h
  (when (fboundp 'set-fontset-font)
    (dolist (font (list
                   "PragmataPro Mono Liga"
                   "Weather Icons"
                   "github-octicons"
                   "FontAwesome"
                   "all-the-icons"
                   "file-icons"
                   "Material Icons"))
      (set-fontset-font t 'unicode font nil 'append))))

(add-hook! 'doom-first-file-hook
  (if (boundp 'pixel-scroll-precision-mode)
      (pixel-scroll-precision-mode t)))

(use-package! keycast
  :commands keycast-mode
  :config
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (progn
          (add-hook 'pre-command-hook 'keycast--update t)
          (add-to-list 'global-mode-string '("" mode-line-keycast " ")))
      (remove-hook 'pre-command-hook 'keycast--update)
      (setq global-mode-string (remove '("" mode-line-keycast " ") global-mode-string))))

  (setq keycast-substitute-alist '((evil-next-line nil nil)
                                   (evil-previous-line nil nil)
                                   (evil-forward-char nil nil)
                                   (evil-backward-char nil nil)
                                   (ivy-done nil nil)
                                   (self-insert-command nil nil)))

  (custom-set-faces!
    '(keycast-command :inherit doom-modeline-debug
      :height 0.9)
    '(keycast-key :inherit custom-modified
      :height 1.1
      :weight bold)))

(use-package! unmodified-buffer
  :hook (doom-first-file . unmodified-buffer-global-mode))

;; TODO remove this once it's remove from core-editor.el
;; https://github.com/hlissner/doom-emacs/issues/6127
(undefadvice! doom--fix-helpful--autoloaded-p (fn &rest args)
  :around #'helpful--autoloaded-p
  (letf! (defun help-fns--autoloaded-p (sym _)
           (funcall help-fns--autoloaded-p sym))
    (apply fn args)))

(after! dirvish
  (setq! dirvish-preview-dispatchers '()))

;; (after! dirvish
;;   (setq!
;;    ;; dirvish-keep-alive-on-quit nil
;;    ;; dirvish-header-line-format nil
;;    dirvish-attributes '(file-size git-msg))
;;   (dirvish-define-preview eza (file)
;;     "Use `eza' to generate directory preview."
;;     (when (file-directory-p file)       ; we only interest in directories here
;;       `(shell . ("eza" "--color=always" "-al" ,file)))) ; use the output of `eza' command as preview
;;   (pushnew! dirvish-preview-dispatchers 'eza)

;;   (defadvice! ++dired/quit-all ()
;;     :before #'+dired/quit-all
;;     (mapc #'kill-buffer (doom-buffers-in-mode 'dirvish-mode)))
;;   (require 'dirvish-vc))

(setq! image-use-external-converter t)

(use-package! vterm
  :defer t
  :init
  (setq! vterm-buffer-name-string "*vterm %s*"))

(use-package! textsize
  :defer t
  ;; :hook (doom-first-file . textsize-mode)
  :init
  (setq! textsize-default-points 18))

(after! dired
  (setq! dired-listing-switches "-l --all --human-readable --time-style=long-iso --no-group --sort=time -v --group-directories-first"))

(add-hook! dired-mode #'dired-async-mode)

(after! comint
  (defun +comint-send-invisible-with-sudo-pwd (&rest args)
    (let ((pwd (++password!))
          (proc (get-buffer-process (current-buffer))))
      (funcall comint-input-sender proc pwd)
      (advice-remove 'comint-send-invisible '+comint-send-invisible-with-sudo-pwd)))

  (defun comint-watch-for-password-prompt (string)
    "Prompt in the minibuffer for password and send without echoing.
Looks for a match to `comint-password-prompt-regexp' in order
to detect the need to (prompt and) send a password.  Ignores any
carriage returns (\\r) in STRING.

This function could be in the list `comint-output-filter-functions'."
    (when (let ((case-fold-search t))
            (string-match comint-password-prompt-regexp
                          (string-replace "\r" "" string)))
      (with-current-buffer (current-buffer)
        (let ((comint--prompt-recursion-depth
               (1+ comint--prompt-recursion-depth)))
          (if (> comint--prompt-recursion-depth 10)
              (message "Password prompt recursion too deep")
            (comint-send-invisible
             (string-trim string "[ \n\r\t\v\f\b\a]+" "\n+"))))))))

(after! hippie-exp
  (setq! hippie-expand-try-functions-list
         '(try-complete-file-name-partially
           try-complete-file-name
           try-expand-all-abbrevs
           try-expand-dabbrev-visible
           try-expand-dabbrev
           try-expand-dabbrev-all-buffers
           try-expand-dabbrev-from-kill
           try-complete-lisp-symbol-partially
           try-complete-lisp-symbol)))

(use-package! detached
  :hook (doom-first-input . detached-init)
  :init
  (defun +detached-state-transition-notifications-message (session)
    (when (executable-find "alerter")
      (let ((status (detached-session-status session))
            (host (detached-session-host-name session)))
        (call-process-shell-command
         (format! "alerter -message \"%s\" -timeout 30 -group dtach -sender org.gnu.Emacs -sound default -title \"%s\"&"
                  (detached-session-command session)
                  (pcase status
                    ('success (format "Detached finished [%s]" host))
                    ('failure (format "Detached failed [%s]" host))))


         nil nil))))

  (defadvice! +detached-rerun-session (session &optional _arg)
    :after #'detached-rerun-session
    (detached-kill-session session t))

  (setq! async-shell-command-display-buffer nil
         detached-show-session-context t
         detached-command-format '(:width 50 :padding 4 :function detached-command-str)
         detached-metadata-annotators-alist '((branch . detached--metadata-git-branch))
         detached-list-config
         `((:name "Command" :function detached-list--command-str :length 30)
           (:name "Status" :function detached-list--status-str :length 10)
           (:name "Host" :function detached--host-str :length 10 :face detached-host-face)
           (:name "Directory" :function detached--working-dir-str :length 30 :face detached-working-dir-face)
           (:name "Metadata" :function detached--metadata-str :length 30 :face detached-metadata-face)
           (:name "Duration" :function detached--duration-str :length 10 :face detached-duration-face)
           (:name "Created" :function detached--creation-str :length 20 :face detached-creation-face))
         detached-notification-function #'+detached-state-transition-notifications-message)
  (set-popup-rule! "^\\*detached-session-output\\*" :side 'right :size 0.4 :vslot 97 :quit t)
  (set-popup-rule! "^\\*detached-list\\*" :side 'right :size 0.6 :vslot 98 :quit t)
  (set-popup-rule! "^\\*Detached Shell Command\\*.*" :side 'right :size 0.35 :vslot 98 :quit t)
  :config
  (defadvice! +detached-create-session (fn command)
    :around #'detached-create-session
    (let ((sessions (detached-get-sessions)))
      (if-let ((dup-session (seq-find
                             (lambda (session)
                               (and
                                (string= command (detached-session-command session))
                                (string= default-directory
                                         (expand-file-name (detached-session-working-directory session)))))
                             (detached-get-sessions))))
          (let ((buffer (get-buffer-create "*detached-list*")))
            (with-current-buffer buffer
              (detached-list-sessions)
              (bury-buffer)
              (when (detached-session-active-p dup-session)
                (detached-session-kill dup-session))
              (run-with-timer 2 nil #'detached-start-session dup-session)))
        (funcall fn command)))))

(use-package! gc-buffers :hook (doom-first-buffer . gc-buffers-mode))


;; https://github.com/doomemacs/doomemacs/issues/5182
(defun doom--get-default-font (&optional font)
  (cl-some #'doom--font-to-spec
           (delq
            nil (list font
                      (unless (equal font doom-font) doom-font)
                      (face-font 'default t)
                      (with-temp-buffer (face-font 'default))))))

(defun doom--font-to-spec (font)
  (cond ((fontp font)   (font-spec :name (font-xlfd-name doom-font)))
        ((stringp font) (font-spec :name font))
        ((vectorp font) (font-spec :name (x-compose-font-name font)))))

(defadvice! fixed-doom-normalize-font (font)
  :override #'doom-normalize-font
  (cl-check-type font (or font string vector))
  (let ((font (doom--font-to-spec font))
        (default-font (doom--get-default-font font)))
    (dolist (prop (list (cons :weight 'normal)
                        (cons :slant  'normal)
                        (cons :width  'normal)
                        (cons :size   (font-get default-font :size))))
      (cl-destructuring-bind (key . val) prop
        (when (or (font-get font key))
          (font-put font key (or (font-get default-font key) val)))))
    font))

(defadvice! fixed-doom-adjust-font-size (increment &optional fixed-size-p font-alist)
  :override #'doom-adjust-font-size
  (unless (display-multi-font-p)
    (user-error "Cannot resize fonts in terminal Emacs"))
  (condition-case-unless-debug e
      (let (changed)
        (dolist (sym '((doom-font . default)
                       (doom-serif-font . fixed-pitch-serif)
                       (doom-variable-pitch-font . variable-pitch))
                     (when changed
                       (doom-init-fonts-h 'reload)
                       t))
          (cl-destructuring-bind (var . face) sym
            (if (null increment)
                (when (get var 'initial-value)
                  (set var (get var 'initial-value))
                  (put var 'initial-value nil)
                  (setq changed t))
              (let* ((original-font (or (symbol-value var)
                                        (face-font face t)
                                        (with-temp-buffer (face-font face))))
                     (font (doom-normalize-font original-font))
                     (dfont (or (if-let* ((remap-font (alist-get var font-alist))
                                          (remap-xlfd (doom-normalize-font remap-font)))
                                    remap-xlfd
                                  (purecopy font))
                                (error "Could not decompose %s font" var))))
                (let* ((step      (if fixed-size-p 0 (* increment doom-font-increment)))
                       (orig-size (font-get font :size))
                       (new-size  (if fixed-size-p increment (+ orig-size step))))
                  (cond ((<= new-size 0)
                         (error "`%s' font is too small to be resized (%d)" var new-size))
                        ((= orig-size new-size)
                         (user-error "Could not resize `%s' for some reason" var))
                        ((setq changed t)
                         (unless (get var 'initial-value)
                           (put var 'initial-value original-font))
                         (font-put dfont :size new-size)
                         (set var dfont)))))))))
    (error
     (ignore-errors (doom-adjust-font-size nil))
     (signal (car e) (cdr e)))))

(unless (fboundp 'indent-buffer)
  (defalias 'indent-buffer #'pp-buffer))

;; (setq!
;;   langtool-http-server-host "api.languagetoolplus.com"
;;   langtool-http-server-port 443
;;   ;; langtool-language-tool-server-jar "/run/current-system/sw/bin/languagetool"
;;   langtool-java-bin (executable-find "java")
;;   ;; langtool-bin nil
;;   langtool-bin (executable-find "languagetool-commandline"))

(after! languagetool-server
  (defadvice! +languagetool-server-parse-request (orig-fn)
    :around #'languagetool-server-parse-request
    "Return a assoc-list with LanguageTool Server request arguments parsed.

Return the arguments as an assoc list of string which will be
used in the POST request made to the LanguageTool server."
    (let (arguments)

      ;; Appends the correction language information
      (push (list "language" languagetool-correction-language) arguments)

      ;; Appends the mother tongue information
      (when (stringp languagetool-mother-tongue)
        (push (list "motherTongue" languagetool-mother-tongue) arguments))

      ;; (unless (stringp +languagetool-token-v2)
      ;;   (setq +languagetool-token-v2
      ;;     (-> (auth-source-search :host "api.languagetoolplus.com"
      ;;           :user languagetool-username)
      ;;       car
      ;;       (plist-get :api_token))))

      ;; Add LanguageTool Preamium features
      (when (stringp +languagetool-token-v2)
        ;; (push (list "apiKey" languagetool-api-key) arguments)
        (push (list "tokenV2" +languagetool-token-v2) arguments))

      (when (stringp languagetool-username)
        (push (list "username" languagetool-username) arguments))

      ;; Appends LanguageTool suggestion level information
      (when (stringp languagetool-suggestion-level)
        (push (list "level" languagetool-suggestion-level) arguments))

      ;; Appends the disabled rules
      (let ((rules))
        ;; Global disabled rules
        (setq rules (string-join (append languagetool-disabled-rules languagetool-local-disabled-rules) ","))
        (unless (string= rules "")
          (push (list "disabledRules" rules) arguments)))

      ;; Add the buffer contents
      (push (list "text" (buffer-substring-no-properties (point-min) (point-max))) arguments))))

(use-package! languagetool
  ;; :hook ((org-mode markdown-mode rst-mode asciidoc-mode latex-mode LaTeX-mode) . languagetool-server-mode)
  :init
  (setq!
   languagetool-api-key "foo"
   languagetool-username user-mail-address
   languagetool-server-url "https://api.languagetoolplus.com"
   languagetool-server-port 443
   languagetool-mother-tongue "zh-CN"))

(after! emacs-everywhere
  (setq! emacs-everywhere-paste-command
         (pcase emacs-everywhere--display-server
           ('quartz (list "osascript" "-e" "delay 0.3 \n tell application \"System Events\" to keystroke \"v\" using command down"))
           ('x11 (list "xdotool" "key" "--clearmodifiers" "Shift+Insert"))
           ((or 'wayland 'unknown)
            (list "notify-send"
                  "No paste command defined for emacs-everywhere"
                  "-a" "Emacs" "-i" "emacs"))))
  (defadvice! +emacs-everywhere-app-info-osx (orig-fn)
    :around #'emacs-everywhere-app-info-osx
    "Return information on the active window, on osx."
    (emacs-everywhere-ensure-oscascript-compiled)
    (let ((default-directory emacs-everywhere--dir))
      (let ((app-name (emacs-everywhere-call
                       "osascript" "app-name"))
            (window-title (emacs-everywhere-call
                           "osascript" "window-title"))
            (window-geometry (mapcar #'string-to-number
                                     (split-string
                                      (emacs-everywhere-call
                                       "osascript" "window-geometry") ", "))))
        (make-emacs-everywhere-app
         :id (if (string= "firefox" app-name)
                 "Firefox Developer Edition"
               app-name)
         :class app-name
         :title window-title
         :geometry window-geometry)))))


(setq! consult-tramp-method "sshx"
       ;; tramp-ssh-controlmaster-options
       ;; "-o ControlMaster=auto -o ControlPath=tramp.%%C -o ControlPersist=600"
       )

(after! tramp
  (pushnew! tramp-connection-properties
            (list
             (regexp-quote "/sshx:studio")
             "remote-shell" "/etc/profiles/per-user/yqrashawn/bin/zsh")))

(pushnew! vc-directory-exclusion-list "node_modules")

(after! osx-trash
  (defadvice! +osx-trash-move-file-to-trash (orig-fn file-name)
    :around #'osx-trash-move-file-to-trash
    (let ((file-name (expand-file-name file-name)))
      (with-temp-buffer
        (let ((retcode (condition-case nil
                           (call-process "trash" nil t nil "-f" file-name)
                         (file-error
                          (call-process "osascript" nil t nil
                                        osx-trash-script-file file-name)))))
          (unless (equal retcode 0)
            (error "Failed to trash %S: %S" file-name (buffer-string))))))))

(after! recentf
  (pushnew! recentf-exclude "^/nix" ;; #'file-remote-p "^/ssh:"
            ))

(defun +gptel-save-buffer (&rest args)
  (interactive)
  (when-let ((buf (current-buffer)))
    (with-current-buffer buf
      (gptel-context-remove-all nil)
      (if buffer-file-name
          (save-buffer)
        (write-file
         (format
          (expand-file-name
           "~/Dropbox/sync/gptel/gptel-%s.org")
          (format-time-string
           "%Y%m%d-%H%M%S-%3N")))))))

(defun +gptel-kill-default-buffer ()
  (interactive)
  (when-let ((buf (get-buffer gptel-default-session)))
    (kill-buffer buf)))

(use-package! gptel
  :commands (gptel)
  :init
  (setq! gptel-api-key +open-ai-api-key
         gptel-default-mode 'org-mode
         gptel-temperature 0.8)
  ;; (defadvice! +gptel-cleanup-default-buffer (&rest args)
  ;;   :before #'gptel
  ;;   (+gptel-kill-default-buffer))
  (set-popup-rule! "^\\*ChatGPT\\*$" :side 'right :size 0.4 :vslot 100 :quit t)
  :config
  (require 'magit)
  (add-hook! 'gptel-mode-hook
    (defun +gptel-mode-setup-kill-buffer-hook ()
      (add-hook! 'kill-buffer-hook :local
        (defun +gptel-mode-reset-on-buffer-killed-h ()
          (with-current-buffer (current-buffer)
            (gptel-context-remove-all nil))))))
  (defun +gptel-context-add-buffer (b)
    (let ((start (with-current-buffer b (point-min)))
          (end (with-current-buffer b (point-max))))
      (gptel-context--add-region b start end t)))

  (defadvice! +before-gptel-make-fsm (&optional args)
    :before #'gptel-send
    (require 'gptel-context)
    (gptel-context-remove-all nil)
    (+gptel-context-add-buffer (+magit-wip-diff-n-min-buffer 5))
    (dolist (b (seq-filter
                (lambda (b)
                  (and
                   (not (string= (buffer-name b) "*Messages*"))
                   (with-current-buffer b
                     (not gptel-mode))))
                (mapcar 'window-buffer (window-list))))
      (+gptel-context-add-buffer b))
    (when-let ((root (doom-project-root)))
      (dolist (f +llm-project-default-files)
        (when-let ((b (get-file-buffer (format "%s%s" root f))))
          (+gptel-context-add-buffer b)))))

  (setq! gptel--openrouter
         (gptel-make-openai "OpenRouter"
           :host "openrouter.ai"
           :endpoint "/api/v1/chat/completions"
           :stream t
           :key +openrouter-api-key
           :models gptel--openrouter-models))
  (setq! gptel-model 'claude-sonnet-4)
  (setq! gptel--gh-copilot
         (gptel-make-openai "Github Copilot"
           :protocol "http"
           :host "localhost:4141"
           :endpoint "/chat/completions"
           :stream t
           :key "no-key-required"
           :models gptel--gh-copilot-models))
  (setq! gptel-post-response-functions nil
         gptel-backend gptel--openrouter
         gptel-backend gptel--gh-copilot)
  (add-hook! 'gptel-post-response-functions '+gptel-save-buffer)
  (setq gptel-directives
        '((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
          (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
          (writing . "You are a large language model and a writing assistant. Respond concisely.")
          (chat . "You are a large language model and a conversation partner. Respond concisely.")))

  (gptel-make-tool
   :function (lambda () "15.4")
   :name "get_current_macos_version"
   :description "Get current macos version"
   :category "utils"
   :args '()))

(use-package shrface
  :defer t
  :config
  (shrface-basic)
  (shrface-trial)
  (shrface-default-keybindings) ; setup default keybindings
  (setq shrface-href-versatile t))

(use-package eww :defer t)

(after! eww
  (add-hook! 'eww-after-render-hook (ignore-errors (eww-readable)))
  (add-hook! 'eww-after-render-hook 'mixed-pitch-mode)
  (add-hook! 'eww-after-render-hook 'writeroom-mode))

;; https://stackoverflow.com/questions/60812866/emacs-gpg-pinentry-el-for-authentication
(use-package! pinentry
  :hook (doom-after-init . pinentry-start)
  :init
  (setq! epg-pinentry-mode 'loopback
         epg-gpg-home-directory (expand-file-name "~/.gnupg"))
  ;; (setq! epg-debug t)
  :config
  (shell-command "gpg-connect-agent reloadagent /bye >/dev/null")
  (shell-command "gpg-connect-agent updatestartuptty /bye >/dev/null")
  ;; (shell-command "gpgconf --reload gpg-agent >/dev/null" nil nil)
  )

(after! time
  (setq! zoneinfo-style-world-list
         '(("America/Los_Angeles" "LA")
           ("America/New_York" "New York")
           ("UTC" "UTC")
           ("Europe/London" "London")
           ("Europe/Paris" "Paris")
           ("Asia/Calcutta" "Bangalore")
           ("Asia/Ho_Chi_Minh" "Vietnam")
           ("Asia/Shanghai" "Beijing")
           ("Asia/Tokyo" "Tokyo"))))

;; (use-package! org-ai
;;   :commands (org-ai-mode org-ai-global-mode)
;;   :hook (org-mode . org-ai-mode)
;;   :init
;;   (setq! org-ai-default-chat-model "gpt-4-1106-preview"
;;          org-ai-default-inject-sys-prompt-for-all-messages nil
;;          org-ai-openai-api-token +open-ai-api-key
;;          org-ai-default-chat-system-prompt "You are a helpful assistant inside Emacs. Respond concisely."))

(after! frame
  (add-function
   :after after-focus-change-function
   (lambda ()
     (unless (frame-focus-state)
       (run-with-timer 3.0 nil
                       (lambda ()
                         (unless (frame-focus-state)
                           (let (garbage-collection-messages)
                             (garbage-collect)))))))))

(defun rk/get-ffmpeg-device ()
  "Gets the list of devices available to ffmpeg.
The output of the ffmpeg command is pretty messy, e.g.
  [AVFoundation indev @ 0x7f867f004580] AVFoundation video devices:
  [AVFoundation indev @ 0x7f867f004580] [0] FaceTime HD Camera (Built-in)
  [AVFoundation indev @ 0x7f867f004580] AVFoundation audio devices:
  [AVFoundation indev @ 0x7f867f004580] [0] Cam Link 4K
  [AVFoundation indev @ 0x7f867f004580] [1] MacBook Pro Microphone
so we need to parse it to get the list of devices.
The return value contains two lists, one for video devices and one for audio devices.
Each list contains a list of cons cells, where the car is the device number and the cdr is the device name."
  (unless (string-equal system-type "darwin")
    (error "This function is currently only supported on macOS"))

  (let ((lines (string-split (shell-command-to-string "ffmpeg -list_devices true -f avfoundation -i dummy || true") "\n")))
    (cl-loop with at-video-devices = nil
             with at-audio-devices = nil
             with video-devices = nil
             with audio-devices = nil
             for line in lines
             when (string-match "AVFoundation video devices:" line)
             do (setq at-video-devices t
                      at-audio-devices nil)
             when (string-match "AVFoundation audio devices:" line)
             do (setq at-audio-devices t
                      at-video-devices nil)
             when (and at-video-devices
                       (string-match "\\[\\([0-9]+\\)\\] \\(.+\\)" line))
             do (push (cons (string-to-number (match-string 1 line)) (match-string 2 line)) video-devices)
             when (and at-audio-devices
                       (string-match "\\[\\([0-9]+\\)\\] \\(.+\\)" line))
             do (push (cons (string-to-number (match-string 1 line)) (match-string 2 line)) audio-devices)
             finally return (list (nreverse video-devices) (nreverse audio-devices)))))

(defun rk/find-device-matching (string type)
  "Get the devices from `rk/get-ffmpeg-device' and look for a device
matching `STRING'. `TYPE' can be :video or :audio."
  (let* ((devices (rk/get-ffmpeg-device))
         (device-list (if (eq type :video)
                          (car devices)
                        (cadr devices))))
    (cl-loop for device in device-list
             when (string-match-p string (cdr device))
             return (car device))))

(defcustom rk/default-audio-device nil
  "The default audio device to use for whisper.el and outher audio processes."
  :type 'string)

(defun rk/select-default-audio-device (&optional device-name)
  "Interactively select an audio device to use for whisper.el and other audio processes.
If `DEVICE-NAME' is provided, it will be used instead of prompting the user."
  (interactive)
  (let* ((audio-devices (cadr (rk/get-ffmpeg-device)))
         (indexes (mapcar #'car audio-devices))
         (names (mapcar #'cdr audio-devices))
         (name (or device-name (completing-read "Select audio device: " names nil t))))
    (setq rk/default-audio-device (rk/find-device-matching name :audio))
    (require 'whisper nil t)
    (when (boundp 'whisper--ffmpeg-input-device)
      (setq whisper--ffmpeg-input-device (format ":%s" rk/default-audio-device)))))

(use-package! whisper
  :commands (whisper-run whisper-file)
  :init
  (defun +whisper-default-lang-model ()
    (setq! whisper-model "medium.en"
           whisper-language "en"))
  (defun +whisper-zh-lang-model ()
    (setq! whisper-model "large-v3"
           whisper-language "zh"))
  :config
  (require 'cl-extra)
  (require 'subr-x)
  (add-hook! 'whisper-post-process-hook
    (lambda ()
      (thread-first (buffer-string)
                    (string-trim "\"" "\"")
                    (string-trim))))
  (setq! whisper-use-threads
         (thread-first "sysctl -n hw.ncpu"
                       (shell-command-to-string)
                       (string-trim)
                       (cl-parse-integer)
                       (- 2)))
  (defadvice! +whisper--ffmpeg-input-device (&optional arg)
    :before #'whisper-run
    (unless whisper--ffmpeg-input-device
      (call-interactively 'rk/select-default-audio-device)))
  (setq! whisper-install-directory (expand-file-name "~/.cache/"))
  (+whisper-zh-lang-model)
  (+whisper-default-lang-model))

(use-package! chatgpt-shell
  :commands (chatgpt-shell-prompt-compose chatgpt-shell-prompt)
  :init
  (setq! chatgpt-shell-openai-key +open-ai-api-key
         chatgpt-shell-model-temperature 0.6)
  :config
  (pushnew! chatgpt-shell-language-mapping '(("javascript". "jsx"))))

(use-package! memoize
  :defer t
  :after f
  ;; :config
  ;; (fset '+f-exists? (memoize (lambda (filename) (file-exists-p filename)) "30 minutes"))
  )

;; (use-package! kagi
;;   :commands (kagi-fastgpt-shell)
;;   :init
;;   (setq! kagi-api-token +kagi-api-token
;;          kagi-summarizer-engine "cecil"
;;          kagi-summarize-default-language "EN"
;;          kagi-summarize-cache t))

(use-package jinx
  :hook (doom-first-file . global-jinx-mode))

(defun +kitten (cmd &optional no-focus)
  (call-process-shell-command
   (format!
    "%s @ --to unix:/tmp/tkitty %s"
    (executable-find "kitten")
    cmd)
   nil 0)
  (unless no-focus
    (call-process-shell-command "open -a kitty.app" nil 0)))

(setq! +buffer-terminator-buffer-file-name-kill-list
       '("^/private/tmp/emacsclient\."))

(setq! +buffer-terminator-buffer-name-kill-list
       '("^\*Dirvish-preview"
         "^\*Embark Export:"))

(defun +buffer-visible-in-persp-p (buffer persp-name)
  "Check if BUFFER is visible in any window of the perspective PERSP."
  (save-window-excursion
    (+workspace-switch persp-name)
    (get-buffer-window buffer)))

(defun +buffer-terminator-pred ()
  (let* ((bname (buffer-name))
         (bfile (buffer-file-name)))
    (cond
     ((memq major-mode
            '(copilot-chat-prompt-mode
              copilot-chat-mode)) :keep)
     ((memq major-mode '(dired-mode)) :kill)
     ((and
       bfile
       (seq-some
        (lambda (re) (string-match-p re bfile))
        +buffer-terminator-buffer-file-name-kill-list))
      :kill)
     ((seq-some
       (lambda (re) (string-match-p re bname))
       +buffer-terminator-buffer-name-kill-list)
      :kill)
     (t nil))))

(use-package! buffer-terminator
  :hook (doom-first-file . buffer-terminator-mode)
  :config
  (setq!
   buffer-terminator-rules-alist
   '((call-function . +buffer-terminator-pred)
     ;; Retain special buffers (DO NOT REMOVE).
     ;; DO NOT REMOVE (keep-buffer-property . special) unless you know of what
     ;; you are doing.
     (keep-buffer-property . special)

     ;; Keep process buffers.
     ;; (Process buffers are buffers where an active process is running.)
     (keep-buffer-property . process)

     ;; Keep visible buffers (DO NOT REMOVE)
     ;; (Buffers currently displayed in a window.)
     (keep-buffer-property . visible)

     ;; Kill inactive buffers.
     ;; (This can be customized with `buffer-terminator-inactivity-timeout'
     ;; and `buffer-terminator-interval'.)
     ;; (kill-buffer-property . inactive)
     (return . :keep))))

;; quote “
;; quote ”

(after! persp-mode
  (setq persp-auto-save-opt 0))
