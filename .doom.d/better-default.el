;;; better-defaults.el -*- lexical-binding: t; -*-

(pushnew! global-hl-line-modes 'dired-mode 'occur-mode 'grep-mode)
(delq! 'prog-mode global-hl-line-modes)
(setq! kmacro-ring-max 8
       use-short-answers t
       max-specpdl-size 10000
       max-lisp-eval-depth 10000
       save-interprogram-paste-before-kill nil
       save-silently t
       echo-keystrokes 1e-6
       split-window-keep-point t
       require-final-newline nil
       mode-require-final-newline nil
       auto-window-vscroll nil
       confirm-kill-processes nil
       browse-url-secondary-browser-function 'eww-browse-url
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
       insert-directory-program "/run/current-system/sw/bin/ls")

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


(+global-word-wrap-mode +1)

(defadvice! ++word-wrap--enable-global-mode (orig-fn)
  :around #'+word-wrap--enable-global-mode
  (unless (derived-mode-p 'prog-mode)
    (funcall orig-fn)))

(set-popup-rules!
  '(("^\\*[Hh]elp" :slot 2 :side right :vslot -8 :size 0.35 :select t :quit current)
     ("^\\*info\\*$" :slot 2 :vslot 2 :side right :size 0.45 :select t :quit nil)
     ;; ("^\\*eww\\*$" :slot 2 :vslot 2 :side right :size 0.45 :select t :quit nil)
     ("^\\*Messages\\*$" :vslot -2 :size 0.5 :autosave t :quit t :ttl nil)
     ("^\\*Completions" :ignore t)
     ("^\\*Local variables\\*$" :vslot -1 :slot 1 :size +popup-shrink-to-fit)
     ("^\\*\\(?:[Cc]ompil\\(?:ation\\|e-Log\\)\\|Messages\\)" :vslot -2 :size 0.3 :autosave t :quit t :ttl nil)
     ("^\\*\\(?:doom \\|Pp E\\)"          ; transient buffers (no interaction required)
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
     ("^\\*declutter\*" :ignore t)))

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

;; (after! dirvish
;;   (setq!
;;    ;; dirvish-keep-alive-on-quit nil
;;    ;; dirvish-header-line-format nil
;;    dirvish-attributes '(file-size git-msg))
;;   (dirvish-define-preview exa (file)
;;     "Use `exa' to generate directory preview."
;;     (when (file-directory-p file)       ; we only interest in directories here
;;       `(shell . ("exa" "--color=always" "-al" ,file)))) ; use the output of `exa' command as preview
;;   (pushnew! dirvish-preview-dispatchers 'exa)

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
  :config
  (setq! detached-show-output-on-attach t))

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

(setq!
  langtool-http-server-host "api.languagetoolplus.com"
  langtool-http-server-port 443
  ;; langtool-language-tool-server-jar "/run/current-system/sw/bin/languagetool"
  langtool-java-bin (executable-find "java")
  ;; langtool-bin nil
  langtool-bin "/run/current-system/sw/bin/languagetool-commandline")

(after! languagetool-server
  (defadvice! +languagetool-server-parse-request (orig-fn)
    :around #'languagetool-server-parse-request
    "Return a json-like object with LanguageTool Server request arguments parsed.

Return the arguments as an assoc list of string which will be
used in the POST request made to the LanguageTool server."
    (let ((arguments (json-new-object)))

      ;; Appends the correction language information
      (setq arguments (json-add-to-object arguments "language" languagetool-correction-language))

      ;; Appends the mother tongue information
      (when (stringp languagetool-mother-tongue)
        (setq arguments (json-add-to-object arguments "motherTongue" languagetool-mother-tongue)))

      ;; Add LanguageTool Preamium features
      (when (stringp languagetool-api-key)
        (setq arguments (json-add-to-object arguments
                                            "tokenV2"
                                            (-> (auth-source-search :host "api.languagetoolplus.com"
                                                                    :user languagetool-username)
                                                car
                                                (plist-get :api_token)))))

      (when (stringp languagetool-username)
        (setq arguments (json-add-to-object arguments "username" languagetool-username)))

      (setq arguments (json-add-to-object arguments "level" "picky"))
      (setq arguments (json-add-to-object arguments "preferredVariants" "en-US,de-DE,pt-BR,ca-ES"))
      (setq arguments (json-add-to-object arguments "preferredLanguages" "en,zh"))

      ;; Appends the disabled rules
      (let ((rules ""))
        ;; Global disabled rules
        (dolist (rule languagetool-disabled-rules)
          (if (string= rules "")
              (setq rules (concat rules rule))
            (setq rules (concat rules "," rule))))
        ;; Local disabled rules
        (dolist (rule languagetool-local-disabled-rules)
          (if (string= rules "")
              (setq rules (concat rules rule))
            (setq rules (concat rules "," rule))))
        (unless (string= rules "")
          (setq arguments (json-add-to-object arguments "disabledRules" rules))))

      ;; Add the buffer contents
      (setq arguments (json-add-to-object arguments
                                          "text"
                                          (buffer-substring-no-properties
                                           (point-min)
                                           (point-max))))
      arguments)))

(use-package! languagetool
  :hook ((org-mode markdown-mode rst-mode asciidoc-mode latex-mode LaTeX-mode) . languagetool-server-mode)
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


(setq! consult-tramp-method "ssh"
       ;; tramp-ssh-controlmaster-options
       ;; "-o ControlMaster=auto -o ControlPath=tramp.%%C -o ControlPersist=600"
       )

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

(use-package! chatgpt-arcana
  :defer t
  :config
  (setq! chatgpt-arcana-system-prompts-alist
         '((programming . "You are a large language model living inside Emacs, and the perfect programmer. You may only respond with concise code unless explicitly asked.")
           (writing . "You are a large language model living inside Emacs, and an excellent writing assistant. Respond concisely and carry out instructions.")
           (chat . "You are a large language model living inside Emacs, and an excellent conversation partner. Respond concisely.")
           (prompt-generator . "You are a large language model living inside Emacs. I want you to act as a ChatGPT prompt generator, I will send a topic, you have to generate a ChatGPT prompt based on the content of the topic, the prompt should start with \" I want you to act as \", and guess what I might do, and expand the prompt accordingly Describe the content to make it useful and be concise.")
           (dev-rel-consultant . "I want you to act as a Developer Relations consultant. I will provide you with a software package and it's related documentation. Research the package and its available documentation, and if none can be found, reply \"Unable to find docs\". Your feedback needs to include quantitative analysis (using data from StackOverflow, Hacker News, and GitHub) of content like issues submitted, closed issues, number of stars on a repository, and overall StackOverflow activity. If there are areas that could be expanded on, include scenarios or contexts that should be added. Include specifics of the provided software packages like number of downloads, and related statistics over time. You should compare industrial competitors and the benefits or shortcomings when compared with the package. Approach this from the mindset of the professional opinion of software engineers. Review technical blogs and websites (such as TechCrunch.com or Crunchbase.com) and if data isn't available, reply \"No data available\". My first request is")
           (general-tool . "I want you to act as a go-to assistant for unit conversion, data format and file type conversions. You will be my guide when I need to know how to convert values from one unit to another or when I have questions about common data formats and file types used in coding. You will provide me with clear and concise explanations, examples and code snippets so that I can easily understand and implement these conversions in my projects. Whether I need to convert inches to centimeters, binary to hexadecimal or CSV to JSON, you will be my reliable source for all my conversion needs. And please be concise.")
           (fallback . "You are a large language model living inside Emacs. Help the user and be concise.")))

  (setq! chatgpt-arcana-system-prompts-modes-alist
         '((prog-mode . programming)
           (emacs-lisp-mode . programming)
           (org-mode . writing)
           (markdown-mode . writing)
           (chatgpt-arcana-chat-mode . chat)
           (fallback . fallback)))

  (setq! chatgpt-arcana-api-key
         (-> (auth-source-search :host "api.openai.com"
                                 :user user-mail-address)
             car
           (plist-get :api_secret))))

(use-package! gptel
  :defer t
  :config
  (setq gptel-directives '((default . "You are a large language model living in Emacs and a helpful coding assistant. Respond concisely.")
                           (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
                           (writing . "You are a large language model and a writing assistant. Respond concisely.")
                           (chat . "You are a large language model and a conversation partner. Respond concisely."))))

(use-package! jit-spell :hook (prog-mode text-mode))

(use-package shrface
  :defer t
  :config
  (shrface-basic)
  (shrface-trial)
  (shrface-default-keybindings) ; setup default keybindings
  (setq shrface-href-versatile t))

(use-package eww
  :defer t
  :config
  (require 'shrface))

(after! eww
  (add-hook! 'eww-after-render-hook #'shrface-mode)
  (add-hook! 'eww-after-render-hook (ignore-errors (eww-readable))))
