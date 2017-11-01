(defconst yq-mode-line-packages
  '(
    (yq-mode-line :location built-in)))

;; dev use
;; (defconst list-faces-sample-text
;;   "➊➋➌abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ"
;;   "Text string to display as the sample text for `list-faces-display'.")

(defvar yq-mode-line-display-default-perspective t
  "If non-nil, the default perspective name is displayed in the mode-line.")
(defvar spaceline-workspace-numbers-unicode t
  "Set to true to enable unicode display in the `workspace-number' segment.")

(defface spaceline-highlight-face
  `((t (:background "DarkGoldenrod2"
                    :foreground "#3E3D31"
                    :inherit 'mode-line)))
  "Default highlight face for spaceline."
  :group 'spaceline)

;; Define various other highlight faces
(dolist (s '((spaceline-evil-normal "DarkGoldenrod2" "Evil normal state face.")
             (spaceline-evil-insert "chartreuse3" "Evil insert state face.")
             (spaceline-evil-emacs "SkyBlue2" "Evil emacs state face.")
             (spaceline-evil-replace "chocolate" "Evil replace state face.")
             (spaceline-evil-visual "gray" "Evil visual state face.")
             (spaceline-evil-motion "plum3" "Evil motion state face.")
             (spaceline-unmodified "DarkGoldenrod2" "Unmodified buffer face.")
             (spaceline-modified "SkyBlue2" "Modified buffer face.")
             (spaceline-read-only "plum3" "Read-only buffer face.")))
  (eval `(defface ,(nth 0 s)
           `((t (:background ,(nth 1 s)
                             :foreground "#3E3D31"
                             :inherit 'mode-line)))
           ,(nth 2 s)
           :group 'spaceline)))

(defun yq-mode-line/init-yq-mode-line ()

  ;; (defun yq/display-mode-indent-width ()
  ;;   (let ((mode-indent-level
  ;;          (catch 'break
  ;;            (dolist (test spacemacs--indent-variable-alist)
  ;;              (let ((mode (car test))
  ;;                    (val (cdr test)))
  ;;                (when (or (and (symbolp mode) (derived-mode-p mode))
  ;;                          (and (listp mode) (apply 'derived-mode-p mode))
  ;;                          (eq 't mode))
  ;;                  (when (not (listp val))
  ;;                    (setq val (list val)))
  ;;                  (dolist (v val)
  ;;                    (cond
  ;;                     ((integerp v) (throw 'break v))
  ;;                     ((and (symbolp v) (boundp v))
  ;;                      (throw 'break (symbol-value v))))))))
  ;;            (throw 'break (default-value 'evil-shift-width)))))
  ;;     (concat "TS:" (int-to-string (or mode-indent-level 0)))))

  (setq my-flycheck-mode-line
        '(:eval
          (pcase flycheck-last-status-change
            ((\` not-checked) nil)
            ((\` no-checker) (propertize " -" 'face 'warning))
            ((\` running) (propertize " ✷" 'face 'success))
            ((\` errored) (propertize " !" 'face 'error))
            ((\` finished)
             (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
                    (no-errors (cdr (assq 'error error-counts)))
                    (no-warnings (cdr (assq 'warning error-counts)))
                    (face (cond (no-errors 'error)
                                (no-warnings 'warning)
                                (t 'success))))
               (propertize (format "[%s/%s]" (or no-errors 0) (or no-warnings 0))
                           'face face)))
            ((\` interrupted) " -")
            ((\` suspicious) '(propertize " ?" 'face 'warning)))))

  (setq-default mode-line-misc-info
                (assq-delete-all 'which-func-mode mode-line-misc-info))

  ;; (setq mode-line-format
  (setq-default mode-line-format
                (list
                 " %1"
                 '(:eval (propertize
                          (concat "<" (window-number-mode-line) ">")
                          'face 'font-lock-string-face))


                 ;; insert vs overwrite mode, input-method in a tooltip
                 ;; evil state
                 '(:eval evil-mode-line-tag)
                 ;; '(:eval (propertize (if overwrite-mode "R" "I")
                 ;;                     'face 'font-lock-preprocessor-face
                 ;;                     'help-echo (concat "Buffer is in "
                 ;;                                        (if overwrite-mode
                 ;;                                            "overwrite"
                 ;;                                          "insert") " mode")))

                 ;; was this buffer modified since the last save?
                 '(:eval (when (buffer-modified-p)
                           (propertize "+"
                                       'face 'warning
                                       'help-echo "Buffer has been modified")))

                 ;; is this buffer read-only?
                 '(:eval (when buffer-read-only
                           (propertize "RO"
                                       'face 'font-lock-type-face
                                       'help-echo "Buffer is read-only")))

                 "%1 "
                 ;; the buffer name; the file name as a tool tip
                 '(:eval (propertize "%b " 'face 'font-lock-keyword-face
                                     'help-echo (buffer-file-name)))

                 ;; anzu
                 anzu--mode-line-format

                 ;; relative position, size of file
                 "["
                 (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
                 "/"
                 (propertize "%I" 'face 'font-lock-constant-face) ;; size
                 "] "

                 ;; the current major mode for the buffer.
                 '(:eval (propertize "%m" 'face 'font-lock-string-face
                                     'help-echo buffer-file-coding-system))

                 "%1 "
                 my-flycheck-mode-line
                 "%1 "

                 ;; minor modes
                 ;; '(:eval (when (> (window-width) 90)
                 ;;           minor-mode-alist))
                 ;; " "

                 ;; git info
                 '(:eval (when (> (window-width) 120)
                           `(vc-mode vc-mode)))

                 " "
                 ;; global-mode-string goes in mode-line-misc-info
                 '(:eval (when (> (window-width) 120)
                           mode-line-misc-info))

                 ;; workspace eyebrowse
                 '(:eval (if (setq-local yq-eyebrowse-config-name
                                       (mapconcat 'identity
                                                  (let* ((num (eyebrowse--get 'current-slot))
                                                         (tag (when num (nth 2 (assoc num (eyebrowse--get 'window-configs)))))
                                                         (str (if (and tag (< 0 (length tag)))
                                                                  tag
                                                                (when num (int-to-string num)))))
                                                    (or (when spaceline-workspace-numbers-unicode
                                                          (spaceline--unicode-number str)))
                                                    (list str)) ""))
                             (propertize (concat "[" yq-eyebrowse-config-name "|") 'face 'font-lock-type-face)))

                 ;; '(:eval (let* ((num (eyebrowse--get 'current-slot))
                 ;;                (tag (when num (nth 2 (assoc num (eyebrowse--get 'window-configs)))))
                 ;;                (str (when spaceline-workspace-numbers-unicode
                 ;;                       (concat "[" (spaceline--unicode-number
                 ;;                                    (if (and tag (< 0 (length tag))) tag
                 ;;                                      (when num (int-to-string num)))) "|"))))
                 ;;           (propertize str 'face 'custom-rogue)))


                 ;; layout function may update later
                 ;; '(:eval (yq/update-persp-name)) ;; layout name
                 ;;layout
                 '(:eval (when (and (bound-and-true-p persp-mode)
                                    ;; There are multiple implementations of
                                    ;; persp-mode with different APIs
                                    (fboundp 'safe-persp-name)
                                    (fboundp 'get-frame-persp)
                                    ;; Display the nil persp only if specified
                                    (or (not (string= persp-nil-name (safe-persp-name (get-frame-persp))))
                                        dotspacemacs-display-default-layout))

                           (let ((name (safe-persp-name (get-frame-persp))))
                             (propertize
                              (if (file-directory-p name)
                                  (file-name-nondirectory (directory-file-name name))
                                (concat name ""))
                              'face 'font-lock-type-face))
                           ;; (let ((name (safe-persp-name (get-frame-persp))))
                           ;;   (propertize
                           ;;    (if (file-directory-p name)
                           ;;        (concat (file-name-nondirectory (directory-file-name name)) "|")
                           ;;      (concat name "|"))
                           ;;    'face 'font-lock-type-face))
                           ))

                 ;;window-purpose
                 ;; '(:eval (when (bound-and-true-p purpose-mode)
                 ;;           (propertize (concat (substring (purpose--modeline-string) 2 -1) "]")
                 ;;                       'face 'font-lock-type-face
                 ;;                       'help-echo "Window purpose")))
                 "]"

                 ;; '(:eval (yq/display-mode-indent-width))

                 (mode-line-fill 'mode-line 16)
                 ;; line and column
                 " (" ;; '%02' to set to 2 chars at least; prevents flickering
                 (propertize "%02l" 'face 'font-lock-type-face) ","
                 (propertize "%02c" 'face 'font-lock-type-face)
                 ") "

                 ;; buffre encoding
                 ;; '(:eval (when (> (window-width) 80)
                 ;; (buffer-encoding-abbrev)))

                 mode-line-end-spaces
                 ;; add the time, with the date and the emacs uptime in the tooltip

                 ;; (propertize (format-time-string "%a, %b %d %Y, %H:%M") 'face 'font-lock-constant-face)
                 '(:eval (propertize (format-time-string "%H:%M")
                                     'help-echo
                                     (concat (format-time-string "%c; ")
                                             (emacs-uptime "Uptime:%hh")))))))



(setq projectile-mode-line
      '(:eval (format " Projectile[%s(%s)]"
                      (projectile-project-name))))

