(defconst yq-mode-line-packages
  '(
    (zilong-mode-line :location built-in)
    )
  )

(defun yq-mode-line/init-zilong-mode-line ()

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

  (setq-default mode-line-format
                (list
                 " %1"
                 '(:eval (propertize
                          (window-number-mode-line)
                          'face
                          'font-lock-type-face))
                 ;; '(:eval (yq/update-persp-name)) ;; layout name


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
                                       'face 'font-lock-warning-face
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

                 ;; " "
                 ;; global-mode-string goes in mode-line-misc-info
                 '(:eval (when (> (window-width) 120)
                           mode-line-misc-info))

                 (mode-line-fill 'mode-line 20)

                 ;; '(:eval (yq/display-mode-indent-width))

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
                 '(:eval (propertize (format-time-string "%H:%M")
                                     'help-echo
                                     (concat (format-time-string "%c; ")
                                             (emacs-uptime "Uptime:%hh"))))
                 )))

(defun yq-mode-line/post-init-spaceline ()
  (use-package spaceline-config
    :config
    (progn
      (defvar spaceline-org-clock-format-function
        'org-clock-get-clock-string
        "The function called by the `org-clock' segment to determine what to show.")

      (spaceline-define-segment org-clock
                                "Show information about the current org clock task.  Configure
`spaceline-org-clock-format-function' to configure. Requires a currently running
org clock.

This segment overrides the modeline functionality of `org-mode-line-string'."
                                (when (and (fboundp 'org-clocking-p)
                                           (org-clocking-p))
                                  (substring-no-properties (funcall spaceline-org-clock-format-function)))
                                :global-override org-mode-line-string)

      (spaceline-compile
       'zilong
       ;; Left side of the mode line (all the important stuff)
       '(((persp-name
           workspace-number
           window-number
           )
          :separator "|"
          :face highlight-face)
         ((buffer-modified buffer-size input-method))
         anzu
         '(buffer-id remote-host buffer-encoding-abbrev)
         ((point-position line-column buffer-position selection-info)
          :separator " | ")
         major-mode
         process
         (flycheck-error flycheck-warning flycheck-info)
         ;; (python-pyvenv :fallback python-pyenv)
         ;; ((minor-modes :separator spaceline-minor-modes-separator) :when active)
         (org-pomodoro :when active)
         (org-clock :when active)
         nyan-cat)
       ;; Right segment (the unimportant stuff)
       '((version-control :when active)
         battery))

      (setq-default mode-line-format '("%e" (:eval (spaceline-ml-zilong))))
      )))
