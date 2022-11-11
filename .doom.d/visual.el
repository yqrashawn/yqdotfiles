;;; visual.el -*- lexical-binding: t; -*-

(setq! text-scale-remap-header-line t
       mac-allow-anti-aliasing t)

(use-package! outline-minor-faces
  :hook (outline-minor-mode . outline-minor-faces-add-font-lock-keywords))

(use-package! backline
  :after outline
  :config
  (advice-add 'outline-flag-region :after 'backline-update))


(after! prog-mode
  (delq! 'highlight-indent-guides-mode prog-mode-hook))

;; (use-package! mini-frame
;;   :hook (doom-first-input . mini-frame-mode)
;;   :init
;;   (setq! mini-frame-show-parameters '((top . 10)
;;                                       (width . 0.7)
;;                                       (left . 0.5)
;;                                       (height . 15)))

;;   (after! ivy (setq! ivy-read-action-function #'ivy-read-action-by-key))
;;   (defadvice! +ivy-shrink-after-dispatching-a (f &rest a)
;;     :around #'ivy-shrink-after-dispatching
;;     (unless mini-frame-mode (apply f a))))

(use-package! elcord
  :hook (doom-first-file . elcord-mode)
  :init
  (defun +elcord-local-time ()
    (let ((hour (nth 2 (decode-time (current-time)))))
      (if (> hour 12)
          (format "%sPM" (- hour 12))
        (format "%sAM" hour))))
  (setq! elcord-quiet t
         elcord-use-major-mode-as-main-icon t
         elcord-display-buffer-details nil)
  :config
  (defadvice! +elcord--start-idle (orig-fn)
    :around #'elcord--start-idle
    "Set presence to idle, pause update and timer."
    (unless elcord--idle-status
      (unless elcord-quiet
        (message (format "elcord: %s" elcord-idle-message)))

      ;;hacky way to stop updates and store elapsed time
      (cancel-timer elcord--update-presence-timer)
      (setq elcord--startup-time (string-to-number (format-time-string "%s" (time-subtract nil elcord--startup-time)))

            elcord--idle-status t)

      (let* ((local-time (+elcord-local-time))
             (activity
              `(("assets" . (,@(elcord--mode-icon-and-text)))
                ("timestamps" ("start" ,@(string-to-number (format-time-string "%s" (current-time)))))
                ("details" . ,local-time) ("state" . ,elcord-idle-message)))
             (nonce (format-time-string "%s%N"))
             (presence
              `(("cmd" . "SET_ACTIVITY")
                ("args" . (("activity" . ,activity)
                           ("pid" . ,(emacs-pid))))
                ("nonce" . ,nonce))))
        (elcord--send-packet 1 presence))
      (add-hook 'pre-command-hook 'elcord--cancel-idle)))
  (defadvice! +elcord--details-and-state (orig-fn)
    :around #'elcord--details-and-state
    (let ((activity (if elcord-display-buffer-details
                        (list
                         (cons "details" (funcall elcord-buffer-details-format-function))
                         (cons "state" (format "Line %s of %S"
                                               (format-mode-line "%l")
                                               (+ 1 (count-lines (point-min) (point-max))))))
                      (list
                        (cons "details" (+elcord-local-time))
                       (cons "state" (elcord--mode-text))))))
      (when elcord-display-elapsed
        (push (list "timestamps" (cons "start" elcord--startup-time)) activity))
      activity))
  (pushnew! elcord-boring-buffers-regexp-list "scratch\\*"))
(use-package! idle-highlight-mode :hook (doom-first-file))
(add-hook! 'doom-first-file-hook #'global-display-fill-column-indicator-mode)

(after! prog-mode
  (when (modulep! :ui ligatures)
    ;; unprettify when idle for 1 seconds
    (defadvice! +prettify-symbols--post-command-hook (orig-fn)
      :around #'prettify-symbols--post-command-hook
      (run-with-timer 1 nil
                      (cmd! (let ((ti (or (current-idle-time) '(0 0 0))))
                              (when (or (> (nth 1 ti) 0)
                                        (> (nth 2 ti) 900000))
                                (funcall orig-fn))))))))

(defun maybe-enable-pixel-scroll-precision-mode ()
  (if (boundp 'pixel-scroll-precision-mode)
      (pixel-scroll-precision-mode t)))

(add-hook! 'doom-first-input-hook 'maybe-enable-pixel-scroll-precision-mode)


(after! js2-mode
  (font-lock-add-keywords
   'js2-mode
   (append `(("\\(\"\\|'\\)0x" (0 (+truncate-0x-hash)))))))
(after! rjsx-mode
  (font-lock-add-keywords
   'rjsx-mode
   (append `(("\\(\"\\|'\\)0x" (0 (+truncate-0x-hash)))))))
(after! typescript-mode
  (font-lock-add-keywords
   'typescript-mode
   (append `(("\\(\"\\|'\\)0x" (0 (+truncate-0x-hash)))))))
(after! clojure-mode
  (font-lock-add-keywords
   'clojurescript-mode
   (append `(("\"0x" (0 (+truncate-0x-hash))))))
  (font-lock-add-keywords
   'clojure-mode
   (append `(("\"0x" (0 (+truncate-0x-hash)))))))
(after! web-mode
  (font-lock-add-keywords
   'web-mode
   (append `(("\\(\"\\|'\\)0x" (0 (+truncate-0x-hash)))))))


(use-package! modus-themes
  :defer t
  :init
  (setq!
   modus-themes-italic-constructs t
   modus-themes-completions '((matches . (extrabold background intense))
                              (selection . (semibold accented intense))
                              (popup . (accented)))
   modus-themes-mixed-fonts t
   modus-themes-fringes 'subtle
   modus-themes-lang-checkers '(straight-underline text-also)
   modus-themes-mode-line '(borderless)
   modus-themes-links '(neutral-underline faint)
   modus-themes-region '(no-extend)))

(defun my-modus-themes-custom-faces ()
  (modus-themes-with-colors
    (custom-set-faces
     ;; Replace green with blue if you use `modus-themes-deuteranopia'.
     `(git-gutter-fr:added ((,class :foreground ,green-fringe-bg)))
     `(git-gutter-fr:deleted ((,class :foreground ,red-fringe-bg)))
     `(git-gutter-fr:modified ((,class :foreground ,yellow-fringe-bg))))))

(add-hook 'modus-themes-after-load-theme-hook #'my-modus-themes-custom-faces)

;; (use-package! zoom :hook (doom-first-buffer))

;; (use-package! fancy-compilation
;;   :after compile
;;   :config
;;   (fancy-compilation-mode))

(use-package! topsy :hook (prog-mode . topsy-mode))

(after! doom-modeline
  (setq! doom-modeline-persp-name t
         doom-modeline-persp-icon nil))

(remove-hook! '(prog-mode-hook text-mode-hook conf-mode-hook) '(highlight-indent-guides-mode))
