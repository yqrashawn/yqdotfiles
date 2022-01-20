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
  (setq! elcord-quiet t))
(use-package! idle-highlight-mode :hook (doom-first-file))
(add-hook! 'doom-first-file-hook #'global-display-fill-column-indicator-mode)

(after! prog-mode
  ;; unprettify when idle for 1 seconds
  (defadvice! +prettify-symbols--post-command-hook (orig-fn)
    :around #'prettify-symbols--post-command-hook
    (run-with-timer 1 nil
                    (cmd! (let ((ti (or (current-idle-time) '(0 0 0))))
                            (when (or (> (nth 1 ti) 0)
                                      (> (nth 2 ti) 900000))
                              (funcall orig-fn)))))))

(add-hook! 'doom-first-input-hook
           (cmd! (if (boundp 'pixel-scroll-precision-mode)
                     (pixel-scroll-precision-mode t))))


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
