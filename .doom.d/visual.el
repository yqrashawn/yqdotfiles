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

(use-package! mini-frame
  :hook (doom-first-input . mini-frame-mode)
  :init
  (setq! mini-frame-show-parameters '((top . 10)
                                      (width . 0.7)
                                      (left . 0.5)
                                      (height . 15)))

  (after! ivy (setq! ivy-read-action-function #'ivy-read-action-by-key))
  (defadvice! +ivy-shrink-after-dispatching-a (f &rest a)
    :around #'ivy-shrink-after-dispatching
    (unless mini-frame-mode (apply f a))))

(use-package! elcord :hook (doom-first-file-hook . elcord-mode))