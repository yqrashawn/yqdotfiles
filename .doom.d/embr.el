;;; embr.el --- embr browser configuration -*- lexical-binding: t; -*-

(use-package! embr
  :defer t
  :config
  (setq embr-hover-rate 30
        embr-default-width 1280
        embr-default-height 720
        embr-screen-width 1920
        embr-screen-height 1080
        embr-color-scheme 'dark
        embr-search-engine 'google
        embr-scroll-method 'instant
        embr-scroll-step 100
        embr-frame-source 'screencast
        embr-render-backend 'default
        embr-display-method 'headless)

  ;; Auto-size viewport to match the current Emacs window pixel dimensions.
  ;; embr only sets viewport at init time (no resize command), so we set
  ;; embr-default-width/height before the daemon starts.
  (defadvice! +embr--auto-size-viewport-a (&rest _)
    :before #'embr-browse
    (let ((w (window-body-width nil t))
          (h (window-body-height nil t)))
      (when (and w h (> w 0) (> h 0))
        (setq embr-default-width w
              embr-default-height h
              embr--viewport-width nil
              embr--viewport-height nil)))))
