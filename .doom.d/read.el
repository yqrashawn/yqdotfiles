;;; read.el -*- lexical-binding: t; -*-

(setq!
  elfeed-use-curl t
  ;; necessary for https without a trust certificate
  elfeed-protocol-newsblur-maxpages 100
  elfeed-curl-extra-arguments `("--insecure"
                                "--cookie-jar"
                                ,(concat doom-cache-dir "newsblur-cookie")
                                "--cookie"
                                ,(concat doom-cache-dir "newsblur-cookie")))

(when (file-exists-p (expand-file-name "~/Dropbox/sync/newsblur.el"))
  (load! (expand-file-name "~/Dropbox/sync/newsblur.el")))

(use-package! elfeed-autotag
  :after elfeed
  :config
  (setq! elfeed-autotag-files `(,(concat org-directory "elfeed.org")))
  (elfeed-autotag))

(after! elfeed
  (setq!
    rmh-elfeed-org-files `(,(concat org-directory "elfeed.org"))
    elfeed-search-filter "+unread")
  (add-hook! 'elfeed-search-mode-hook 'elfeed-update)
  (elfeed-set-timeout 36000)
  (elfeed-protocol-enable))
