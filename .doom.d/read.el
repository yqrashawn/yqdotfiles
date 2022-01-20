;;; read.el -*- lexical-binding: t; -*-
(setq!
  elfeed-use-curl nil
  ;; necessary for https without a trust certificate
  elfeed-protocol-newsblur-maxpages 100
  elfeed-curl-extra-arguments `("--insecure"
                                "--cookie-jar"
                                ,(concat doom-cache-dir "newsblur-cookie")
                                "--cookie"
                                ,(concat doom-cache-dir "newsblur-cookie")))

(setq! elfeed-feeds '(("newsblur+https://yqrashawn@newsblur.com"
                       :use-authinfo t)))

;; with org
;; (defadvice! ++elfeed (&rest _)
;;   :after #'elfeed
;;   "Make elfeed-org autotags rules works with elfeed-protocol."
;;   (setq! elfeed-protocol-tags (or elfeed-feeds '()))
;;   (setq! elfeed-feeds (list
;;                        (list "newsblur+https://yqrashawn@newsblur.com"
;;                              :use-authinfo t
;;                              :autotags elfeed-protocol-tags))))

(after! elfeed
  (setq!
    rmh-elfeed-org-files `(,(concat org-directory "elfeed.org"))
    elfeed-search-filter "+unread")
  (add-hook! 'elfeed-search-mode-hook 'elfeed-update)
  (elfeed-set-timeout 36000)
  (elfeed-protocol-enable)
  (add-hook! 'elfeed-new-entry-hook
    (list
      (elfeed-make-tagger
        :feed-url "github\\.com"
        :add '(github))
      (elfeed-make-tagger
        :entry-title "^Sentry"
        :add '(sentry))
      (elfeed-make-tagger
        :feed-url "planet\\.clojure\\.in"
        :add '(clojure))
      (elfeed-make-tagger
        :feed-url "twitter\\.com"
        :add '(github))
      (elfeed-make-tagger
        :feed-url "forum\\.conflux\\.fun"
        :add '(conflux)))))
