;;; read.el -*- lexical-binding: t; -*-
(setq!
  elfeed-use-curl t
  ;; necessary for https without a trust certificate
  elfeed-protocol-newsblur-maxpages 20
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
  (run-with-idle-timer 300 t #'elfeed-update)
  (setq!
    ;; rmh-elfeed-org-files `(,(concat org-directory "elfeed.org"))
    elfeed-search-filter "+unread @3-months-ago -ghstar -twitter -metamask"
    elfeed-search-trailing-width 60)
  (add-hook 'elfeed-search-mode-hook 'elfeed-update)
  (elfeed-set-timeout 36000)
  (setq elfeed-protocol-enabled-protocols '(newsblur))
  (elfeed-protocol-enable)
  ;; (setq elfeed-new-entry-hook (list))
  (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "reddit\\.com"  :add '(reddit)))
  (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "reddit\\.com\\/r\\/emacs"  :add '(emacs)))
  (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "reddit\\.com\\/r/Clojure"  :add '(clojure)))
  (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :before "2 weeks ago" :add '2wo))
  (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "github\\.com" :add '(github)))
  (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-url "conflux\\.fun" :add '(crypto conflux)))
  (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-url "twitter\\.com" :add '(twitter)))
  (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-url "planet\\.clojure" :add '(clojure)))
  (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-title "metamask" :add '(metamask crypto)))
  (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "china" :add '(china)))
  (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "protocol\\.com" :add '(protocol news)))
  (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-url "\\/\\/community\\..*\\.\w" :add '(community)))
  (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "ethereum"  :add '(ethereum crypto)))
  (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-title "ethereum"  :add '(ethereum crypto)))
  (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-title "solana"  :add '(solana crypto)))
  (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-title "polygon"  :add '(polygon crypto)))
  (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-title "curve"  :add '(curve crypto)))
  (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-title "uniswap"  :add '(uniswap crypto)))
  (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-title "crypto"  :add '(crypto)))
  (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-title "clojure"  :add '(clojure)))
  (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-title "emacs"  :add '(emacs)))
  (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-title "emacs"  :add '(emacs)))
  (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "uxdesign\\.cc"  :add '(ux)))
  (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-title "Hacker News" :add '(hn)))
  (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-title "New releases from starred repo" :add '(ghstar)))
  (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-title "bankless" :add '(crypto)))
  (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-url "yqrashawn@newsblur\\.com::http:\\/\\/github\\.com" :add '(ghn)))
  (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "yqrashawn@newsblur\\.com::http:\\/\\/github\\.com" :add '(ghn))))

(defun +elfeed-debug ()
  (interactive)
  (setq elfeed-log-level 'debug)
  (toggle-debug-on-error)
  (setq elfeed-protocol-log-trace t)
  (setq elfeed-protocol-fever-maxsize 5)
  (setq elfeed-protocol-owncloud-maxsize 5)
  (setq elfeed-protocol-ttrss-maxsize 5)
  (setq elfeed-protocol-newsblur-maxpages 20))

(defadvice! +elfeed-search-browse-url (orig-fn &optional use-general-p)
  "M-RET to view entry in eww, 1 M-RET to view entry in default browser"
  :around #'elfeed-search-browse-url
  (let ((browse-url-browser-function 'eww-browse-url)
         (browse-url-generic-program "open"))
    (call-interactively orig-fn)))

(use-package! elfeed-dashboard
  :commands (elfeed-dashboard)
  :config
  (setq elfeed-dashboard-file "~/.doom.d/elfeed-dashboard.org")
  ;; update feed counts on elfeed-quit
  (advice-add 'elfeed-search-quit-window :after #'elfeed-dashboard-update-links))
