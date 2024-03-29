;;; read.el -*- lexical-binding: t; -*-
(setq!
 shr-use-xwidgets-for-media t
 shr-discard-aria-hidden t
 shr-width 80
 elfeed-use-curl t
 elfeed-curl-extra-arguments `("--insecure"
                               "--cookie-jar"
                               ,(concat doom-cache-dir "newsblur-cookie")
                               "--cookie"
                               ,(concat doom-cache-dir "newsblur-cookie")))

(add-hook! 'doom-after-init-hook
  (lambda () (run-with-idle-timer 10 nil (lambda () (require 'elfeed)))))

(after! elfeed
  (require 'eww)
  (elfeed-set-timeout 36000)
  (elfeed-update)
  (run-with-idle-timer 300 t #'elfeed-update)
  (setq!
   ;; necessary for https without a trust certificate
   elfeed-protocol-newsblur-maxpages 80
   ;; rmh-elfeed-org-files `(,(concat org-directory "elfeed.org"))
   ;; elfeed-search-filter "+unread -releases -crypto -design"
   elfeed-search-filter "+unread +P1"
   elfeed-search-trailing-width 10)
  (add-hook! 'elfeed-search-mode-hook 'elfeed-update)
  (setq! elfeed-protocol-enabled-protocols '(newsblur)
         elfeed-protocol-feeds `(("newsblur+https://yqrashawn@newsblur.com"
                                  :use-authinfo t)))
  (elfeed-protocol-enable)

  ;; (load-file (expand-file-name "~/Dropbox/sync/elfeed.el"))
  ;; (setq! elfeed-show-mode-hook
  ;;        '(doom--setq-shr-external-rendering-functions-for-elfeed-show-mode-h
  ;;          doom--setq-shr-put-image-function-for-elfeed-show-mode-h
  ;;          elfeed-goodies/show-mode-setup))
  (remove-hook! 'elfeed-show-mode-hook #'+rss-elfeed-wrap-h)
  ;; (add-hook! 'elfeed-show-mode-hook (lambda () (text-scale-set 2)))
  (add-hook! 'elfeed-show-mode-hook #'mixed-pitch-mode)
  (add-hook! 'elfeed-show-mode-hook #'writeroom-mode)
  (add-hook! 'elfeed-show-mode-hook #'hl-line-mode)
  (add-hook! 'elfeed-update-hooks (lambda (&optional _url) (message "elfeed: update finished")))

  (defun +elfeed-full-update ()
    (interactive)
    (let ((elfeed-search-trailing-width 100))
      (elfeed-update)))

  (defadvice! +elfeed-db-save (orig-fn)
    :around #'elfeed-db-save
    (let ((gc-cons-threshold most-positive-fixnum))
      (funcall orig-fn))))

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

(after! evil
  (pushnew! evil-emacs-state-modes 'elfeed-dashboard-mode))

(use-package! elfeed-dashboard
  :commands (elfeed-dashboard)
  :config
  (setq elfeed-dashboard-file "~/.doom.d/elfeed-dashboard.org")
  ;; update feed counts on elfeed-quit
  (advice-add 'elfeed-search-quit-window :after #'elfeed-dashboard-update-links))

(cl-defun elfeed-make-tagger
    (&key feed-title feed-url entry-title entry-link after before
          add remove callback)
  "Create a function that adds or removes tags on matching entries.

FEED-TITLE, FEED-URL, ENTRY-TITLE, and ENTRY-LINK are regular
expressions or a list (not <regex>), which indicates a negative
match. AFTER and BEFORE are relative times (see
`elfeed-time-duration'). Entries must match all provided
expressions. If an entry matches, add tags ADD and remove tags
REMOVE.

Examples,

  (elfeed-make-tagger :feed-url \"youtube\\\\.com\"
                      :add '(video youtube))

  (elfeed-make-tagger :before \"1 week ago\"
                      :remove 'unread)

  (elfeed-make-tagger :feed-url \"example\\\\.com\"
                      :entry-title '(not \"something interesting\")
                      :add 'junk)

The returned function should be added to `elfeed-new-entry-hook'."
  (let ((after-time  (and after  (elfeed-time-duration after)))
        (before-time (and before (elfeed-time-duration before))))
    (when (and add (symbolp add)) (setf add (list add)))
    (when (and remove (symbolp remove)) (setf remove (list remove)))
    (lambda (entry)
      (let ((feed (elfeed-entry-feed entry))
            (date (elfeed-entry-date entry))
            (case-fold-search t))
        (cl-flet ((match (r s)
                    (or (null r)
                        (if (listp r)
                            (not (string-match-p (cl-second r) s))
                          (string-match-p r s)))))
          (when (and
                 (match feed-title  (elfeed-feed-title  feed))
                 (match feed-url    (elfeed-feed-url    feed))
                 (match entry-title (elfeed-entry-title entry))
                 (match entry-link  (elfeed-entry-link  entry))
                 (or (not after-time)  (> date (- (float-time) after-time)))
                 (or (not before-time) (< date (- (float-time) before-time))))
            (when add
              (apply #'elfeed-tag entry add))
            (when remove
              (apply #'elfeed-untag entry remove))
            (when callback
              (funcall callback entry))
            entry))))))

(when (modulep! :app rss)
  ;; (setq-hook! 'eww-mode-hook
  ;;   shr-put-image-function #'+rss-put-sliced-image-fn ; produce empty lines
  ;;   shr-external-rendering-functions '((img . +rss-render-image-tag-without-underline-fn)))
  )
