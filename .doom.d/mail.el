;;; mail.el -*- lexical-binding: t; -*-

;; (pushnew! load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
;; (setq mu4e-view-use-old t)
;; (setq mu4e-html2text-command 'mu4e-shr2text)
;; (setq mu4e-html2text-command "w3m -dump -cols 80 -T text/html")
;; (setq mu4e-html2text-command (expand-file-name "~/local/bin/rdr_w3m"))
;; (setq mu4e-html2text-command "html2text -from_encoding utf8 -ascii -width 72")
;; (setq mu4e-html2text-command "html2text -width 72")

(defun +notmuch-get-sync-command ()
  "custom notmuch sync command"
  "cd ~/.mail/account.gmail && gmi sync && cd ~/.mail/account.yqrashawn && gmi sync && notmuch new")

(after! notmuch
  (setq! mm-text-html-renderer 'w3m-standalone)
  (setq! +notmuch-delete-tags '("+deleted" "-inbox" "-unread")
         notmuch-show-indent-messages-width 2))