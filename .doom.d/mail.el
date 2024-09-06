;;; mail.el -*- lexical-binding: t; -*-

;; (pushnew! load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
;; (setq mu4e-view-use-old t)
;; (setq mu4e-html2text-command 'mu4e-shr2text)
;; (setq mu4e-html2text-command "w3m -dump -cols 80 -T text/html")
;; (setq mu4e-html2text-command (expand-file-name "~/local/bin/rdr_w3m"))
;; (setq mu4e-html2text-command "html2text -from_encoding utf8 -ascii -width 72")
;; (setq mu4e-html2text-command "html2text -width 72")

(setq +notmuch-home-function #'++notmuch-start)
(after! notmuch
  (defun +notmuch-get-sync-command ()
    "custom notmuch sync command"
    ;; "cd ~/mail/namy.19@gmail.com && gmi sync && notmuch new"
    ;; "bash ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/notmuch-hooks/pre-sync && mbsync -a && notmuch new"
    (expand-file-name "~/local/bin/syncmail"))
  (setq!
   mm-text-html-renderer 'w3m-standalone
   sendmail-program (executable-find "msmtp")
   send-mail-function 'smtpmail-send-it
   message-sendmail-f-is-evil t
   message-sendmail-extra-arguments '("--read-envelope-from")
   +notmuch-delete-tags '("+deleted" "-unread" "-inbox")
   notmuch-archive-tags '("-inbox" "-unread" "+archived")
   notmuch-show-indent-messages-width 2)
  (set-popup-rule! "^\\*notmuch-hello" :ignore t)
  (set-popup-rule! "^\\*subject:" :ignore t))
