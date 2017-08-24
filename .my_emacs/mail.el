(setq mu4e-maildir "~/Maildir"
      mu4e-trash-folder "/Trash"
      mu4e-refile-folder "/Archive"
      mu4e-attachment-dir "/Attachments"
      mu4e-get-mail-command "mbsync -a"
      mu4e-update-interval 300 ;; 30 min
      mu4e-compose-signature-auto-include t
      mu4e-view-show-images t
      mu4e-enable-mode-line t
      mu4e-confirm-quit nil
      mu4e-enable-notifications t
      mu4e-change-filenames-when-moving t
      mu4e-use-fancy-chars t
      mu4e-view-show-addresses t)

(setq mu4e-compose-auto-include-date t
      mu4e-compose-keep-self-cc t)
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(with-eval-after-load 'mu4e
  (setq org-mu4e-convert-to-html t)
  (setq message-send-mail-function 'message-send-mail-with-sendmail))

;; Call EWW to display HTML messages
(defun jcs-view-in-eww (msg)
  (eww-browse-url (concat "file://" (mu4e~write-body-to-html msg))))


;; From Ben Maughan: Get some Org functionality in compose buffer
(add-hook 'message-mode-hook 'turn-on-orgtbl)
(add-hook 'message-mode-hook 'turn-on-orgstruct++)

;; every new email composition gets its own frame! (window)
(setq mu4e-compose-in-new-frame nil)

;;store link to message if in header view, not to header query
(setq org-mu4e-link-query-in-headers-mode nil)

;; give me ISO(ish) format date-time stamps in the header list
(setq mu4e-headers-date-format "%Y-%m-%d %H:%M")

;; Set format=flowed
;; mu4e sets up visual-line-mode and also fill (M-q) to do the right thing
;; each paragraph is a single long line; at sending, emacs will add the
;; special line continuation characters.
(setq mu4e-compose-format-flowed t)
;; Try to show images
(setq mu4e-view-show-images t
      mu4e-show-images t
      mu4e-view-image-max-width 800)
(setq mu4e-headers-fields
      '((:date          .  25)    ;; alternatively, use :human-date
        (:flags         .   6)
        (:from          .  22)
        (:subject       .  nil))) ;; alternatively, use :thread-subject

;; not using smtp-async yet
;; some of these variables will get overridden by the contexts
(setq
 send-mail-function 'smtpmail-send-it
 message-send-mail-function 'smtpmail-send-it)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;;; Mail directory shortcuts
(setq mu4e-maildir-shortcuts
      '(("/gmail/INBOX" . ?g)
        ("/bimsop/INBOX" . ?b)
        ("/icloud/INBOX" . ?a)
        ("/qq/INBOX" . ?q)
        ("/zoho/INBOX" . ?z)))

;;; Bookmarks
(setq mu4e-bookmarks
      `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
        ("date:today..now" "Today's messages" ?t)
        ("date:7d..now" "Last 7 days" ?w)
        ("mime:image/*" "Messages with images" ?p)
        (,(mapconcat 'identity
                     (mapcar
                      (lambda (maildir)
                        (concat "maildir:" (car maildir)))
                      mu4e-maildir-shortcuts) " OR ")
         "All inboxes" ?i)))

(setq mu4e-account-alist
      '(("gmail"
         ;; Under each account, set the account-specific variables you want.
         (mu4e-sent-messages-behavior delete)
         (user-mail-address "namy.19@gmail.com")
         ;; (mu4e-drafts-folder "/Gmail/[Gmail].Drafts")
         ;; (mu4e-sent-folder "/Gmail/[Gmail].Sent")
         ;; (mu4e-trash-folder "/Gmail/[Gmail].Trash")
         ;; (mu4e-refile-folder "/Gmail/[Gmail].Archive")
         (user-full-name "yqrashawn"))
        ("bimsop"
         (mu4e-sent-messages-behavior sent)
         (mu4e-sent-folder "/bimsop/Sent Messages")
         (mu4e-drafts-folder "/bimsop/Drafts")
         (user-mail-address "zhangyuxiao@bimsop.com")
         (user-full-name "张雨潇"))
        ("zoho"
         (mu4e-sent-messages-behavior sent)
         (mu4e-sent-folder "/zoho/Sent")
         (user-mail-address "hi@yqrashawn.com")
         (user-full-name "yqrashawn"))
        ("qq"
         (mu4e-sent-messages-behavior sent)
         (mu4e-sent-folder "/qq/Sent Messages")
         (user-mail-address "254651372@qq.com")
         (user-full-name "yqrashawn"))
        ))
(mu4e/mail-account-reset)
(with-eval-after-load 'mu4e-alert
  ;; Enable Desktop notifications
  ;; (mu4e-alert-set-default-style 'notifications)) ; For linux
  ;; (mu4e-alert-set-default-style 'libnotify))  ; Alternative for linux
  (mu4e-alert-set-default-style 'notifier))   ; For Mac OSX (through the
                                        ; terminal notifier app)
;; (mu4e-alert-set-default-style 'growl))      ; Alternative for Mac OSX


(with-eval-after-load 'mu4e-alert
  ;; Arrange to view messages in either the default browser or EWW
  (add-to-list 'mu4e-view-actions '("Eww view" . jcs-view-in-eww) t)
  ;; Contexts: One for each mail personality.
  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "namy.19@gmail.com"
             :enter-func (lambda () (mu4e-message "Enter namy.19@gmail.com context"))
             :leave-func (lambda () (mu4e-message "Leave namy.19@gmail.com context"))
             ;; Match based on the contact-fields of the message (that we are replying to)
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg
                                                                 :to "namy.19@gmail.com")))
             :vars '( ( user-mail-address      . "namy.19@gmail.com"  )
                      ( user-full-name         . "yqrashawn" )
                      ( smtpmail-smtp-server   . "smtp.gmail.com" )
                      ( smtpmail-smtp-service  . 587)
                      ( smtpmail-stream-type   . ssl)))

           ,(make-mu4e-context
             :name "hi@yqrashawn.com"
             :enter-func (lambda () (mu4e-message "Enter hi@yqrashawn.com context"))
             :leave-func (lambda () (mu4e-message "Leave hi@yqrashawn.com context"))
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg
                                                                 :to "hi@yqrashawn.com")))
             :vars '( ( user-mail-address      . "hi@yqrashawn.com"  )
                      ( user-full-name         . "yqrashawn" )
                      ( smtpmail-smtp-server   . "smtp.zoho.com" )
                      ( smtpmail-smtp-service  . 465)
                      ( smtpmail-stream-type   . ssl)))

           ,(make-mu4e-context
             :name "zhangyuxiao@bimsop.com"
             :enter-func (lambda () (mu4e-message "Enter zhangyuxiao@bimsop.com context"))
             :leave-func (lambda () (mu4e-message "Leave zhangyuxiao@bimsop.com context"))
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg
                                                                 :to "zhangyuxiao@bimsop.com")))
             :vars '( ( user-mail-address       . "zhangyuxiao@bimsop.com" )
                      ( user-full-name          . "张雨潇" )
                      ( smtpmail-smtp-server    . "smtp.exmail.qq.com" )
                      ( smtpmail-stream-type    . ssl)
                      ( smtpmail-smtp-service   . 465)))))
  )


;; start with the first (default) context;
(setq mu4e-context-policy 'pick-first)

;; compose with the current context if no context matches;
(setq mu4e-compose-context-policy nil)

;; (require mu4e)
;; (require mu4e-alert)

