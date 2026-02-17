;;; slack.el -*- lexical-binding: t; -*-

(use-package! slack
  :defer t
  :commands (slack-start slack-stop slack-select-rooms slack-select-unread-rooms
                         slack-channel-select slack-im-select slack-search-from-messages)
  :init
  (setq
   slack-buffer-emojify t
   slack-update-quick t
   slack-prefer-current-team t
   slack-completing-read-function #'completing-read
   slack-display-team-name t
   slack-current-team "0G"
   slack-typing-visibility 'frame
   ;; slack-request-curl-options (append request-curl-options (list "--user-agent" ""))
   )
  

  :config
  ;; Teams are registered in not-secret.el via slack-register-team calls.
  ;; Example (add to ~/Dropbox/sync/not-secret.el):
  
  (slack-register-team
   :name "clojurians"
   :token +slack-clojurians-api-token
   :cookie +slack-clojurians-cookie
   :full-and-display-names nil
   ;; :default t
   :subscribed-channels nil)
  
  (slack-register-team
   :name "0G"
   :token +slack-0g-api-token
   :cookie +slack-0g-cookie
   :default t
   :full-and-display-names nil
   :modeline-name "0G"
   :visible-threads t
   :modeline-enabled t
   :subscribed-channels '((general)))

  (setq slack-buffer-function #'switch-to-buffer))

(use-package! alert
  :commands (alert)
  :config
  (setq alert-default-style 'notifier))
