(setq erc-autojoin-timing 'ident)
(setq erc-prompt-for-nickserv-password nil)
(load "~/.my_emacs/secret.el.gpg")
(setq erc-email-userid "namy.19@gmail.com"
      erc-nick "yqrashawn"
      erc-port "6667")

(setq erc-nickserv-passwords
      (quote
       ((irc\.gitter\.im
         (("yqrashawn" . ,gitter-token)))
        (freenode
         (("yqrashawn" . ,freenode-pass))))))

(setq erc-server-list
   '(("irc.gitter.im"
      :port "6667"
      :nick "yqrashawn"
      :ssl t
      )))


