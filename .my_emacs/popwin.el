(push '("*Messages*" :dedicated t :position bottom :stick nil :noselect nil) popwin:special-display-config)
(push '("*ycmd-server*" :dedicated t :position bottom :stick nil :noselect t) popwin:special-display-config)
(push '("*Compile-Log*" :dedicated t :position bottom :stick nil :noselect t) popwin:special-display-config)
(push '("*quickrun*" :dedicated t :position bottom :stick t :noselect t) popwin:special-display-config)
(push '("*warnings*" :dedicated t :position bottom :stick nil :noselect t) popwin:special-display-config)
(push '("*git-messenger*" :dedicated t :position bottom :stick nil :noselect nil) popwin:special-display-config)

;; purpose setting
(add-to-list 'purpose-user-mode-purposes '(prog-mode . prog))
(add-to-list 'purpose-user-mode-purposes '(circe-mode . irc))
(add-to-list 'purpose-user-mode-purposes '(circe-chat-mode . irc))
(add-to-list 'purpose-user-mode-purposes '(circe-query-mode . irc))
(add-to-list 'purpose-user-mode-purposes '(circe-lagmon-mode . irc))
(add-to-list 'purpose-user-mode-purposes '(circe-server-mode . irc))
(purpose-compile-user-configuration)


