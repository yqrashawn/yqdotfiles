;;; tty.el -*- lexical-binding: t; -*-
(load! "./autoloads.el")

(setq terminal-keybinding-fix-prefix "M-+ M-+ ")
(setq ctrl-keys-to-remap '(?\; ?: ?' ?\" ?. ?> ?, ?< ?/ ?? ?- ?= ?+))
(setq command-keys-to-remap '(?k))

(dolist (char ctrl-keys-to-remap)
  (let ((from (concat terminal-keybinding-fix-prefix "C " (char-to-string char)))
        (to (concat "C-" (char-to-string char))))
    (define-key key-translation-map (kbd from) (kbd to))))

(dolist (char command-keys-to-remap)
  (let ((from (concat terminal-keybinding-fix-prefix "D " (char-to-string char)))
        (to (concat "s-" (char-to-string char))))
    (define-key key-translation-map (kbd from) (kbd to))))

;; (add-hook 'evil-insert-state-entry-hook (lambda () (when (not (display-graphic-p)) (send-string-to-terminal "\033[5 q"))))
;; (add-hook 'evil-normal-state-entry-hook (lambda () (when (not (display-graphic-p)) (send-string-to-terminal "\033[0 q"))))

(define-key key-translation-map (kbd (concat terminal-keybinding-fix-prefix "W")) (kbd "C-M-s-7"))

(use-package! emamux
  :commands (emamux:tmux-run-command emamux:check-tmux-running)
  :when (not (display-graphic-p))
  :init
  (define-key!
    [remap windmove-up] (cmd! () (yq/tmux-select-pane "up"))
    [remap evil-window-up] (cmd! () (yq/tmux-select-pane "up"))
    [remap windmove-down] (cmd! () (yq/tmux-select-pane "down"))
    [remap evil-window-down] (cmd! () (yq/tmux-select-pane "down"))
    [remap windmove-left] (cmd! () (yq/tmux-select-pane "left"))
    [remap evil-window-left] (cmd! () (yq/tmux-select-pane "left"))
    [remap windmove-right] (cmd! () (yq/tmux-select-pane "right"))
    [remap evil-window-right] (cmd! () (yq/tmux-select-pane "right"))
    [remap split-window-below] 'yq/split-window-below-tmux
    [remap split-window-right] 'yq/split-window-right-tmux
    [remap evil-window-vsplit] 'yq/split-window-right-tmux
    [remap evil-window-split] 'yq/split-window-below-tmux
    [remap spacemacs/toggle-maximize-buffer] 'yq/toggle-maximize-buffer-tmux))
