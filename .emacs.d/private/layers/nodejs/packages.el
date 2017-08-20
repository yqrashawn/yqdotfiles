(setq nodejs-packages '(nodejs-repl))
(defun nodejs/init-nodejs ()
  (use-package nodejs-repl
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'js2-mode "mn" "nodejs")
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode
        "nn" 'nodejs-repl-send-last-expression
        "nj" 'nodejs-repl-send-reglin
        "nl" 'nodejs-repl-send-line
        "nf" 'nodejs-repl-load-file
        "nk" 'nodejs-repl-switch-to-repl
        )
      (add-hook 'js2-mode-hook
                (lambda ()
                  (define-key js2-mode-map (kbd "C-x C-e") 'nodejs-repl-send-last-expression)
                  (define-key js2-mode-map (kbd "C-c C-j") 'nodejs-repl-send-line)
                  (define-key js2-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
                  (define-key js2-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
                  (define-key js2-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl)))
      )))
