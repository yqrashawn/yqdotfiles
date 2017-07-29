;; packages.el --- Indium Layer packages File for Spacemacs
;; Author: yqrashawn <namy.19@gmail.com>
(setq indium-packages '(indium))

(defun indium/init-indium ()
  (use-package indium
    :init
    (progn
      ;; add hook for js2 mode
      (add-hook 'js2-mode-hook #'init-indium)
      (spacemacs/declare-prefix-for-mode 'js2-mode "mi" "indium")
      (setq indium-chrome-executable "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode
        "ir" 'indium-run-node
        "iR" 'indium-connect-to-nodejs
        "iC" 'indium-run-chrome
        "ic" 'indium-connect-to-chrome
        )

      (defvar indium-repl-mode-map
        (let ((map (make-sparse-keymap)))
          (define-key map [return] #'indium-repl-return)
          (define-key map "\C-m" #'indium-repl-return)
          (define-key map (kbd "TAB") #'indium-repl--complete-or-indent)
          (define-key map [mouse-1] #'indium-follow-link)
          (define-key map (kbd "C-<return>") #'newline)
          (define-key map (kbd "C-c C-i") #'indium-repl-inspect)
          (define-key map (kbd "C-c C-u") #'indium-repl-clear-output)
          (define-key map (kbd "C-c C-z") #'indium-repl-pop-buffer)
          (define-key map (kbd "C-c C-q") #'indium-quit)
          (define-key map (kbd "C-c C-q") #'indium-quit)
          (define-key map (kbd "C-n") #'next-line)
          (define-key map (kbd "C-p") #'previous-line)
          (define-key map (kbd "M-p") #'indium-repl-previous-input)
          (define-key map (kbd "M-n") #'indium-repl-next-input)
          (easy-menu-define indium-repl-mode-menu map
            "Menu for Indium REPL"
            '("Indium REPL"
              ["Clear output" indium-repl-clear-output]
              ["Inspect" indium-repl-inspect]
              "--"
              ["Switch to source buffer" indium-repl-pop-buffer]
              "--"
              ["Quit" indium-quit]))
          map))
      )))
