;; packages.el --- Indium Layer packages File for Spacemacs
;; Author: yqrashawn <namy.19@gmail.com>
(setq indium-packages '(indium))

(defun indium/init-indium ()
  (use-package indium
    :init
    (progn
      ;; add hook for js2 mode
      (spacemacs/declare-prefix-for-mode 'js2-mode "mi" "indium")
      (setq indium-chrome-executable "/Applications/Google Chrome Canary.app/Contents/MacOS/Google Chrome Canary")

      (spacemacs/declare-prefix-for-mode 'js2-mode "mi" "indium")
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode
        "inr" 'indium-run-node
        "inc" 'indium-connect-to-nodejs
        "icr" 'indium-run-chrome
        "icC" 'indium-toggle-v8-cache
        "icc" 'indium-connect-to-chrome
        "ibb" 'indium-add-breakpoint
        "ibB" 'indium-add-conditional-breakpoint
        "ibk" 'indium-remove-breakpoint
        "ibK" 'indium-remove-all-breakpoints-from-buffer
        "ibl" 'indium-list-breakpoint)


      (defvar indium-inspector-mode-map
        (let ((map (make-sparse-keymap)))
          (define-key map [return] #'indium-follow-link)
          (define-key map "\C-m" #'indium-follow-link)
          (define-key map [mouse-1] #'indium-follow-link)
          (evil-define-key 'normal map "gl" #'indium-inspector-pop)
          (evil-define-key 'normal map "gr" #'indium-inspector-refresh)
          (evil-define-key 'normal map "n" #'indium-inspector-refresh)
          (evil-define-key 'normal map "p" #'indium-inspector-refresh)
          (evil-define-key 'normal map "q" #'quit-window)
          (define-key map "l" #'indium-inspector-pop)
          (define-key map "g" #'indium-inspector-refresh)
          (define-key map "n" #'indium-inspector-next-reference)
          (define-key map "p" #'indium-inspector-previous-reference)
          (define-key map [tab] #'indium-inspector-next-reference)
          (define-key map [backtab] #'indium-inspector-previous-reference)
          map))

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
          ;; (define-key map (kbd "C-n") #'next-line)
          ;; (define-key map (kbd "C-p") #'previous-line)
          (evil-define-key 'normal map "q" #'quit-window)
          (evil-define-key 'normal map (kbd "C-p") #'indium-repl-previous-input)
          (evil-define-key 'normal map (kbd "C-n") #'indium-repl-next-input)
          (evil-define-key 'insert map (kbd "C-p") #'indium-repl-previous-input)
          (evil-define-key 'insert map (kbd "C-n") #'indium-repl-next-input)
          (define-key map (kbd "C-p") #'indium-repl-previous-input)
          (define-key map (kbd "C-n") #'indium-repl-next-input)
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

      (defvar indium-debugger-mode-map
        (let ((map (make-sparse-keymap)))
          (define-key map " " #'indium-debugger-step-over)
          (define-key map (kbd "H-'") #'indium-debugger-step-over)
          (define-key map (kbd "H-;") #'indium-debugger-step-into)
          (define-key map (kbd "i") #'indium-debugger-step-into)
          (define-key map (kbd "o") #'indium-debugger-step-out)
          (define-key map (kbd "H-:") #'indium-debugger-step-out)
          (define-key map (kbd "c") #'indium-debugger-resume)
          (define-key map (kbd "H-\\") #'indium-debugger-resume)
          (define-key map (kbd "l") #'indium-debugger-locals)
          (define-key map (kbd "H-L") #'indium-debugger-locals)
          (define-key map (kbd "s") #'indium-debugger-stack-frames)
          (define-key map (kbd "H-S") #'indium-debugger-stack-frames)
          (define-key map (kbd "q") #'indium-debugger-resume)
          (define-key map (kbd "h") #'indium-debugger-here)
          (define-key map (kbd "H-\"") #'indium-debugger-here)
          (define-key map (kbd "e") #'indium-debugger-evaluate)
          (define-key map (kbd "H-E") #'indium-debugger-evaluate)
          (define-key map (kbd "n") #'indium-debugger-next-frame)
          (define-key map (kbd "p") #'indium-debugger-previous-frame)
          (define-key map (kbd "H-N") #'indium-debugger-next-frame)
          (define-key map (kbd "H-P") #'indium-debugger-previous-frame)
          (easy-menu-define indium-debugger-mode-menu map
            "Menu for Indium debugger"
            '("Indium Debugger"
              ["Resume" indium-debugger-resume]
              ["Step over" indium-debugger-step-over]
              ["Step into" indium-debugger-step-into]
              ["Step out" indium-debugger-step-out]
              ["Jump here" indium-debugger-here]
              "--"
              ["Inspect locals" indium-debugger-locals]
              ["Show stack" indium-debugger-stack-frames]
              "--"
              ["Evaluate" indium-debugger-evaluate]
              "--"
              ["Jump to the next frame" indium-debugger-next-frame]
              ["Jump to the previous frame" indium-debugger-previous-frame]))
          map)))))
