;; (eval-after-load 'js2-mode
;;   '(add-hook 'js2-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix))))
(setq babel-repl-cli-program "~/.npm-packages/bin/babel-node")

;; (add-to-list 'spacemacs-jump-handlers-js2-mode '(dumb-jump-go :async t))
;;;;;;;;;;;;; web-mode ;;;;;;;;;;;;;
;; adjust indents for web-mode to 2 spaces
;; (defun my-web-mode-hook ()
;;   "Hooks for Web mode. Adjust indents"
;;   ;;; http://web-mode.org/
;;   (setq web-mode-markup-indent-offset 2)
;;   (setq web-mode-css-indent-offset 2)
;;   (setq web-mode-code-indent-offset 2))
;; (add-hook 'web-mode-hook  'my-web-mode-hook)

;; for better jsx syntax-highlighting in web-mode
;; - courtesy of Patrick @halbtuerke
;; (defadvice web-mode-highlight-part (around tweak-jsx activate)
;;   (if (equal web-mode-content-type "jsx")
;;       (let ((web-mode-enable-part-face nil))
;;         ad-do-it)
;;     ad-do-it))
(setq standard-indent 2)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-element-content-fontification t)
(setq web-mode-enable-element-tag-fontification t)
(setq web-mode-enable-html-entities-fontification t)
(setq web-mode-enable-part-face t)
(setq web-mode-enable-block-face t)
(setq web-mode-enable-auto-expanding t)
(setq web-mode-code-indent-offset 2)
(setq web-mode-indent-style 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-attr-indent-offset 2)
(setq web-mode-attr-value-indent-offset 2)

(defun nodejs-repl-real-quit ()
  (interactive)
  (nodejs-repl-quit-or-cancel)
  (nodejs-repl-quit-or-cancel)
  )
(defun nodejs-repl-resend-buffer ()
  (interactive)
  (nodejs-repl-quit-or-cancel)
  (nodejs-repl-send-buffer)
  )

(use-package nodejs-repl
  :init
  (progn
    (spacemacs/declare-prefix-for-mode 'js2-mode "mn" "nodejs")
    (spacemacs/set-leader-keys-for-major-mode 'js2-mode
      "ne" 'nodejs-repl-send-last-expression
      "nk" 'nodejs-repl-send-reglin
      "nj" 'nodejs-repl-send-line
      "nf" 'nodejs-repl-load-file
      "n," 'nodejs-repl-send-buffer
      "n." 'nodejs-repl-real-quit
      "nl" 'nodejs-repl-switch-to-repl
      "nn" 'nodejs-repl-resend-buffer
      )
    (add-hook 'js2-mode-hook
              (lambda ()
                (define-key js2-mode-map (kbd "C-x C-e") 'nodejs-repl-send-last-expression)
                (define-key js2-mode-map (kbd "C-c C-j") 'nodejs-repl-send-line)
                (define-key js2-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
                (define-key js2-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
                (define-key js2-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl)))
    ))

  ;;;;;;;;;;;;; flycheck ;;;;;;;;;;;;;
(setq flycheck-javascript-eslint-executable "eslint_d")
(setq flycheck-disabled-checkers (quote (javascript-jshint javascript-jscs)))

;; (setq-default save-place t)
(setq js2-mode-show-parse-errors t)
(setq js2-mode-show-strict-warnings nil)
(spacemacs/add-flycheck-hook 'web-mode)
(spacemacs/add-flycheck-hook 'vue-mode)
(spacemacs/add-flycheck-hook 'js2-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")
  ;;;;;;;;;;;; settings ;;;;;;;;;;;;;;
(setq-default js-indent-level 2)

(defun eslint-fix ()
  (interactive)
  (let ((current-point (point))
        (line (count-screen-lines (window-start) (point)))
        (command (concat
                  "eslint_d"
                  " --stdin"
                  " --fix-to-stdout"
                  " --stdin-filename " buffer-file-name))
        (buffer (current-buffer))
        (text (buffer-substring-no-properties (point-min) (point-max))))
    (with-temp-buffer
      (insert text)
      (when (eq 0
                (shell-command-on-region
                 ;; Region
                 (point-min)
                 (point-max)
                 ;; Command
                 command
                 ;; Output to current buffer
                 t
                 ;; Replace buffer
                 t
                 ;; Error buffer name
                 "*eslint-fix error*"))
        (let ((fixed-text (buffer-substring-no-properties (point-min) (point-max))))
          (with-current-buffer buffer
            (delete-region (point-min) (point-max))
            (insert fixed-text)
            ;; Restore point and scroll position
            (goto-char current-point)
            (recenter (- line 1))))))))
