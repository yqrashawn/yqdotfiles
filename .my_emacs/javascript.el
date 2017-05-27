;; (eval-after-load 'js2-mode
;;   '(add-hook 'js2-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix))))
(setq babel-repl-cli-program "~/.npm-packages/bin/babel-node")

(defun my-js2-mode-hook ()
  (lambda ()
    (loccal-set-key (kbd "C-x C-e") 'nodejs-repl-send-last-sexp)
    (loccal-set-key (kbd "C-c C-r") 'nodejs-repl-send-region)
    (loccal-set-key (kbd "C-c C-l") 'nodejs-repl-load-file)
    (loccal-set-key (kbd "C-c C-z") 'nodejs-repl-switch-to-repl)
    ))
(require 'indium)
(add-hook 'js2-mode-hook #'indium-interaction-mode)
;; (add-hook 'js2-mode-hook 'my-js2-mode-hook)
;; (add-hook 'js2-mode-hook #'jscs-indent-apply)
;; (add-hook 'js2-mode-hook #'jscs-fix-run-before-save)
;; (setq inferior-js-program-command "node")
;; (setq inferior-js-program-arguments '("--interactive"))
;; (setq inferior-js-mode-hook
;;       (lambda ()
;;         ;; We like nice colors
;;         (ansi-color-for-comint-mode-on)))

;; (add-hook 'js2-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
;;             (local-set-key (kbd "C-M-x") 'js-send-last-sexp-and-go)
;;             (local-set-key (kbd "C-c b") 'js-send-buffer)
;;             (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go)
;;             (local-set-key (kbd "C-c l") 'js-load-file-and-go)
;;             ))


;;;;;;;;;;;;; web-mode ;;;;;;;;;;;;;
;; adjust indents for web-mode to 2 spaces
(defun my-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
  ;;; http://web-mode.org/
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)
;; for better jsx syntax-highlighting in web-mode
;; - courtesy of Patrick @halbtuerke
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

  ;;;;;;;;;;;;; flycheck ;;;;;;;;;;;;;
(setq flycheck-javascript-eslint-executable "eslint_d")
(setq flycheck-eslint-rules-directories '("/Users/rashawnzhang"))
(setq-default save-place t)
(spacemacs/add-flycheck-hook 'web-mode)
(spacemacs/add-flycheck-hook 'vue-mode)
(spacemacs/add-flycheck-hook 'js2-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")
  ;;;;;;;;;;;; settings ;;;;;;;;;;;;;;
(setq-default js2-basic-offset 2)
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
