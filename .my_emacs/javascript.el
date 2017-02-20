;; (eval-after-load 'js2-mode
;;   '(add-hook 'js2-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix))))
(setq babel-repl-cli-program "~/.npm-packages/bin/babel-node")

(defun my-js2-mode-hook ()
  (lambda ()
    (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
    (local-set-key (kbd "C-M-x") 'js-send-last-sexp-and-go)
    (local-set-key (kbd "C-c b") 'js-send-buffer)
    (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go)
    (local-set-key (kbd "C-c l") 'js-load-file-and-go)
    (local-set-key (kbd "]q") 'flycheck-next-error)
    (local-set-key (kbd "[q") 'flycheck-previous-error)
    ))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)
(add-hook 'js2-mode-hook #'jscs-indent-apply)
;; (add-hook 'js2-mode-hook #'jscs-fix-run-before-save)
(setq inferior-js-program-command "node")
(setq inferior-js-program-arguments '("--interactive"))
(setq inferior-js-mode-hook
      (lambda ()
        ;; We like nice colors
        (ansi-color-for-comint-mode-on)))

(add-hook 'js2-mode-hook
          (lambda ()
            (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
            (local-set-key (kbd "C-M-x") 'js-send-last-sexp-and-go)
            (local-set-key (kbd "C-c b") 'js-send-buffer)
            (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go)
            (local-set-key (kbd "C-c l") 'js-load-file-and-go)
            ))


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
(setq flycheck-eslint-rules-directories '("/Users/rashawnzhang"))
(setq-default save-place t)
(spacemacs/add-flycheck-hook 'web-mode)
(spacemacs/add-flycheck-hook 'js2-mode)
(spacemacs/add-flycheck-hook 'js-mode)
;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")
  ;;;;;;;;;;;; settings ;;;;;;;;;;;;;;
(setq-default js2-basic-offset 2)
(setq-default js-indent-level 2)
;; http://rejeep.github.io/emacs/javascript/js2-mode/yasnippet/2009/06/14/js2-mode-and-yasnippet.html
(eval-after-load 'js2-mode
  '(progn
     (define-key js2-mode-map (kbd "TAB")
       (lambda()
         (interactive)
         (let ((yas/fallback-behavior 'return-nil))
           (unless (yas/expand)
             (indent-for-tab-command)
             (if (looking-back "^\s*")
                 (back-to-indentation))))))))
