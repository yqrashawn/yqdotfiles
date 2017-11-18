(setq babel-repl-cli-program "~/.npm-packages/bin/babel-node")
(setq vue-html-color-interpolations t)

(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'typescript-mode-hook 'prettier-js-mode)
(add-hook 'react-mode-hook 'prettier-js-mode)
(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
          (funcall (cdr my-pair)))))
(add-hook 'web-mode-hook #'(lambda ()
                             (enable-minor-mode
                              '("\\.jsx?\\'" . prettier-js-mode))))

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

;; (defun my-web-mode-hook ()
;; (smartparens-mode 0))
;; (add-hook 'web-mode-hook  'my-web-mode-hook)
(with-eval-after-load 'web-mode
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))
(setq standard-indent 2)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-element-content-fontification t)
(setq web-mode-enable-element-tag-fontification t)
(setq web-mode-enable-html-entities-fontification t)
(setq web-mode-enable-part-face nil)
(setq web-mode-enable-block-face nil)
(setq web-mode-enable-auto-expanding t)
(setq web-mode-code-indent-offset 2)
(setq web-mode-indent-style 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-attr-indent-offset 2)
(setq web-mode-attr-value-indent-offset 2)
(setq web-mode-script-padding 2)
(setq web-mode-style-padding 2)

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
(setq eslintd-fix-executable (concat user-home-directory ".npm-packages/bin/eslint_d"))
(setq flycheck-javascript-eslint-executable "eslint")
(setq flycheck-disabled-checkers (quote (javascript-jshint javascript-jscs)))

;; (setq-default save-place t)
(setq js2-mode-show-parse-errors t)
(setq js2-mode-show-strict-warnings nil)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;;;;;;;;;;;; settings ;;;;;;;;;;;;;;
(setq-default js-indent-level 2)

;; Enable JavaScript completion between <script>...</script> etc.
;; (defadvice company-tern (before web-mode-set-up-ac-sources activate)
;;   "Set `tern-mode' based on current language before running company-tern."
;;   (if (equal major-mode 'web-mode)
;;       (let ((web-mode-cur-language
;;              (web-mode-language-at-pos)))
;;         (if (or (string= web-mode-cur-language "javascript")
;;                 (string= web-mode-cur-language "jsx")
;;                 )
;;             (unless tern-mode (tern-mode))
;;           (if tern-mode (tern-mode -1))))))

(add-hook 'web-mode-hook 'web-narrow-mode)
(use-package web-narrow-mode
  :mode (("\\.vue\\'" . web-mode)("\\.html\\'" . web-mode) ("\\.jsx\\'" . react-mode))
  :commands (web-narrow-to-element
             web-narrow-to-region
             web-narrow-to-block
             web-narrow-mode)
  :defer
  :config)
(setq js-doc-author "yqrashawn")

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))