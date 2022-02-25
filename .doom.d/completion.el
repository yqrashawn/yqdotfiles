;;; completion.el -*- lexical-binding: t; -*-

(after! prog-mode
  (set-company-backend! 'prog-mode
    'company-capf
    'company-files
    'company-yasnippet
    'company-keywords
    'company-dabbrev-code
    'company-dabbrev))

(after! text-mode
  (set-company-backend! 'text-mode
    '(:separate company-dabbrev company-yasnippet company-files company-ispell)))

(use-package! company-tabnine-capf
  :defer t
  :init
  (after! js2-mode
    (set-company-backend! 'js2-mode 'company-tabnine-capf))
  (after! rjsx-mode
    (set-company-backend! 'rjsx-mode 'company-tabnine-capf))
  (after! typescript-mode
    (set-company-backend! 'typescript-mode 'company-tabnine-capf))
  (after! conf-mode
    (set-company-backend! 'conf-mode
      'company-tabnine-capf
      'company-files
      'company-yasnippet
      'company-keywords
      'company-dabbrev-code
      'company-dabbrev)))

(use-package! company-flx
  :defer t
  :init (add-hook! emacs-lisp-mode #'company-flx-mode))

(defvar yq//company-numbers '(59 ?a ?s ?d ?f ?g ?h ?j ?k ?l))

(defun yq//company-format-numbers (numbered)
  (format " %s" (char-to-string (nth (mod numbered 10) yq//company-numbers))))

(after! company
  (setq! company-selection-wrap-around t
         company-show-numbers t
         company-frontends '(company-preview-frontend company-echo-frontend)
         company-require-match nil
         company-dabbrev-minimum-length 2
         company-search-regexp-function #'company-search-flex-regexp
         company-show-numbers-function 'yq//company-format-numbers)

  (setq-hook!
    '(clojure-mode-hook clojurescript-mode-hook clojurec-mode-hook)
    company-idle-delay 0)
  (setq-hook! '(js2-mode-hook rjsx-mode-hook js-mode-hook typescript-mode-hook)
    company-idle-delay 0)
  (after! eldoc
    (defadvice! +eldoc--message (orig-fn &optional string)
      :around #'eldoc--message
      (unless (company--active-p)
        (funcall orig-fn string))))

  (dotimes (i 10)
    (define-key! company-active-map
      (read-kbd-macro (format "M-%d" i)) #'company-complete-number
      (read-kbd-macro (format "C-x C-6 %d" i)) #'company-complete-number)))

(use-package! company-tabnine
  :defer t
  :commands (company-tabnine-restart-server)
  :init
  (setq! company-tabnine-binaries-folder "~/.TabNine/binaries/"
         ;; company-tabnine-context-radius 6000
         ;; company-tabnine-context-radius-after 6000
         company-tabnine-log-file-path "~/Downloads/tabnine.log"))

(use-package! company-ctags :defer t)

;; try fix company overlay performance
;; TODO: check if this works
(defadvice! +company-tng-frontend (orig command)
  :around #'company-tng-frontend
  (overlay-recenter (point))
  (setq-local inhibit-field-text-motion t)
  (funcall orig command))
(defadvice! +company-enable-overriding-keymap (orig keymap)
  :around #'company-enable-overriding-keymap
  (if keymap (setq-local inhibit-field-text-motion t)
    (setq-local inhibit-field-text-motion nil))
  (funcall orig keymap))

;; try to speed up the overlay
(defvar last-post-command-position 0
  "Holds the cursor position from the last run of post-command-hooks.")

(make-variable-buffer-local 'last-post-command-position)

(defun do-stuff-if-moved-post-command ()
  (let ((p (point)))
    (unless (equal p last-post-command-position)
      (overlay-recenter p))
    (setq last-post-command-position p)))

(add-hook! 'post-command-hook #'do-stuff-if-moved-post-command)

(use-package! capf-autosuggest
  :hook ((eshell-mode comint-mode) . capf-autosuggest-mode))

(after! company-box
  (setq! company-box-enable-icon nil
    company-box-color-icon nil
    company-box-max-candidates 15
    company-box-scrollbar nil
    company-box-doc-delay 1.5))

;; (use-package! corfu
;;   :hooks (doom-first-input-hook . corfu-global-mode))

(after! orderless
  (setq orderless-component-separator "[ ,j]"))

(defun +company-abort ()
  (when (fboundp 'company--active-p)
    (when (company--active-p)
      (company-abort))))

(after! evil
  (add-hook! 'evil-normal-state-entry-hook '+company-abort))
