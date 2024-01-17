;;; completion.el -*- lexical-binding: t; -*-

(when (modulep! :completion company)
  (set-company-backend! +lispy-modes
                        'company-capf
                        'company-files
                        'company-yasnippet
                        'company-keywords
                        'company-dabbrev-code
                        'company-dabbrev)

  (set-company-backend! '(prog-mode js2-mode rjsx-mode typescript-mode conf-mode)
                        'company-tabnine
                        'company-capf
                        'company-files
                        'company-yasnippet
                        'company-keywords
                        'company-dabbrev-code
                        'company-dabbrev)

  (set-company-backend! 'text-mode
                        '(:separate company-dabbrev company-yasnippet company-files company-ispell)))

;; (use-package! company-flx
;;   :defer t
;;   :init (add-hook! emacs-lisp-mode #'company-flx-mode))

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
  :after company
  :commands (company-tabnine-restart-server)
  :init
  (setq!
   company-tabnine-binaries-folder "~/.TabNine/binaries/"
   company-tabnine-wait 0.25
   company-tabnine-max-num-results 5
   company-tabnine-no-continue nil
   ;; company-tabnine-context-radius 6000
   ;; company-tabnine-context-radius-after 6000
   company-tabnine-log-file-path "~/Downloads/tabnine.log"))

;; (use-package! company-ctags :defer t)

(after! company
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
    (funcall orig keymap)))

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
         company-box-doc-delay 1.5)
  (defadvice! +company-box--set-mode (&rest args)
    "stop company-box add company-pseudo-tooltip-frontend back"
    :after #'company-box--set-mode
    (delq! 'company-pseudo-tooltip-frontend company-frontends)
    (delq! 'company-pseudo-tooltip-unless-just-one-frontend company-frontends)))

;; (use-package! corfu
;;   :hook (doom-first-input . corfu-global-mode)
;;   :init
;;   (setq! corfu-auto t))

(after! orderless
  (setq orderless-component-separator "[ ,j]"))

(defun +company-abort ()
  (when (fboundp 'company--active-p)
    (when (company--active-p)
      (company-abort))))

(after! evil
  (when (modulep! :completion company)
    (add-hook! 'evil-normal-state-entry-hook '+company-abort)))

(use-package! fuz
  :after orderless
  :init
  (set-popup-rule! "^\\*fuz compilation\\*" :select nil :side 'bottom :size 0.2 :quit t)
  :config
  (unless (require 'fuz-core nil t)
    (fuz-build-and-load-dymod))
  (setq! fussy-score-fn 'fussy-fuz-score))

(use-package! fussy
  :after fuz
  :config
  (setq! fussy-filter-fn #'fussy-filter-default
         fussy-ignore-case t
         fussy-use-cache t
         fussy-default-regex-fn #'fussy-pattern-flex-2)
  ;; (pushnew! completion-styles 'fussy)

  (after! corfu
    (advice-add 'corfu--capf-wrapper :before 'fussy-wipe-cache)
    (add-hook 'corfu-mode-hook
              (lambda ()
                (setq-local fussy-max-candidate-limit 5000
                            fussy-default-regex-fn 'fussy-pattern-first-letter
                            fussy-prefer-prefix nil))))

  (defadvice! +read-extended-command (orig-fn &optional prompt)
    :around #'read-extended-command
    (let ((completion-styles '(fussy orderless basic)))
      (funcall orig-fn prompt))))

(use-package! hotfuzz
  :after orderless
  :init
  (setq! completion-ignore-case t)
  :config
  (require 'hotfuzz-module nil t)
  (pushnew! completion-styles 'hotfuzz)
  ;; (setq! fussy-score-fn 'fussy-hotfuzz-score)
  )

(after! cape
  (setq! cape-dict-file (list
                         (expand-file-name "~/Dropbox/sync/personal_dict")
                         (expand-file-name "~/Dropbox/sync/gh_username_dict")
                         "/usr/share/dict/words")))
