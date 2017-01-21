;;; funcs.el --- Auto-completion functions File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3



(spacemacs|add-toggle auto-completion
  :status
  (if (eq 'company auto-completion-front-end)
      (bound-and-true-p company-mode)
    (bound-and-true-p auto-complete-mode))
  :on
  (progn
    (if (eq 'company auto-completion-front-end)
        (company-mode)
      (auto-complete-mode))
    (message "Enabled auto-completion (using %S)."
             auto-completion-front-end))
  :off
  (progn
    (if (eq 'company auto-completion-front-end)
        (company-mode -1)
      (auto-complete-mode -1))
    (message "Disabled auto-completion."))
  :documentation "Enable auto-completion."
  :evil-leader "ta")


;; auto-completion key bindings functions

(defun spacemacs//auto-completion-set-RET-key-behavior (package)
  "Bind RET key appropriately for the given PACKAGE and value of
`auto-completion-return-key-behavior'."
  (cond
   ((eq 'company package)
    (let ((map company-active-map))
      (cond
       ((eq 'complete auto-completion-return-key-behavior)
        (define-key map [return] 'company-complete-selection)
        (define-key map (kbd "RET") 'company-complete-selection))
       (t
        (define-key map [return] 'nil)
        (define-key map (kbd "RET") 'nil)))))
   (t (message "Not yet implemented for package %S" package))))

(defun spacemacs//auto-completion-set-TAB-key-behavior (package)
  "Bind TAB key appropriately for the given PACKAGE and value of
`auto-completion-tab-key-behavior'."
  (cond
   ((eq 'company package)
    (let ((map company-active-map))
      (cond
       ((eq 'complete auto-completion-tab-key-behavior)
        (define-key map (kbd "TAB") 'company-complete-selection)
        (define-key map (kbd "<tab>") 'company-complete-selection))
       ((eq 'cycle auto-completion-tab-key-behavior)
        (define-key map (kbd "TAB") 'company-complete-common-or-cycle)
        (define-key map (kbd "<tab>") 'company-complete-common-or-cycle)
        (define-key map (kbd "<S-tab>")
          'spacemacs//company-complete-common-or-cycle-backward)
        (define-key map (kbd "<backtab>")
          'spacemacs//company-complete-common-or-cycle-backward))
       (t
        (define-key map (kbd "TAB") nil)
        (define-key map (kbd "<tab>") nil)))))
   (t (message "Not yet implemented for package %S" package))))

(defun spacemacs//auto-completion-setup-key-sequence (package)
  "Setup the key sequence to complete current selection."
  (when auto-completion-complete-with-key-sequence
    (let ((first-key (elt auto-completion-complete-with-key-sequence 0)))
      (cond ((eq 'company package)
             (define-key company-active-map (kbd (char-to-string first-key))
               'spacemacs//auto-completion-key-sequence-start))
            (t (message "Not yet implemented for package %S" package))))))


;; Editing style

(defun spacemacs//company-active-navigation (style)
  "Set navigation for the given editing STYLE."
  (cond
   ((or (eq 'vim style)
        (and (eq 'hybrid style)
             hybrid-mode-enable-hjkl-bindings))
    (let ((map company-active-map))
      (define-key map (kbd "C-j") 'company-select-next)
      (define-key map (kbd "C-k") 'company-select-previous)
      (define-key map (kbd "C-l") 'company-complete-selection))
    (when (require 'company-quickhelp nil 'noerror)
      (evil-define-key 'insert company-quickhelp-mode-map (kbd "C-k") 'company-select-previous)))
   (t
    (let ((map company-active-map))
      (define-key map (kbd "C-n") 'company-select-next)
      (define-key map (kbd "C-p") 'company-select-previous)
      (define-key map (kbd "C-f") 'company-complete-selection)))))


;; Transformers

(defun spacemacs//company-transformer-cancel (candidates)
  "Cancel completion if prefix is in the list
`company-mode-completion-cancel-keywords'"
  (unless (member company-prefix company-mode-completion-cancel-keywords)
    candidates))



(defvar-local company-fci-mode-on-p nil)

(defun company-turn-off-fci (&rest ignore)
  (when (boundp 'fci-mode)
    (setq company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))

(defun company-maybe-turn-on-fci (&rest ignore)
  (when company-fci-mode-on-p (fci-mode 1)))


;; Yasnippet

(defun spacemacs/load-yasnippet ()
  (unless yas-global-mode (yas-global-mode 1))
  (yas-minor-mode 1))

(defun spacemacs/force-yasnippet-off ()
  (yas-minor-mode -1)
  (setq yas-dont-activate t))


;; Yasnippet and Smartparens

;; If enabled, smartparens will mess snippets expanded by `hippie-expand`.
;; We want to temporarily disable Smartparens during the snippet expansion and
;; switch it back to the initial state when done.
;;
;; However, there is an asymmetry in Yasnippet's hooks:
;; * `yas-before-expand-snippet-hook' is called for all snippet expansions,
;; including the nested ones.
;; * `yas-after-exit-snippet-hook' is called only for the top level snippet,
;; but NOT for the nested ones.
;;
;; That's why we introduce `spacemacs--yasnippet-expanding' below.

(defvar spacemacs--smartparens-enabled-initially t
  "Stored whether smartparens is originally enabled or not.")
(defvar spacemacs--yasnippet-expanding nil
  "Whether the snippet expansion is in progress.")

(defun spacemacs//smartparens-disable-before-expand-snippet ()
  "Handler for `yas-before-expand-snippet-hook'.
Disable smartparens and remember its initial state."
  ;; Remember the initial smartparens state only once, when expanding a top-level snippet.
  (unless spacemacs--yasnippet-expanding
    (setq spacemacs--yasnippet-expanding t
          spacemacs--smartparens-enabled-initially smartparens-mode))
  (smartparens-mode -1))

(defun spacemacs//smartparens-restore-after-exit-snippet ()
  "Handler for `yas-after-exit-snippet-hook'.
 Restore the initial state of smartparens."
  (setq spacemacs--yasnippet-expanding nil)
  (when spacemacs--smartparens-enabled-initially
    (smartparens-mode 1)))
