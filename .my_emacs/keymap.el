;; -*- mode: emacs-lisp -*-
;; (defun sr-speedbar-open-and-select ()
;;   "Open sr-speedbar window and select it."
;;   (interactive)
;;   (sr-speedbar-open)
;;   (sr-speedbar-select-window))

;; (defun my-toggle-speedbar ()
;;   "Toggle sr-speedbar window and select it.
;; Toggle visibility of sr-speedbar by resizing
;; the `sr-speedbar-window' to a minimal width
;; or the last width when visible.
;; Use this function to create or toggle visibility
;; of a speedbar-window.  It will be created if necessary."
;;   (interactive)
;;   (if (sr-speedbar-exist-p)
;;       (sr-speedbar-close)
;;     (sr-speedbar-open-and-select)))

(global-set-key (kbd "C-s") 'phi-search)

;; dired
(define-key dired-mode-map (kbd "l") 'dired-find-file)
(define-key dired-mode-map (kbd "h") 'dired-up-directory)

;; spacemacs leader
(spacemacs/set-leader-keys "sj" 'imenu-anywhere)
(spacemacs/set-leader-keys "ss" 'counsel-imenu)
(spacemacs/set-leader-keys "gf" 'magit-file-popup)
(spacemacs/set-leader-keys "wz" 'spacemacs/toggle-maximize-buffer)
(spacemacs/set-leader-keys "t0" 'centered-cursor-mode)
(spacemacs/set-leader-keys "hc" 'hide/show-comments-toggle)

(spacemacs/declare-prefix "o" "own-prefix")
(spacemacs/declare-prefix "oe" "edit-prefix")
(spacemacs/set-leader-keys "oey" 'evil-cp-yank-enclosing)
(spacemacs/set-leader-keys "oed" 'evil-cp-delete-enclosing)
(spacemacs/set-leader-keys "oec" 'evil-cp-change-enclosing)

;; global
(global-set-key (kbd "C-SPC") 'swiper)
(global-set-key (kbd "s-/") 'evilnc-comment-or-uncomment-lines)
(global-set-key (kbd "^@") 'swiper)
(global-set-key (kbd "S-s-<mouse-1>") 'mc/add-cursor-on-click)

;; evil global
(evil-global-set-key 'normal (kbd "C-w z") 'spacemacs/toggle-maximize-buffer)

;; evil normal
(define-key evil-normal-state-map "zl" 'hs-hide-level)
(define-key evil-normal-state-map "gd" 'evil-goto-definition)
(define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "u") 'undo-tree-undo)
(define-key evil-normal-state-map (kbd "d") 'evil-delete)
(define-key evil-normal-state-map (kbd "gy") 'duplicate-line)
(define-key evil-normal-state-map (kbd "gY") 'spacemacs/copy-and-comment-lines)
(define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo)
(define-key evil-normal-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
(define-key evil-normal-state-map (kbd "zz") 'evil-toggle-fold)
(define-key evil-normal-state-map (kbd "C-k") 'evil-toggle-fold)
(define-key evil-normal-state-map (kbd "za") 'evil-scroll-line-to-center)
(define-key evil-normal-state-map (kbd "ga") 'evil-cp-insert-at-end-of-form)
(define-key evil-normal-state-map (kbd "gi") 'evil-cp-insert-at-beginning-of-form)
(define-key evil-normal-state-map (kbd "gI") 'evil-insert-resume)

;; evil visual
(define-key evil-visual-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
(define-key evil-visual-state-map (kbd "C-a") 'mwim-beginning-of-code-or-line)

;; evil insert
(define-key evil-insert-state-map (kbd "C-j") 'evil-ret-and-indent)
(define-key evil-insert-state-map (kbd "C-v") 'forward-word)
(define-key evil-insert-state-map (kbd "C-k") 'backward-word)
(define-key evil-insert-state-map (kbd "C-d") 'delete-forward-char)
(define-key evil-insert-state-map (kbd "C-n") 'mc/mark-next-like-this-word)
(define-key evil-insert-state-map (kbd "C-p") 'mc/mark-previous-like-this-word)
(define-key evil-insert-state-map (kbd "M-n") 'mc/mark-next-like-this)
(define-key evil-insert-state-map (kbd "M-p") 'mc/mark-previous-like-this)
(define-key evil-insert-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
(define-key evil-insert-state-map (kbd "C-a") 'mwim-beginning-of-code-or-line)
(define-key evil-insert-state-map (kbd "C-l") 'evil-complete-next)
(define-key evil-insert-state-map (kbd "C-S-n") 'mc/skip-to-next-like-this)
(define-key evil-insert-state-map (kbd "C-S-p") 'mc/skip-to-previous-like-this)

;; evil motion
(define-key evil-motion-state-map "e" 'evil-cp-forward-symbol-end)
(define-key evil-motion-state-map "w" 'evil-cp-forward-symbol-begin)
(define-key evil-motion-state-map "b" 'evil-cp-backward-symbol-end)

;;;;;C-h
(keyboard-translate ?\C-h ?\C-?)
(global-set-key [(control ?h)] 'delete-backward-char)

;; ivy-minibuffer-map
(define-key ivy-minibuffer-map (kbd "C-n") 'ivy-next-history-element)
(define-key ivy-minibuffer-map (kbd "C-p") 'ivy-previous-history-element)

;; remap s
(define-key evil-normal-state-map "s" nil)
(define-key evil-normal-state-map "sf" 'counsel-find-file)
(define-key evil-visual-state-map "sa" 'avy-goto-word-or-subword-1)
(define-key evil-normal-state-map "sk" 'spacemacs/kill-this-buffer)
(define-key evil-normal-state-map "sj" 'evil-window-delete)
(define-key evil-normal-state-map "sl" 'counsel-imenu)
(define-key evil-normal-state-map "sss" 'spacemacs/search-ack)
(define-key evil-normal-state-map "sp" 'evil-jump-item)
(define-key evil-normal-state-map "sv" 'er/expand-region)
