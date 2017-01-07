;; -*- mode: emacs-lisp -*-
(defun sr-speedbar-open-and-select ()
  "Open sr-speedbar window and select it."
  (interactive)
  (sr-speedbar-open)
  (sr-speedbar-select-window))

(defun my-toggle-speedbar ()
  "Toggle sr-speedbar window and select it.
Toggle visibility of sr-speedbar by resizing
the `sr-speedbar-window' to a minimal width
or the last width when visible.
Use this function to create or toggle visibility
of a speedbar-window.  It will be created if necessary."
  (interactive)
  (if (sr-speedbar-exist-p)
      (sr-speedbar-close)
    (sr-speedbar-open-and-select)))

(global-set-key (kbd "C-s") 'phi-search)
(spacemacs/declare-prefix "c" "my-git-prefix")
(spacemacs/set-leader-keys "sj" 'imenu-anywhere)
(spacemacs/set-leader-keys "ss" 'counsel-imenu)
(spacemacs/set-leader-keys "gff" 'magit-file-popup)
(spacemacs/set-leader-keys "gp" 'magit-push-popup)
(spacemacs/set-leader-keys "gF" 'magit-fetch-popup)
(spacemacs/set-leader-keys "gfe" 'magit-ediff-popup)
(spacemacs/set-leader-keys "gc" 'magit-commit-popup)
(spacemacs/set-leader-keys "thV" 'js2-highlight-vars-mode)
(spacemacs/set-leader-keys "wz"  'spacemacs/toggle-maximize-buffer)
(define-key evil-normal-state-map (kbd "v") 'er/expand-region)
(define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)
(global-set-key (kbd "S-s-<mouse-1>") 'mc/add-cursor-on-click)
(evil-global-set-key 'normal (kbd "C-w z") 'spacemacs/toggle-maximize-buffer)
(define-key evil-normal-state-map (kbd "u") 'undo-tree-undo)
(define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo)
(define-key evil-normal-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
(define-key evil-normal-state-map (kbd "zz") 'evil-toggle-fold)
(define-key evil-normal-state-map (kbd "za") 'evil-scroll-line-to-center)
(define-key evil-normal-state-map (kbd "ga") 'evil-cp-insert-at-end-of-form)
(define-key evil-normal-state-map (kbd "gi") 'evil-cp-insert-at-beginning-of-form)
(define-key evil-normal-state-map (kbd "gI") 'evil-insert-resume)
(define-key evil-insert-state-map (kbd "C-v") 'forward-word)
(define-key evil-insert-state-map (kbd "C-k") 'backward-word)
(define-key evil-insert-state-map (kbd "C-d") 'delete-forward-char)
(define-key evil-insert-state-map (kbd "C-n") 'mc/mark-next-like-this-word)
(define-key evil-insert-state-map (kbd "C-p") 'mc/mark-previous-like-this-word)
(define-key evil-insert-state-map (kbd "M-n") 'mc/mark-next-like-this)
(define-key evil-insert-state-map (kbd "M-p") 'mc/mark-previous-like-this)
(define-key evil-insert-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
(define-key evil-insert-state-map (kbd "C-a") 'mwim-beginning-of-code-or-line)
(define-key evil-visual-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
(define-key evil-visual-state-map (kbd "C-a") 'mwim-beginning-of-code-or-line)
(define-key evil-insert-state-map (kbd "C-l") 'evil-complete-next)
(define-key evil-insert-state-map (kbd "C-S-n") 'mc/skip-to-next-like-this)
(define-key evil-insert-state-map (kbd "C-S-p") 'mc/skip-to-previous-like-this)
(define-key evil-insert-state-map [(control return)] 'mc/mark-all-dwim)
(global-set-key (kbd "C-SPC") 'swiper)
(global-set-key (kbd "^@") 'swiper)

;;;;;C-h
(keyboard-translate ?\C-h ?\C-?)
(global-set-key [(control ?h)] 'delete-backward-char)

(define-key evil-normal-state-map "zl" 'hs-hide-level)
(define-key evil-insert-state-map (kbd "C-j") 'evil-ret-and-indent)
(define-key evil-visual-state-map (kbd "g<") 'mc/edit-beginnings-of-lines)
(define-key evil-visual-state-map (kbd "g>") 'mc/edit-ends-of-lines)
