;; -*- mode: emacs-lisp -*-
(defun hide-emacs ()
  (interactive)
  (call-process "osascript" nil nil nil "-e" "tell application \"Finder\"" "-e" "set visible of process \"Emacs\" to false" "-e" "end tell"))
;; (define-key ivy-mode-map (kbd "C-s-9") 'minibuffer-keyboard-quit)
(global-set-key (kbd "s-d") 'dired)
(global-set-key (kbd "s-l") 'spacemacs/workspaces-transient-state/body)
(global-set-key (kbd "s-k") 'spacemacs/kill-this-buffer)
(global-set-key (kbd "s-j") 'ivy-switch-buffer)
(global-set-key (kbd "s-b") 'bookmark-jump)
(global-set-key (kbd "s-m") 'magit-dispatch-popup)
(global-set-key (kbd "C-s") 'counsel-grep-or-swiper)
(global-set-key (kbd "C-;") 'spacemacs/default-pop-shell)
(spacemacs/set-leader-keys "fd" 'diredp-dired-recent-dirs)
(push 'evil-escape-mode evil-mc-incompatible-minor-modes)
(setq evil-mc-undo-cursors-on-keyboard-quit t)

;; dired
(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map "l" 'diredp-find-file-reuse-dir-buffer)
  (evil-define-key 'normal dired-mode-map "f" 'dired-narrow-fuzzy)
  (evil-define-key 'normal dired-mode-map "gk" 'dired-k)
  (evil-define-key 'normal dired-mode-map "e" 'ora-ediff-files)
  ;; (evil-define-key 'normal dired-mode-map (kbd "RET") '(shell-command (concat "open " (shell-quote-argument filename))))
  (evil-define-key 'normal dired-mode-map "h" 'diredp-up-directory-reuse-dir-buffer))

(use-package multiple-cursors
  :ensure t
  :bind (("M-." . mc/mark-next-like-this)
         ("M-," . mc/unmark-next-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

;; spacemacs leader
(spacemacs/set-leader-keys "sj" 'imenu-anywhere)
(spacemacs/set-leader-keys "ss" 'counsel-imenu)
(spacemacs/set-leader-keys "sn" 'spacemacs/swiper-region-or-symbol)
(spacemacs/set-leader-keys "gf" 'magit-file-popup)
(spacemacs/set-leader-keys "fp" 'counsel-git)
(spacemacs/set-leader-keys "gn" 'magit-gitflow-popup)
(spacemacs/set-leader-keys "wz" 'spacemacs/toggle-maximize-buffer)
(spacemacs/set-leader-keys "t0" 'centered-cursor-mode)
(spacemacs/set-leader-keys "hc" 'hide/show-comments-toggle)
(spacemacs/set-leader-keys "fl" 'counsel-locate)
(spacemacs/set-leader-keys "fL" 'find-file-literally)

(spacemacs/declare-prefix "o" "own-prefix")
(spacemacs/declare-prefix "oe" "edit-prefix")

;; global
(global-set-key (kbd "C-SPC") 'counsel-grep-or-swiper)
(global-set-key (kbd "^@") 'counsel-grep-or-swiper)
;; (global-set-key (kbd "C-SPC") 'evil-search-forward)
;; (global-set-key (kbd "^@") 'evil-search-forward)

(global-set-key (kbd "s-/") 'evilnc-comment-or-uncomment-lines)
(global-set-key (kbd "s-K") 'spacemacs/kill-other-buffers)
(global-set-key (kbd "S-s-<mouse-1>") 'mc/add-cursor-on-click)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-r") 'counsel-recentf)
(global-set-key (kbd "C-x C-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-y") 'counsel-yank-pop)
(global-set-key (kbd "C-x C-j") 'dired-jump)


;; evil global
(evil-global-set-key 'normal (kbd "C-w z") 'spacemacs/toggle-maximize-buffer)

;; (define-key evil-normal-state-map (kbd "C-s-9") 'ivy-switch-buffer)
;; (define-key evil-insert-state-map (kbd "C-s-9") 'ivy-switch-buffer)
;; evil normal
(define-key evil-normal-state-map (kbd "C-'") 'evil-avy-goto-word-1)
(define-key evil-normal-state-map (kbd "C-u") 'golden-ratio-scroll-screen-down)
(define-key evil-normal-state-map (kbd "C-d") 'golden-ratio-scroll-screen-up)
(define-key evil-normal-state-map "zl" 'hs-hide-level)
(define-key evil-normal-state-map "gn" 'evil-goto-definition)
(define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)
(define-key evil-normal-state-map (kbd "C-f") 'counsel-find-file)
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "gy") 'duplicate-line)
(define-key evil-normal-state-map (kbd "gl") 'dumb-jump-go)
(define-key evil-normal-state-map (kbd "gY") 'spacemacs/copy-and-comment-lines)
(define-key evil-normal-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
(define-key evil-normal-state-map (kbd "zz") 'evil-toggle-fold)
(define-key evil-normal-state-map (kbd "C-k") 'evil-toggle-fold)
(define-key evil-normal-state-map (kbd "za") 'evil-scroll-line-to-center)
(define-key evil-normal-state-map (kbd "C-m") 'evil-jump-item)
(define-key evil-normal-state-map (kbd "M-j") 'evil-mc-make-cursor-move-next-line)
(define-key evil-normal-state-map (kbd "M-k") 'evil-mc-make-cursor-move-prev-line)
(define-key evil-normal-state-map (kbd "C-h") 'dumb-jump-quick-look)

(defun evil-search-next-recenter ()
  "evil search next and recenter"
  (interactive)
  (evil-search-next)
  (recenter))
(defun evil-search-previous-recenter ()
  "evil search next and recenter"
  (interactive)
  (evil-search-previous)
  (recenter))
(define-key evil-normal-state-map (kbd "n") 'evil-search-next-recenter)
(define-key evil-normal-state-map (kbd "N") 'evil-search-previous-recenter)

(defun evil-jump-backward-recenter ()
  "evil search next and recenter"
  (interactive)
  (evil-jump-backward)
  (recenter))
(defun evil-jump-forward-recenter ()
  "evil search next and recenter"
  (interactive)
  (evil-jump-forward)
  (recenter))
(define-key evil-motion-state-map (kbd "C-o") 'evil-jump-backward-recenter)
(define-key evil-motion-state-map (kbd "C-i") 'evil-jump-forward-recenter)

;; evil visual
(define-key evil-visual-state-map (kbd "C-a") 'evil-first-non-blank)
(define-key evil-visual-state-map (kbd "C-e") 'evil-end-of-line)
(define-key evil-visual-state-map (kbd "C-m") 'evil-jump-item)
(define-key evil-visual-state-map (kbd "C-x C-;") 'evilnc-comment-or-uncomment-lines)
(define-key evil-visual-state-map "gE" 'mc-edit-lines)

;; evil insert
(define-key evil-insert-state-map (kbd "C-j") 'evil-ret-and-indent)
(define-key evil-insert-state-map (kbd "C-k") 'paredit-kill)
(define-key evil-insert-state-map (kbd "C-d") 'delete-forward-char)
;; (define-key evil-insert-state-map (kbd "M-n") 'mc/mark-next-like-this)
;; (define-key evil-insert-state-map (kbd "M-p") 'mc/mark-previous-like-this)
(define-key evil-insert-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
(define-key evil-insert-state-map (kbd "C-a") 'mwim-beginning-of-code-or-line)
(define-key evil-insert-state-map (kbd "C-l") 'evil-complete-next)
(define-key evil-insert-state-map (kbd "C-S-n") 'mc/skip-to-next-like-this)
(define-key evil-insert-state-map (kbd "C-S-p") 'mc/skip-to-previous-like-this)

;; textobj
(define-key evil-inner-text-objects-map "f" 'evil-textobj-anyblock-inner-block)
(define-key evil-outer-text-objects-map "f" 'evil-textobj-anyblock-a-block)
(define-key evil-inner-text-objects-map "c" 'evil-textobj-column-word)
(define-key evil-inner-text-objects-map "C" 'evil-textobj-column-WORD)
(add-hook 'lisp-mode-hook
          (lambda ()
            (setq-local evil-textobj-anyblock-blocks
                        '(("(" . ")")
                          ("{" . "}")
                          ("\\[" . "\\]")
                          ("\"" . "\"")))))

;; (define-key helm-find-files-map (kbd "C-h") 'helm-find-files-up-one-level)
;;;;;C-h
(keyboard-translate ?\C-h ?\C-?)
(global-set-key [(control ?h)] 'delete-backward-char)

;; ivy-minibuffer-map
(define-key ivy-minibuffer-map (kbd "C-n") 'ivy-next-history-element)
(define-key ivy-minibuffer-map (kbd "C-p") 'ivy-previous-history-element)

;; remap s
(define-key evil-normal-state-map "s" nil)
(define-key evil-normal-state-map "sg" 'magit-dispatch-popup)
(define-key evil-normal-state-map "sf" 'spacemacs/search-auto)
(define-key evil-normal-state-map "sF" 'spacemacs/search-auto-region-or-symbol)
(define-key evil-normal-state-map "sk" 'phi-search)
(define-key evil-normal-state-map "sK" 'spacemacs/swiper-region-or-symbol)
(define-key evil-visual-state-map "sa" 'avy-goto-word-or-subword-1)
(define-key evil-normal-state-map "sh" 'save-buffer)
(define-key evil-normal-state-map "sl" 'counsel-imenu)
(define-key evil-normal-state-map "sL" 'imenu-anywhere)
(define-key evil-normal-state-map "sb" 'ace-jump-code-buffers)
(define-key evil-normal-state-map "sj" 'counsel-recentf)
(define-key evil-normal-state-map "sv" 'er/expand-region)
(define-key evil-normal-state-map "sQ" 'aya-create)
(define-key evil-normal-state-map "sq" 'aya-expand)

;; helm
;; (define-key helm-map (kbd "C-w") 'evil-delete-backward-word)
;; (define-key helm-map (kbd "C-n") 'next-history-element)
;; (define-key helm-map (kbd "C-p") 'previous-history-element)
;; (define-key helm-find-files-map (kbd "C-w") 'helm-find-files-up-one-level)
;; (define-key helm-read-file-map (kbd "C-w") 'helm-find-files-up-one-level)

(use-package webpaste
  :ensure t
  :bind (("C-c C-p C-b" . webpaste-paste-buffer)
         ("C-c C-p C-r" . webpaste-paste-region)))

(defun dired-do-shell-mac-open-vqn ()
  (interactive)
  (save-window-excursion
    (dired-do-async-shell-command
     "open" current-prefix-arg
     (dired-get-marked-files t current-prefix-arg))))

;; Term mode map
(add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "RET") 'dired-do-shell-mac-open-vqn)))
(with-eval-after-load 'term
  (evil-define-key 'insert term-raw-map (kbd "C-d") 'term-send-eof)
  (evil-define-key 'insert term-raw-map (kbd "C-c") 'term-send-raw)
  (evil-define-key 'insert term-raw-map (kbd "C-m") 'term-send-input)
  (evil-define-key 'insert term-raw-map (kbd "C-z") 'term-stop-subjob)
  (evil-define-key 'insert term-raw-map (kbd "C-a") 'term-send-home)
  (evil-define-key 'insert term-raw-map (kbd "<tab>") 'term-send-tab)
  (evil-define-key 'insert term-raw-map (kbd "RET") 'term-send-input)
  (evil-define-key 'insert term-raw-map (kbd "C-u") 'term-kill-input)
  (evil-define-key 'insert term-raw-map (kbd "C-w") 'backward-kill-word)
  (evil-define-key 'insert term-raw-map (kbd "C-c") 'term-interrupt-subjob)
  (evil-define-key 'insert term-raw-map (kbd "C-z") 'term-stop-subjob)
  (evil-define-key 'insert term-raw-map (kbd "C-\\") 'term-quit-subjob)
  (evil-define-key 'insert term-raw-map (kbd "C-o") 'evil-window-next)
  (evil-define-key 'insert term-raw-map (kbd "C-r") 'term-show-output)
  (evil-define-key 'insert term-raw-map (kbd "C-l") 'term-dynamic-list-input-ring)
  (evil-define-key 'insert term-raw-map (kbd "C-n") 'term-send-down)
  (evil-define-key 'insert term-raw-map (kbd "C-p") 'term-send-up)
  (evil-define-key 'insert term-raw-map (kbd "C-d") 'term-send-eof)
  (evil-define-key 'insert term-raw-map (kbd "C-q") 'term-pager-toggle)
  (evil-define-key 'insert term-raw-map (kbd "C-e") 'term-send-end))

(defun switch-to-nth-buffer (n)
  "Switches to nth most recent buffer. Ignores a bunch of stuff."
  (catch 'tag
    (mapcar (lambda (b)
              (unless
                  (or
                   (minibufferp b)
                   (string-match "^ " (buffer-name b))
                   (equal b (current-buffer)))
                (if (= n 1)
                    (progn
                      (switch-to-buffer b)
                      (throw 'tag nil))
                  (setq n (- n 1)))))
            (buffer-list))))

(defun switch-to-most-recent-buffer ()
  (interactive)
  (switch-to-nth-buffer 1))
(defun switch-to-second-most-recent-buffer ()
  (interactive)
  (switch-to-nth-buffer 2))
(defun switch-to-third-most-recent-buffer ()
  (interactive)
  (switch-to-nth-buffer 3))

;;fast switching between two buffers
(define-key evil-normal-state-map (kbd "<tab>") 'switch-to-most-recent-buffer)

;;fast switching between three buffers
(define-key evil-normal-state-map (kbd "<C-tab>") 'switch-to-second-most-recent-buffer)
(define-key evil-normal-state-map (kbd "<C-s-tab>") 'switch-to-third-most-recent-buffer)
