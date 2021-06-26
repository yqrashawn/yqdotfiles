;;; keybindings.el -*- lexical-binding: t; -*-

(define-prefix-command '+windmove-map)
(define-prefix-command '+cmd-prefix-map)
(define-prefix-command '+ctl-prefix-map)
(define-prefix-command 'ctl-x-9-map)
(define-prefix-command 'yq-s-map)

(map!
 :g "C-c" nil
 :g "C-c C-c" #'eval-defun
 :g "C-s" #'swiper-isearch
 :g "s-k" #'kill-this-buffer
 :g "s-u" #'revert-buffer
 :g "C-M-s-7" '+windmove-map
 :g "C-a" #'mwim-beginning-of-code-or-line
 :g "C-e" #'mwim-end-of-code-or-line
 [remap split-window-below] #'evil-window-split
 [remap split-window-right] #'evil-window-vsplit
 (:leader
  "TAB" (cmd! (switch-to-nth-buffer 1))
  "fe" nil
  "sf" #'+default/search-cwd
  "SPC" #'counsel-M-x
  "fww" (cmd! (counsel-fzf "" "~/workspace"))
  "fwo" (cmd! (counsel-fzf "" "~/workspace/office"))
  "fwh" (cmd! (counsel-fzf "" "~/workspace/home"))
  "fwt" (cmd! (counsel-fzf "" "~/workspace/third"))
  "fes" (cmd! (find-file-existing "~/.ssh/config") (call-interactively 'swiper))
  "fed" (cmd! (find-file-existing "~/.doom.d/config.el"))
  "fek" (cmd! (find-file-existing "~/.config/karabiner.edn"))
  "feS" (cmd! (find-file-existing "~/Dropbox/应用/Surge Profiles/D.conf"))
  "fem" (cmd! (let ((default-directory "~/.emacs.d/")) (call-interactively #'+default/search-project)))
  "fel" #'counsel-find-library
  "fJ" #'yq/open-junk-file
  "bb" #'yq/open-with-alfred
  "bs" #'doom/switch-to-scratch-buffer
  "bm" (cmd! (+popup-buffer (get-buffer "*Messages*")))
  "hK" #'describe-keymap
  "kgh" #'git-link-homepage
  "kgc" #'git-link-commit
  "kgg" #'git-link
  "gs" #'magit-status
  "gg" #'git-gutter:stage-hunk)
 (:localleader
  (:after js2-mode
   (:map js2-mode-map
    :n "jj" #'+jest-popup
    :n "jd" #'+jest-popup-debug)))
 (:map ctl-x-map
  "@ @" '+cmd-prefix-map
  "@ C-x" '+ctl-prefix-map
  "C-n" #'narrow-or-widen-dwim
  "9" 'ctl-x-9-map)
 (:map ctl-x-9-map
  "j" #'iflipb-next-buffer
  "k" #'iflipb-previous-buffer)
 (:map +cmd-prefix-map
  :g "k" (general-simulate-key "s-k")
  :g "u" (general-simulate-key "s-u"))
 (:map +ctl-prefix-map
  :g "'" (general-simulate-key "C-'")
  :g "." (general-simulate-key "C-.")
  :g "," (general-simulate-key "C-,"))
 (:map +windmove-map
  :g "h" #'windmove-left
  :g "j" #'windmove-down
  :g "k" #'windmove-up
  :g "l" #'windmove-right)
 (:map yq-s-map
  :g "j" #'counsel-recentf
  :g "h" #'save-buffer
  :g "d" #'sp-kill-sexp
  :g "v" #'er/expand-region
  :g "c" #'delete-window
  :g "k" #'kill-current-buffer
  :g "m" #'+ivy/projectile-find-file
  :g "l" #'counsel-imenu
  :g "n" #'evil-avy-goto-char-2
  :g "f" #'+default/search-project
  :g "F" #'+default/search-project-for-symbol-at-point
  :g "o" #'dired-jump
  :g "b" #'ivy-switch-buffer)
 (:after evil-snipe
  (:map evil-snipe-local-mode-map
   :n "s" nil
   :v "s" nil
   :m "s" nil))
 (:after evil-iedit
  (:map evil-iedit-state-map
   "V" nil))
 (:after evil-multiedit
  (:map evil-multiedit-state-map
   :g "C-n" #'evil-multiedit-match-and-next
   :g "C-p" #'evil-multiedit-match-and-prev))
 (:after company
  (:map company-active-map
   "C-l" #'company-complete-selection
   "C-m" nil))
 (:after dired
  (:map dired-mode-map
   :n "l" #'dired-find-file
   :n "h" #'dired-up-directory))
 :nvm "C-e" nil
 :i "C-d" nil
 :i "C-n" nil
 :i "C-p" nil
 :i "C-l" #'hippie-expand
 :n "C-k" #'evil-toggle-fold
 :n "gy" #'yq/duplicate-line
 :n "," (general-simulate-key "SPC m")
 :n "gn" #'evil-search-word-forward
 :n "zl" #'hs-hide-level
 :n "s" 'yq-s-map
 :nv "C-m" #'evil-jump-item
 :n "C-n" #'evil-multiedit-match-and-next
 :n "C-p" #'evil-multiedit-match-and-prev
 :v "s" #'evil-surround-region
 :v "C-e" #'evil-end-of-line-or-visual-line
 :v "C-a" #'evil-beginning-of-visual-line

 :textobj "B" #'+evil:defun-txtobj                #'+evil:defun-txtobj
 :textobj "f" #'evil-textobj-anyblock-inner-block #'evil-textobj-anyblock-a-block)