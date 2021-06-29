;;; keybindings.el -*- lexical-binding: t; -*-

(define-prefix-command '+windmove-map)
(define-prefix-command '+cmd-prefix-map)
(define-prefix-command '+ctl-prefix-map)
(define-prefix-command 'ctl-x-9-map)
(define-prefix-command 'yq-s-map)
(define-prefix-command 'ctl-x-c-8-map)

(map!
 :g "C-c" nil
 :g "C-c C-c" #'eval-defun
 :g "C-c s" #'swiper-isearch-thing-at-point
 :g "C-c U" #'counsel-unicode-char
 :g "C-y" #'counsel-yank-pop
 :g "C-c <xterm-paste>" #'+default/yank-pop
 :g "C-s" #'swiper-isearch
 :g "C-SPC" #'counsel-grep-or-swiper
 :g " " #'counsel-grep-or-swiper
 :g "s-k" #'kill-this-buffer
 :g "s-u" #'revert-buffer
 :g "s-i" #'side-notes-toggle-notes
 :g "C-M-s-7" '+windmove-map
 :g "C-a" #'mwim-beginning-of-code-or-line
 :g "C-e" #'mwim-end-of-code-or-line
 [remap split-window-below] #'evil-window-split
 [remap split-window-right] #'evil-window-vsplit
 (:leader
  :desc "M-x" "SPC" #'execute-extended-command
  :desc "Git commit link" "kgc" #'git-link-commit
  :desc "Git homepage link" "kgh" #'git-link-homepage
  :desc "Git link" "kgl" #'git-link
  :desc "Open scratch buffer" "bs" #'doom/switch-to-scratch-buffer
  :desc "Open scratch project buffer" "bS" #'doom/switch-to-project-scratch-buffer
  :desc "Pop up message buffer" "bm" (cmd! (+popup-buffer (get-buffer "*Messages*")))
  :desc "Find file" "ff" #'find-file
  :desc "Find file" "fj" #'dired-jump
  :desc "Open junk file" "fJ" #'yq/open-junk-file
  :desc "Indent" "j=" #'yq/indent-region-or-buffer
  :desc "Tab buffer" "TAB" (cmd! () (switch-to-nth-buffer 1))
  :desc "Tramp" "fT" #'counsel-tramp
  :desc "Git stage hunk" "gg" #'git-gutter:stage-hunk
  :desc "Magit status" "gs" #'magit-status
  :desc "Magit status here" "gS" #'magit-status-here
  :desc "Search other directory" "sf" #'+default/search-other-cwd
  :desc "Locate file" "sD" #'locate
  (:prefix-map ("fw" . "Workspace")
   :desc "Find file in workspace" "w" (cmd! (counsel-fzf "" "~/workspace"))
   :desc "Home" "h" (cmd! (counsel-fzf "" "~/workspace/home"))
   :desc "Office" "o" (cmd! (counsel-fzf "" "~/workspace/office"))
   :desc "Third" "t" (cmd! (counsel-fzf "" "~/workspace/third")))
  (:prefix-map ("r" . "Misc")
   :desc "Resume ivy" "l" #'ivy-resume)
  (:prefix-map ("fe" . "Edit srcs")
   :desc "Find library" "l" #'find-library
   :desc "Edit .doom.d config" "d" (cmd! (find-file-existing "~/.doom.d/config.el"))
   :desc "Search in ~/.emacs.d" "s" (cmd! (find-file-existing "~/.ssh/config") (call-interactively 'swiper))
   :desc "Search in ~/.emacs.d" "m" (cmd! (let ((default-directory "~/.emacs.d/")) (call-interactively #'+default/search-project)))
   :desc "Edit goku edn config" "k" (cmd! (find-file-existing "~/.config/karabiner.edn"))
   :desc "Edit surge config" "S" (cmd! (find-file-existing "~/Dropbox/应用/Surge Profiles/D.conf"))
   :desc "Find library" "l" #'find-library))
 (:localleader
  (:after js2-mode
   (:map js2-mode-map
    :n "jj" #'+jest-popup
    :n "jd" #'+jest-popup-debug)))
 (:map help-map
  "K" #'describe-keymap)
 (:map ctl-x-map
  :g "C-r" #'counsel-recentf
  :g "1" #'spacemacs/toggle-maximize-buffer
  "@ @" '+cmd-prefix-map
  "@ C-x" '+ctl-prefix-map
  "C-n" #'narrow-or-widen-dwim
  "C-8" 'ctl-x-c-8-map
  "9" 'ctl-x-9-map)
 (:map 'ctl-x-c-8-map
  :g "[" #'goto-last-change
  :g "" #'goto-last-change-reverse)
 (:map ctl-x-9-map
  :g "j" #'iflipb-next-buffer
  :g "k" #'iflipb-previous-buffer)
 (:map +cmd-prefix-map
  :g "k" (general-simulate-key "s-k")
  :g "u" (general-simulate-key "s-u")
  :g "i" (general-simulate-key "s-i"))
 (:map +ctl-prefix-map
  :g "i" (general-simulate-key "C-i")
  :g "'" (general-simulate-key "C-'")
  :g ";" (general-simulate-key "C-;")
  :g "-" (general-simulate-key "C--")
  :g "=" (general-simulate-key "C-=")
  :g "_" (general-simulate-key "C-_")
  :g "+" (general-simulate-key "C-+")
  :g "'" (general-simulate-key "C-'")
  :g "." (general-simulate-key "C-.")
  :g "," (general-simulate-key "C-,"))
 (:map +windmove-map
  :g "h" #'windmove-left
  :g "j" #'windmove-down
  :g "k" #'windmove-up
  :g "l" #'windmove-right)
 (:map yq-s-map
  :g "u" #'magit-dispatch
  :g "j" #'counsel-recentf
  :g "h" #'save-buffer
  :g "d" #'sp-kill-sexp
  :g "," #'sp-copy-sexp
  :g "y" #'bjm/ivy-dired-recent-dirs
  :g "v" #'er/expand-region
  :g "c" #'delete-window
  :g "K" #'project-kill-buffers
  :g "a" #'loccur-current
  :g "n" #'evil-avy-goto-char-2
  :g "/" #'evil-avy-goto-char-timer
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
   :g "C-p" #'evil-multiedit-match-and-prev
   :g "n" #'evil-multiedit-next
   :g "p" #'evil-multiedit-prev))
 (:after ivy
  (:map ivy-minibuffer-map
   "C-n" #'ivy-next-history-element
   "C-p" #'ivy-previous-history-element)
  (:map ivy-switch-buffer-map
   "C-d" #'ivy-switch-buffer-kill))
 (:after company
  (:map company-active-map
   "C-l" #'company-complete-selection
   "C-m" nil))
 (:after dired
  (:map dired-mode-map
   :n "l" #'dired-find-file
   :n "h" #'dired-up-directory
   :n "F" #'fd-dired
   :n "gb" #'yq/open-with-alfred))
 (:after double-saber
  (:map double-saber-mode-map
   :n "x" #'double-saber-narrow
   :n "d" #'double-saber-delete
   :n "u" #'double-saber-undo
   :n "C-r" #'double-saber-redo
   :n "S" #'double-saber-sort-lines))
 (:after outline
  (:map outline-minor-mode-map
   :g "TAB" #'bicycle-cycle
   :g "C-i" #'bicycle-cycle))
 :nvm "C-e" nil
 :i "C-d" nil
 :i "C-n" nil
 :i "C-p" nil
 :i "C-l" #'hippie-expand
 :n "j" #'evil-next-visual-line
 :n "k" #'evil-previous-visual-line
 :n "gj" #'evil-next-line
 :n "gk" #'evil-previous-line
 :n "C-k" #'evil-toggle-fold
 :n "gy" #'yq/duplicate-line
 :n "gY" #'evilnc-copy-and-comment-lines
 :n "," (general-simulate-key "SPC m")
 :n "gn" #'evil-search-word-forward
 :n "zl" #'hs-hide-level
 :n "s" 'yq-s-map
 :n ">" #'evil-shift-right-line
 :n "<" #'evil-shift-left-line
 :n "gm" #'evil-snipe-s
 :nv "C-m" #'evil-jump-item
 :n "C-n" #'evil-multiedit-match-and-next
 :n "C-p" #'evil-multiedit-match-and-prev
 :v ">" (cmd! (call-interactively 'evil-shift-right) (execute-kbd-macro "gv"))
 :v "<" (cmd! (call-interactively 'evil-shift-left) (execute-kbd-macro "gv"))
 :v "v" #'evil-multiedit-match-all
 :v "s" #'evil-surround-region
 :v "C-e" #'evil-end-of-line-or-visual-line
 :v "C-a" #'evil-beginning-of-visual-line

 :textobj "B" #'+evil:defun-txtobj #'+evil:defun-txtobj
 :textobj "f" #'evil-textobj-anyblock-inner-block #'evil-textobj-anyblock-a-block)