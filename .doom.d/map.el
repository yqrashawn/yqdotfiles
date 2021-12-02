;;; map.el -*- lexical-binding: t; -*-

(define-prefix-command '+windmove-map)
(define-prefix-command '+cmd-prefix-map)
(define-prefix-command '+ctl-prefix-map)
(define-prefix-command '+ctl-p-prefix-map)
(define-prefix-command 'ctl-x-9-map)
(define-prefix-command 'yq-s-map)
(define-prefix-command 'ctl-x-at-8-map)
(define-prefix-command 'ctl-x-at-6-map)
(define-prefix-command '+thing-edit-map)

(global-set-key (kbd "<mouse-4>") (kbd "<wheel-up>"))
(global-set-key (kbd "<mouse-5>") (kbd "<wheel-down>"))

(setq doom-localleader-key ","
      doom-localleader-alt-key "M-SPC ,")

(map!
 :g "C-c" nil
 :g "C-c C-c" #'eval-defun
 :g "C-c '" #'separedit
 :g "C-c s" #'swiper-isearch-thing-at-point
 :g "C-c U" #'counsel-unicode-char
 :g "C-y" #'yank
 :g "C-c <xterm-paste>" #'+default/yank-pop
 ;; :g "C-s" #'consult-line
 :g "C-s" #'swiper-isearch
 ;; :g "C-SPC" #'consult-line
 ;; :g "C-@" #'consult-line
 :g "C-SPC" #'counsel-grep-or-swiper
 :g "C-@" #'counsel-grep-or-swiper
 :g "s-k" #'bury-buffer
 :g "s-m" #'+popup/toggle
 :g "s-u" #'revert-buffer
 :g "s-i" #'side-notes-toggle-notes
 :g "C-M-s-7" '+windmove-map
 :g "C-M-s-j" #'iflipb-next-buffer
 :g "C-M-s-k" #'iflipb-previous-buffer
 :g "C-a" #'mwim-beginning-of-code-or-line
 :g "C-e" #'mwim-end-of-code-or-line
 :g "C-'" #'yq/vterm-toggle
 [remap split-window-below] #'evil-window-split
 [remap split-window-right] #'evil-window-vsplit
 [remap xterm-paste] #'yank
 [remap eval-last-sexp] #'pp-eval-last-sexp
 [eval-expression] #'pp-eval-expression
 (:leader
  (:prefix-map ("1" . "DO")
   :desc "Daily Capture" "c" #'org-roam-dailies-capture-today
   :desc "Daily Note" "1" #'org-roam-dailies-goto-today
   :desc "What To Do" "SPC" #'org-agenda
   :desc "New Node" "n" #'org-roam-capture
   :desc "Find Node" "f" #'org-roam-node-find
   :desc "Agenda" "a" (lambda (arg) (interactive "P") (org-agenda arg "a")))
  :desc "Local Leader" "m" (general-simulate-key ",")
  :desc "M-x" "SPC" #'execute-extended-command
  :desc "Reveal in finder" "bf" #'reveal-in-osx-finder
  :desc "Open scratch buffer" "bs" #'doom/switch-to-scratch-buffer
  :desc "Open scratch project buffer" "bS" #'doom/switch-to-project-scratch-buffer
  :desc "Toggle popup" "bm" #'++popup-messages
  :desc "Find file" "ff" #'find-file
  :desc "Find file" "fj" #'dired-jump
  :desc "Open junk file" "fJ" #'yq/open-junk-file
  :desc "Indent" "j=" #'yq/indent-region-or-buffer
  :desc "Tab buffer" "TAB" (cmd! () (switch-to-nth-buffer 1))
  :desc "Tramp" "fT" #'counsel-tramp
  :desc "Git stage hunk" "gg" #'git-gutter:stage-hunk
  :desc "Magit status" "gs" #'magit-status
  :desc "Magit status here" "gS" #'magit-status-here
  :desc "Smerge mode" "gd" #'hydra-smerge/body
  :desc "Search other directory" "sf" #'+default/search-other-cwd
  :desc "Locate file" "sD" #'locate
  :desc "Toggle debug on error" "tD" #'toggle-debug-on-error
  :desc "Toggle Tabnine" "tt" #'yq/toggle-company-tabnine
  :desc "Line numbers" "tl" #'doom/toggle-line-numbers
  :desc "Imenu sidebar" "cb" #'side-hustle-toggle
  (:prefix-map ("e" . "Edit")
   :desc "String" "s" #'string-edit-at-point
   :desc "Indirect" "I" #'edit-indirect-region)
  (:prefix-map ("k" . "Kill")
   :desc "Browse at remote" "k" #'browse-at-remote
   (:prefix-map ("g" . "git link")
    :desc "Git commit link" "c" #'git-link-commit
    :desc "Git homepage link" "h" #'git-link-homepage
    :desc "Git link" "g" #'git-link)
   (:prefix-map ("a" . "copy as format")
    :desc "Github" "g" #'copy-as-format-github
    :desc "Slack" "s" #'copy-as-format-slack
    :desc "Org" "o" #'copy-as-format-org-mode
    :desc "Markdown" "m" #'copy-as-format-markdown))
  (:prefix-map ("fw" . "Workspace")
   :desc "Find file in workspace" "w" (cmd! (let ((default-directory "~/workspace/")) (call-interactively #'find-file)))
   :desc "Home" "h" (cmd! (let ((default-directory "~/workspace/home/")) (call-interactively #'find-file)))
   :desc "Office" "o" (cmd! (let ((default-directory "~/workspace/office/")) (call-interactively #'find-file)))
   :desc "Third" "t" (cmd! (let ((default-directory "~/workspace/third/")) (call-interactively #'find-file))))
  (:prefix-map ("r" . "Misc")
   :desc "Resume ivy" "l" #'ivy-resume)
  (:prefix-map ("fe" . "Edit srcs")
   :desc "Find library" "l" #'find-library
   :desc "Edit .doom.d config" "d" (cmd! (find-file-existing "~/.doom.d/config.el"))
   :desc "Search in ~/.emacs.d" "s" (cmd! (find-file-existing "~/.ssh/config.gpg") (call-interactively #'swiper))
   :desc "Search in ~/.emacs.d" "m" (cmd! (let ((default-directory "~/.emacs.d/")) (call-interactively #'+default/search-project)))
   :desc "Edit goku edn config" "k" (cmd! (find-file-existing "~/.config/karabiner.edn"))
   :desc "Edit hammerspoon config" "h" (cmd! (find-file-existing "~/.spacehammer/config.fnl"))
   :desc "Edit surge config" "S" (cmd! (find-file-existing "~/Dropbox/sync/surge/D.conf"))
   :desc "Find library" "l" #'find-library))
 (:localleader
  (:after js2-mode
   (:map js2-mode-map
    :n "jj" #'+jest-popup
    :n "jd" #'+jest-popup-debug
    :n "rll" #'turbo-log-print))
  (:after org
   (:map org-mode-map
    :n "i" nil
    :n "it" #'org-toggle-item
    :n "iii" #'+org/insert-item-below
    :n "iik" #'+org/insert-item-above
    :n "ihh" #'org-insert-heading
    :n "ihj" #'org-insert-heading-after-current
    :n "iht" (cmd! (org-insert-heading) (insert (format-time-string "%T")))
    :n "ds" #'orgbox-schedule)))
 (:map help-map
  "K" #'describe-keymap)
 (:map ctl-x-map
  :g "C-r" #'recentf-open-files
  :g "1" #'spacemacs/toggle-maximize-buffer
  :g "C-&" (lambda (&optional arg)
             (interactive "P")
             (if (and (region-active-p) (< (region-beginning) (region-end)))
                 (save-restriction
                   (narrow-to-region (region-beginning) (region-end))
                   (fit-text-scale-max-font-size-fit-buffer))
               (fit-text-scale-max-font-size-fit-buffer)))
  :g "C-*" (lambda (&optional arg)
             (interactive "P")
             (if (and (region-active-p) (< (region-beginning) (region-end)))
                 (save-restriction
                   (narrow-to-region (region-beginning) (region-end))
                   (fit-text-scale-max-font-size-fit-buffer))
               (fit-text-scale-max-font-size-fit-buffer)))
  "@ @" '+cmd-prefix-map
  "p" '+ctl-p-prefix-map
  "@ C-x" '+ctl-prefix-map
  "C-n" #'narrow-or-widen-dwim
  "@ 6" 'ctl-x-at-6-map
  "@ 8" 'ctl-x-at-8-map
  "9" 'ctl-x-9-map)
 (:map ctl-x-at-6-map
  :g "1" (general-simulate-key "M-1")
  :g "2" (general-simulate-key "M-2")
  :g "3" (general-simulate-key "M-3")
  :g "4" (general-simulate-key "M-4")
  :g "5" (general-simulate-key "M-5")
  :g "6" (general-simulate-key "M-6")
  :g "7" (general-simulate-key "M-7")
  :g "8" (general-simulate-key "M-9")
  :g "9" (general-simulate-key "M-9")
  :g "0" (general-simulate-key "M-0"))
 (:map ctl-x-at-8-map
  :g "[" #'goto-last-change
  :g "" #'goto-last-change-reverse)
 (:map ctl-x-9-map
  :g "j" #'iflipb-next-buffer
  :g "k" #'iflipb-previous-buffer)
 (:map +cmd-prefix-map
  :g "m" (general-simulate-key "s-m")
  :g "k" (general-simulate-key "s-k")
  :g "u" (general-simulate-key "s-u")
  :g "i" (general-simulate-key "s-i"))
 (:map +ctl-prefix-map
  ;; :g "i" (general-simulate-key "C-i")
  :g "'" (general-simulate-key "C-'")
  :g ";" (general-simulate-key "C-;")
  :g "-" (general-simulate-key "C--")
  :g "=" (general-simulate-key "C-=")
  :g "_" (general-simulate-key "C-_")
  :g "+" (general-simulate-key "C-+")
  :g "'" (general-simulate-key "C-'")
  :g "." (general-simulate-key "C-.")
  :g "," (general-simulate-key "C-,")
  :g "C-x" (general-simulate-key "C-RET")
  :g "C-s C-x" (general-simulate-key "C-S-RET"))
 (:map +ctl-p-prefix-map
  :g "r" #'profiler-report
  :g "1" #'profiler-start
  :g "0" #'profiler-stop)
 (:map +windmove-map
  :g "h" #'windmove-left
  :g "j" #'windmove-down
  :g "k" #'windmove-up
  :g "l" #'windmove-right)
 (:map yq-s-map
  :g "u" #'magit-dispatch
  :g "j" #'recentf-open-files
  :g "h" #'save-buffer
  :g "d" #'sp-kill-sexp
  :g "," #'sp-copy-sexp
  :g "y" #'zoxide-find-file
  :g "v" #'er/expand-region
  :g "c" #'delete-window
  :g "K" #'project-kill-buffers
  :g "a" #'loccur-current
  :g "/" #'evil-avy-goto-char-2
  :g "n" #'evil-avy-goto-char-timer
  :g "k" #'bury-buffer
  :g "m" #'+ivy/projectile-find-file
  :g "l" #'imenu
  :g "f" #'+default/search-project
  :g "F" #'+default/search-project-for-symbol-at-point
  :g "o" #'dired-jump
  :g "b" #'switch-to-buffer
  :g "RET" #'hydra-change-mode/body
  :g "R" #'rg)
 ;; (:map +thing-edit-map
 ;;  :g "," (cmd! (message "this operator is %s" evil-this-operator))
 ;;  :g "s" (+thing-edit-gen-evil-op-f 'sexp)
 ;;  :g "E" (+thing-edit-gen-evil-op-f 'email))
 (:after org
  [remap org-insert-link ar/org-insert-link-dwim])
 (:after evil-snipe
  (:map evil-snipe-override-mode-map
   :nm "s" nil)
  (:map evil-snipe-local-mode-map
   :nm "s" nil))
 (:after evil-iedit
  (:map evil-iedit-state-map
   "V" nil))
 (:after evil-multiedit
  (:map evil-multiedit-state-map
   :g "C-n" #'evil-multiedit-match-and-next
   :g "C-p" #'evil-multiedit-match-and-prev
   :g "n" #'evil-multiedit-next
   :g "p" #'evil-multiedit-prev))
 (:after swiper
  (:map swiper-map
   "ESC" #'minibuffer-keyboard-quit))
 (:after vertico
  (:map vertico-map
   "C-l" (general-simulate-key "RET")))
 (:after ivy
  (:map ivy-minibuffer-map
   "C-n" #'ivy-next-history-element
   "C-p" #'ivy-previous-history-element)
  (:map ivy-switch-buffer-map
   "C-d" #'ivy-switch-buffer-kill))
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
   :g "TAB" #'bicycle-cycle))
 (:after company
  (:map company-active-map
   :g "C-l" #'+company-complete-selection-or-default
   :g "ESC" (cmd! (company-abort) (when (evil-insert-state-p) (evil-normal-state)))
   :g "C-d" nil
   :g "C-m" nil
   :g "C-n" nil
   :g "C-p" nil
   :g "C-x @ 6 1" (cmd! (company-complete-number 1))
   :g "C-x @ 6 2" (cmd! (company-complete-number 2))
   :g "C-x @ 6 3" (cmd! (company-complete-number 3))
   :g "C-x @ 6 4" (cmd! (company-complete-number 4))
   :g "C-x @ 6 5" (cmd! (company-complete-number 5))
   :g "C-x @ 6 6" (cmd! (company-complete-number 6))
   :g "C-x @ 6 7" (cmd! (company-complete-number 7))
   :g "C-x @ 6 8" (cmd! (company-complete-number 8))
   :g "C-x @ 6 9" (cmd! (company-complete-number 9))
   :g "C-x @ 6 0" (cmd! (company-complete-number 0))))
 (:after lispy
  (:map lispy-mode-map
   :g "C-a" #'mwim-beginning-of-code-or-line
   :g "C-e" #'mwim-end-of-code-or-line
   :i "C-a" #'mwim-beginning-of-code-or-line
   :i "C-e" #'mwim-end-of-code-or-line))
 (:after lispyville
  (:map lispyville-mode-map
   :n ";" (cmd! (lispy-comment) (evil-next-visual-line))
   :n "ti" #'lispyville-backward-up-list
   :n "ta" (cmd! (lispyville-up-list) (lispy-newline-and-indent))
   :n "tR" #'lispyville-raise-list
   :n "tr" #'lispy-raise-sexp
   :n "tt" (cmd! (lispyville-backward-up-list) (lispy-parens 1))
   :n "td" #'transpose-sexps
   :n "tw" #'lispy-move-up
   :n "tJ" #'lispy-join
   :n "t/" #'lispy-splice
   :n "ts" #'lispy-split
   :n "tC" #'lispy-convolute
   :n "txb" #'lispy-bind-variable
   :n "{" (cmd! (lispyville-insert-at-beginning-of-list 1) (insert " ") (backward-char))
   :n "}" (cmd! (lispyville-insert-at-end-of-list 1) (insert ""))
   :ni "M-RET" #'lispyville-wrap-round
   :i "C-r" #'lispy-right
   :i "C-i" #'lispy-parens-down)
  :v "(" #'lispyville-wrap-round
  :v "{" #'lispyville-wrap-braces
  :v "[" #'lispyville-wrap-brackets)
 (:after json-mode
  (:map json-mode-map
   :g "C-c C-j" #'jq-interactively))
 (:after evil
  :nvm "C-e" nil
  :i "C-d" nil
  :i "C-n" nil
  :i "C-p" nil
  :i "C-r" nil
  ;; :i "C-m" #'newline-and-indent
  :i "C-j" #'avy-goto-char-timer
  :i "C-l" #'hippie-expand
  :i "." #'+yas-expand-when-inserting-dot
  :m "TAB" nil
  ;; seperate C-i and tab
  :m "C-x @ C-x i" #'evil-jump-forward
  :n "j" #'evil-next-visual-line
  :n "k" #'evil-previous-visual-line
  ;; dj dk
  :m "j" #'evil-next-line
  :m "k" #'evil-previous-line
  :n "gj" #'evil-next-line
  :n "gk" #'evil-previous-line
  :n "C-k" #'evil-toggle-fold
  :n "gy" #'yq/duplicate-line
  :n "gY" #'evilnc-copy-and-comment-lines
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
  :i "C-y" #'yank
  :m "r" '+thing-edit-map

  :textobj "F" #'+evil:defun-txtobj #'+evil:defun-txtobj
  :textobj "f" #'evil-textobj-anyblock-inner-block #'evil-textobj-anyblock-a-block

  ;; XXX: copied from doom-emacs config, needs better way to custom this
  :m [tab] (cmds! (and (featurep! :editor snippets)
                       (evil-visual-state-p)
                       (or (eq evil-visual-selection 'line)
                           (not (memq (char-after) (list ?\( ?\[ ?\{ ?\} ?\] ?\))))))
                  #'yas-insert-snippet
                  (and (featurep! :editor fold)
                       (save-excursion (end-of-line) (invisible-p (point))))
                  #'+fold/toggle
                  outline-minor-mode
                  #'bicycle-cycle

                  ;; Fixes #4548: without this, this tab keybind overrides
                  ;; mode-local ones for modes that don't have an evil
                  ;; keybinding scheme or users who don't have :editor (evil
                  ;; +everywhere) enabled.
                  (or (doom-lookup-key
                       [tab]
                       (list (evil-get-auxiliary-keymap (current-local-map) evil-state)
                             (current-local-map)))
                      (doom-lookup-key
                       (kbd "TAB")
                       (list (evil-get-auxiliary-keymap (current-local-map) evil-state)))
                      (doom-lookup-key (kbd "TAB") (list (current-local-map))))
                  it
                  (fboundp 'evil-jump-item)
                  #'evil-jump-item))
 (:after vterm
  (:map vterm-mode-map
   :gi "C-y" #'vterm-yank))
 (:after side-hustle
  (:map side-hustle-mode-map
   :g "TAB" #' side-hustle-show-item
   :g "q" #'side-hustle-toggle))
 (:after proced
  (:map proced-mode-map
   :g "/" #'proced-narrow
   :n "/" #'proced-narrow))
 (:after evil-textobj-tree-sitter
  :textobj ".f" (evil-textobj-tree-sitter-get-textobj "function.inner") (evil-textobj-tree-sitter-get-textobj "function.outer")
  :textobj ".l" (evil-textobj-tree-sitter-get-textobj "block.inner") (evil-textobj-tree-sitter-get-textobj "block.outer")
  :textobj ".C" (evil-textobj-tree-sitter-get-textobj "class.inner") (evil-textobj-tree-sitter-get-textobj "class.outer")
  :textobj ".t" (evil-textobj-tree-sitter-get-textobj "condition.inner") (evil-textobj-tree-sitter-get-textobj "condition.outer")
  :textobj ".L" (evil-textobj-tree-sitter-get-textobj "loop.inner") (evil-textobj-tree-sitter-get-textobj "loop.outer")
  :textobj ".c" (evil-textobj-tree-sitter-get-textobj "call.inner") (evil-textobj-tree-sitter-get-textobj "call.outer")
  :textobj ".s" (evil-textobj-tree-sitter-get-textobj "scopename.inner") (evil-textobj-tree-sitter-get-textobj "statement.outer")
  :textobj ".p" (evil-textobj-tree-sitter-get-textobj "parameter.inner") (evil-textobj-tree-sitter-get-textobj "parameter.outer"))
 (:after ranger
  (:map ranger-normal-mode-map
   :g "s" 'yq-s-map)))