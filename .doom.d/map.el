;;; map.el -*- lexical-binding: t; -*-

(define-prefix-command '+windmove-map)
(define-prefix-command '+cmd-prefix-map)
(define-prefix-command '+ctl-prefix-map)
(define-prefix-command '+ctl-p-prefix-map)
(define-prefix-command 'ctl-x-9-map)
(define-prefix-command 'ctl-x-8-map)
(define-prefix-command 'ctl-x-9-w-map)
(define-prefix-command 'ctl-x-8-w-map)
(define-prefix-command 'yq-s-map)
(define-prefix-command 'yq-cljr-map)
(define-prefix-command 'ctl-x-at-9-map)
(define-prefix-command 'ctl-x-at-8-map)
(define-prefix-command 'ctl-x-at-6-map)
(define-prefix-command '+thing-edit-map)
(define-prefix-command 'visual-t-map)

(global-set-key (kbd "<mouse-4>") (kbd "<wheel-up>"))
(global-set-key (kbd "<mouse-5>") (kbd "<wheel-down>"))

(setq doom-localleader-key ","
      doom-localleader-alt-key "M-SPC ,")

(map!
 :gn "C-." #'+eshell/toggle
 :g "C-;" #'embark-act
 :g "C-c" nil
 :g "C-c C-c" #'eval-defun
 :g "C-c '" #'separedit
 ;; :g "C-c s" #'swiper-isearch-thing-at-point
 :g "C-c s" #'+vertico/search-symbol-at-point
 ;; :g "C-c U" #'counsel-unicode-char
 :g "C-y" #'yank
 :g "C-c <xterm-paste>" #'+default/yank-pop
 :g "C-SPC" #'+default/search-buffer
 :g "C-@" #'+default/search-buffer
 :g "C-s" #'isearch-forward-regexp
 :g "C-r" #'isearch-backward-regexp
 :g "C-s" #'+default/search-buffer
 :g "C-s" #'swiper-isearch
 :g "C-SPC" #'counsel-grep-or-swiper
 :g "C-@" #'counsel-grep-or-swiper
 :g "s-k" #'bury-buffer
 :g "s-m" #'+popup/toggle
 :g "s-u" #'revert-buffer
 :g "s-i" #'side-notes-toggle-notes
 ;; :g "s-j" #'+whisper-run
 ;; :g "s-j" #'+chat-with-ai
 :g "s-j" #'chatgpt-shell-prompt-compose
 :g "C-M-s-7" '+windmove-map
 :g "C-M-s-j" #'iflipb-next-buffer
 :g "C-M-s-k" #'iflipb-previous-buffer
 ;; :g "C-M-s-j" #'next-buffer
 ;; :g "C-M-s-k" #'previous-buffer
 :g "C-a" #'mwim-beginning-of-code-or-line
 :g "C-e" #'mwim-end-of-code-or-line
 ;; :g "C-'" #'yq/vterm-toggle
 :g "C-'" #'+eat
 [remap split-window-below] #'evil-window-split
 [remap split-window-right] #'evil-window-vsplit
 [remap xterm-paste] #'yank
 [remap eval-last-sexp] #'pp-eval-last-sexp
 [eval-expression] #'pp-eval-expression
 [remap async-shell-command] #'detached-shell-command
 [remap compile] #'detached-compile
 [remap recompile] #'detached-compile-recompile
 (:leader
  (:prefix-map ("d" . "Detached")
   :desc "View" "l" #'detached-view-session
   :desc "Rerun" "R" #'detached-rerun-session
   :desc "List" "d" #'detached-list-sessions
   :desc "Kill" "K" #'detached-session-kill
   :desc "Compile" "c" #'detached-compile-session)
  (:prefix-map ("1" . "DO")
   :desc "Daily Capture" "c" #'org-roam-dailies-capture-today
   :desc "Daily Note" "1" #'org-roam-dailies-goto-today
   :desc "What To Do" "SPC" #'org-agenda
   :desc "New Node" "n" #'org-roam-capture
   :desc "Find Node" "f" #'org-roam-node-find
   :desc "Agenda" "a" (lambda (arg) (interactive "P") (org-agenda arg "a"))
   :desc "Log" "l" #'+forge-log-current-issue)
  ;; :desc "turbo log print" "cll" #'turbo-log-print-immediately
  :desc "Local Leader" "m" (general-simulate-key ",")
  :desc "M-x" "SPC" #'execute-extended-command
  :desc "Reveal in finder" "bf" #'reveal-in-osx-finder
  :desc "Open scratch buffer" "bs" #'doom/switch-to-scratch-buffer
  :desc "Open scratch project buffer" "bS" #'doom/switch-to-project-scratch-buffer
  :desc "Toggle popup" "bm" #'++popup-messages
  :desc "Toggle popup" "b RET" #'bookmark-set
  :desc "Find file" "ff" #'find-file
  :desc "Find file" "fj" #'dired-jump
  :desc "Compile with auto sudo pwd" "c SPC" #'++compile
  :desc "Open junk file" "fJ" #'yq/open-junk-file
  :desc "Indent" "j=" #'yq/indent-region-or-buffer
  ;; :desc "Tab buffer" "TAB" (cmd! () (switch-to-nth-buffer 1))
  ;; :desc "Tramp" "fT" #'counsel-tramp
  :desc "Git stage hunk" "gg" #'git-gutter:stage-hunk
  :desc "Magit status" "gs" #'magit-status
  :desc "Magit status here" "gS" #'magit-status-here
  :desc "Smerge mode" "gd" #'hydra-smerge/body
  :desc "Search other directory" "sf" #'+default/search-other-cwd
  :desc "Locate file" "sD" #'locate
  :desc "Toggle debug on error" "tD" #'toggle-debug-on-error
  ;; :desc "Toggle Tabnine" "tt" #'yq/toggle-company-tabnine
  :desc "Line numbers" "tl" #'+doom/toggle-line-numbers
  :desc "Imenu sidebar" "cb" #'side-hustle-toggle
  :desc "Git timemachine" "gt" #'git-timemachine
  :desc "Todoist" "o SPC" #'todoist
  :desc "RSS" "on" #'=rss
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
   ;; :desc "Resume ivy" "l" #'ivy-resume
   :desc "Resume vertico" "l" #'vertico-repeat)
  (:prefix-map ("fe" . "Edit srcs")
   :desc "Find library" "l" #'find-library
   :desc "Edit .doom.d config" "d" (cmd! (let ((default-directory (expand-file-name "~/.doom.d/")))
                                           (call-interactively #'find-file)))
   :desc "Search in ~/.emacs.d" "s" (cmd! (find-file-existing "~/.ssh/config.gpg") (call-interactively #'+default/search-buffer))
   :desc "Search in ~/.emacs.d" "m" (cmd! (let ((default-directory (expand-file-name "~/.emacs.d/")))
                                            (call-interactively #'+default/search-project)))
   :desc "Edit goku edn config" "k" (cmd!
                                     ;; (let ((projectile-switch-project-action (cmd!)))
                                     ;;   (projectile-switch-project-by-name "~/.nixpkgs/"))
                                     (find-file-existing "~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/karabiner.edn"))
   :desc "Edit nix config" "n" (cmd!
                                ;; (projectile-switch-project-by-name "~/.nixpkgs/")
                                (let ((default-directory (expand-file-name "~/.nixpkgs/")))
                                  (call-interactively #'project-find-file)))
   :desc "Edit hammerspoon config" "h" (cmd! (find-file-existing "~/.spacehammer/config.fnl"))
   :desc "Edit surge config" "S" (cmd! (find-file-existing "~/Dropbox/sync/surge/D.conf"))
   :desc "Find library" "l" #'find-library))
 (:localleader
  (:after js2-mode
          (:map js2-mode-map
           :n "jj" #'+jest-popup
           :n "jd" #'+jest-popup-debug
           :n "rll" #'turbo-log-print-immediately
           :n "rlv" #'turbo-log-print))
  (:after org
          (:map org-mode-map
           :n "i" nil
           :n "it" #'org-toggle-item
           :n "iii" #'+org/insert-item-below
           :n "iik" #'+org/insert-item-above
           :n "ihh" #'org-insert-heading
           :n "ihj" #'org-insert-heading-after-current
           :n "iht" (cmd! (org-insert-heading) (insert (format-time-string "%T")))
           :n "ds" #'orgbox-schedule))
  (:after clojure-mode
          (:map clojure-mode-map
           :n "," 'yq-cljr-map))
  (:after todoist
          (:map todoist-mode-map
           :g "," #'todoist-task-menu)))
 (:map help-map
       "K" #'describe-keymap)
 (:map ctl-x-map
  :g "C-b" #'ibuffer
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
  "@ 9" 'ctl-x-at-9-map
  "8" 'ctl-x-8-map
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
 (:map ctl-x-8-map
       "w" 'ctl-x-8-w-map
       "@" 'ctl-x-at-8-map)
 (:map ctl-x-9-map
       "w" 'ctl-x-9-w-map
       "@" 'ctl-x-at-9-map)
 (:map ctl-x-9-w-map
  :g "h" #'+evil/window-move-left
  :g "j" #'+evil/window-move-down
  :g "k" #'+evil/window-move-up
  :g "l" #'+evil/window-move-right)
 (:map ctl-x-at-9-map
  :g "j" #'iflipb-next-buffer
  :g "k" #'iflipb-previous-buffer
  ;; :g "j" #'next-buffer
  ;; :g "k" #'previous-buffer
  )
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
  :g "s" #'+ss
  :g "S" #'+sS
  :g "0" #'+gpt-dwim-current-buffer
  :g "P" #'doom/find-file-in-other-project
  :g "p" #'++projectile-switch-project-and-rename-workspace
  :g "u" #'magit-dispatch
  ;; :g "j" #'recentf-open-files
  :g "j" #'projectile-recentf
  :g "h" #'save-buffer
  :g "d" #'sp-kill-sexp
  :g "," #'sp-copy-sexp
  :g "y" #'consult-dir
  :g "v" #'er/expand-region
  ;; :g "v" #'expreg-expand
  :g "c" #'delete-window
  :g "K" #'project-kill-buffers
  :g "a" #'loccur-current
  :g "/" #'evil-avy-goto-char-2
  :g "n" #'evil-avy-goto-char-timer
  :g "k" #'bury-buffer
  ;; :g "m" #'+ivy/projectile-find-file
  :g "m" #'projectile-find-file
  :g "l" #'imenu
  :g "f" #'+default/search-project
  :g "F" #'+default/search-project-for-symbol-at-point
  :g "o" #'dired-jump
  :g "b" #'switch-to-buffer
  :g "RET" #'hydra-change-mode/body
  :g "R" #'rg)
 (:map visual-t-map
  :g "t" #'gptel-menu
  :g "j" (cmd!
          ;; (insert-before-and-after-region "```" "```")
          (gptel--suffix-send `("p" ,(concat "nGPT-" (buffer-name (current-buffer)) ".md")))))
 (:map yq-cljr-map
  :desc "Add import to ns" "ai" #'lsp-clojure-add-import-to-namespace
  :desc "Add missing libspec" "am" #'lsp-clojure-add-missing-libspec
  :desc "Add project dependency" "ap" #'cljr-add-project-dependency
  :desc "Add require to ns" "ar" #'cljr-add-require-to-ns
  :desc "Add stubs for the interface/protocol at point" "as" #'cljr-add-stubs
  :desc "Add use to ns" "au" #'cljr-add-use-to-ns
  :desc "Cycle if" "ci" #'clojure-cycle-if
  :desc "Clean ns" "cn" #'lsp-clojure-clean-ns
  :desc "Cycle coll" "cc" #'lsp-clojure-cycle-coll
  :desc "Cycle privacy" "cp" #'lsp-clojure-cycle-privacy
  :desc "Change function signature" "cs" #'cljr-change-function-signature
  :desc "Cycle thread" "ct" #'cljr-cycle-thread
  :desc "Destructure keys" "dk" #'cljr-destructure-keys
  :desc "Extract constant" "ec" #'cljr-extract-constant
  :desc "Extract form as def" "ed" #'cljr-extract-def
  :desc "Extract function" "ef" #'lsp-clojure-extract-function
  :desc "Expand let" "el" #'lsp-clojure-expand-let
  :desc "Introduce let" "il" #'lsp-clojure-introduce-let
  :desc "Create function from example" "fe" #'cljr-create-fn-from-example
  :desc "Find usages" "fu" #'cljr-find-usages
  :desc "Hotload dependency" "hd" #'cljr-hotload-dependency
  :desc "Introduce let" "il" #'cljr-introduce-let
  :desc "Inline symbol" "is" #'lsp-clojure-inline-symbol
  :desc "Move form" "mf" #'cljr-move-form
  :desc "Move to let" "ml" #'lsp-clojure-move-to-let
  :desc "Project clean" "pc" #'cljr-project-clean
  :desc "Promote function" "pf" #'cljr-promote-function
  :desc "Rename file-or-dir" "rf" #'cljr-rename-file-or-dir
  :desc "Remove let" "rl" #'cljr-remove-let
  :desc "Add to or extend the require-macros form" "rm" #'cljr-require-macro
  :desc "Rename symbol" "rs" #'cljr-rename-symbol
  :desc "Show the project's changelog" "sc" #'cljr-show-changelog
  :desc "Sort project dependencies" "sp" #'cljr-sort-project-dependencies
  :desc "Stop referring" "sr" #'cljr-stop-referring
  :desc "Thread first all" "tf" #'clojure-thread-first-all
  :desc "Thread" "th" #'clojure-thread
  :desc "Thread last all" "tl" #'clojure-thread-last-all
  :desc "Unwind all" "ua" #'clojure-unwind-all
  :desc "Update project dependencies" "up" #'cljr-update-project-dependencies
  :desc "Unwind" "uw" #'clojure-unwind
  :desc "Add declaration" "ad" #'cljr-add-declaration
  :desc "Describe refactoring" "?" #'cljr-describe-refactoring
  :desc "Parent menu for hydra menus" "hh" #'hydra-cljr-help-menu/body
  :desc "Hydra menu for ns refactorings" "hn" #'hydra-cljr-ns-menu/body
  :desc "Hydra menu for code refactorings" "hc" #'hydra-cljr-code-menu/body
  :desc "Hydra menu for project refactorings" "hp" #'hydra-cljr-project-menu/body
  :desc "Hydra menu for top level refactorings " "ht" #'hydra-cljr-toplevel-form-menu/body
  :desc "Hydra menu for self features" "hs" #'hydra-cljr-cljr-menu/body)
 ;; (:map +thing-edit-map
 ;;  :g "," (cmd! (message "this operator is %s" evil-this-operator))
 ;;  :g "s" (+thing-edit-gen-evil-op-f 'sexp)
 ;;  :g "E" (+thing-edit-gen-evil-op-f 'email))
 (:after org
         [remap org-insert-link] 'ar/org-insert-link-dwim)
 (:after evil-snipe
         (:map evil-snipe-override-mode-map
          :nm "s" nil
          :v "t" 'visual-t-map)
         (:map evil-snipe-local-mode-map
          :nm "s" nil)
         [remap evil-find-char] #'evil-snipe-f)
 (:after evil-iedit
         (:map evil-iedit-state-map
               "V" nil))
 (:after evil-multiedit
         (:map evil-multiedit-state-map
          :g "C-n" #'evil-multiedit-match-and-next
          :g "C-p" #'evil-multiedit-match-and-prev
          :g "n" #'evil-multiedit-next
          :g "p" #'evil-multiedit-prev))
 (:after vertico
         (:map vertico-map
               "C-l" #'vertico-directory-enter
               "RET" #'vertico-exit
               "C-p" (general-simulate-key "<prior>")
               "C-n" (general-simulate-key "<next>")))
 (:after ivy
         (:map ivy-minibuffer-map
               "C-n" #'ivy-next-history-element
               "C-p" #'ivy-previous-history-element)
         (:map ivy-switch-buffer-map
               "C-d" #'ivy-switch-buffer-kill))
 (:after dired
         (:map dired-mode-map
          ;; :n "q" #'quit-window
          ;; :n "TAB" #'dirvish-subtree-toggle
          ;; :n "*" #'dirvish-mark-menu
          ;; :n "o" #'dirvish-quicksort
          ;; :n "M-p" #'dirvish-history-go-backward
          ;; :n "M-n" #'dirvish-history-go-forward
          ;; :n "F" #'dirvish-fd
          ;; [remap dired-summary] #'dirvish-dispatch
          ;; [remap dired-do-redisplay] #'dirvish-ls-switches-menu

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
 (:after corfu
         (:map corfu-map
          :gi "C-l" #'corfu-insert
          ;; :gi "ESC" #'keyboard-escape-quit
          :gi "C-d" nil
          :gi "C-m" nil
          :gi "C-n" nil
          :gi "C-p" nil))
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
          :n "tr" #'paredit-raise-sexp
          :n "tt" (cmd! (lispyville-backward-up-list) (lispy-parens 1))
          :n "td" #'transpose-sexps
          :n "tw" #'lispy-move-up
          :n "tl" #'+spy
          :n "tJ" #'lispy-join
          :n "t/" #'lispy-splice
          :n "tj" #'symex-evaluate
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
 ;; (:after symex
 ;;   (:map symex-editing-mode-map
 ;;     :o "j" #'symex-go-forward
 ;;     :o "k" #'symex-go-backward
 ;;     :o "h" #'symex-go-up
 ;;     :o "l" #'symex-go-down))
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
  ;; :i "," #'+company-complete
  :i "," #'+complete-at-point
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
  [remap evil-ret] #'evil-jump-item
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

  :textobj "g" #'evil-textobj-anyblock-inner-block #'evil-textobj-anyblock-a-block
  :textobj "G" #'+evil:whole-buffer-txtobj #'+evil:whole-buffer-txtobj

  ;; XXX: copied from doom-emacs config, needs better way to custom this
  :m [tab] (cmds! (and (modulep! :editor snippets)
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
 (:after evil-matchit
         (:map evil-matchit-mode-map
          :mnv "RET" #'evilmi-jump-items))
 (:after vterm
         (:map vterm-mode-map
          :gi "C-y" #'vterm-yank
          :g "C-l" #'recenter-top-bottom
          :i "C-l" #'vterm-clear
          :i "C-e" #'vterm-send-C-e
          :i "C-a" #'vterm-send-C-a
          :i "C-w" #'vterm-send-C-w))
 (:after side-hustle
         (:map side-hustle-mode-map
          :g "TAB" #'side-hustle-show-item
          :g "q" #'side-hustle-toggle))
 (:after proced
         (:map proced-mode-map
          :g "/" #'proced-narrow
          :n "/" #'proced-narrow))
 (:after swiper
         (:map swiper-map
          :g "<escape>" (general-simulate-key "C-g")
          :g "C-j" (general-simulate-key "<down>")
          :g "C-k" (general-simulate-key "<up>")
          :g "C-w" #'doom/delete-backward-word))
 (:after ivy
         (:map ivy-minibuffer-map
          :g "<escape>" (general-simulate-key "C-g")
          :g "C-j" (general-simulate-key "<down>")
          :g "C-k" (general-simulate-key "<up>")
          :g "C-w" #'doom/delete-backward-word))
 ;; (:after evil-textobj-tree-sitter)
 (:after ranger
         (:map ranger-normal-mode-map
          :g "s" 'yq-s-map))
 (:after notmuch
         [remap evil-collection-notmuch-tree-toggle-delete] #'+notmuch/tree-delete
         [remap evil-collection-notmuch-show-toggle-delete] #'+notmuch/show-delete
         [remap evil-collection-notmuch-search-toggle-delete] #'+notmuch/search-delete
         (:map notmuch-hello-mode-map
          :n "i" #'notmuch-hello-mode-transient)
         (:map notmuch-search-mode-map
          :n "i" #'notmuch-search-mode-transient)
         (:map notmuch-tree-mode-map
          :n "i" #'notmuch-tree-mode-transient)
         (:map notmuch-show-mode-map
          :n "s" 'yq-s-map
          :n "gs" #'notmuch-search
          :n "i" #'notmuch-show-mode-transient))
 (:after code-review
         (:map code-review-mode-map
          :gnv "r" #'code-review-transient-api
          :nv "i" #'code-review-comment-add-or-edit
          :n "M-RET" #'code-review-comment-add-or-edit
          :gn "RET" #'magit-diff-visit-file-other-window))
 (:after elfeed
         (:map elfeed-show-mode-map
          :n "&" (cmd! () (elfeed-show-visit 1))
          :n "R" #'writeroom-mode
          :n "o" (cmd! () (let ((current-prefix-arg 1)) (call-interactively #'shr-browse-url)))
          :n "gy" #'elfeed-show-yank
          :n "gr" #'elfeed-show-refresh
          :n "s" 'yq-s-map
          :n "gs" #'elfeed-show-new-live-search
          :n "C-j" #'+rss/next
          :n "C-k" #'+rss/previous))
 (:after pprint-to-buffer
         (:map emacs-lisp-mode-map
          :g "C-c C-p" #'pprint-to-buffer-last-sexp))
 (:after ibuffer
         (:map ibuffer-mode-map
          :n "gX" #'ibuffer-do-kill-lines))
 (:after cider
         (:map cider-inspector-mode-map
          :n "C-j" #'cider-inspector-next-inspectable-object
          :n "C-k" #'cider-inspector-previous-inspectable-object
          :n "gj" #'cider-inspector-next-page
          :n "gk" #'cider-inspector-prev-page)
         (:map cider-repl-mode-map
               (:localleader
                "sp" (cmd!
                      (+cider-repl-clear-input)
                      (cider-insert-in-repl "(println :connected)" t))
                "sd" (cmd!
                      (+cider-repl-clear-input)
                      (cider-insert-in-repl "(require 're-frame.db) (deref re-frame.db/app-db)" t)))))
 (:after forge
         (:map forge-pullreq-list-mode-map
          :n "RET" #'forge-visit-topic
          :n "o" #'forge-browse-topic
          :n "'" #'forge-dispatch))
 (:after eat
         (:map eat-mode-map
          :g "C-c DEL" #'+workspace/close-window-or-workspace))
 (:after copilot
         (:map copilot-completion-map
          :i "RET" #'copilot-accept-completion
          :i "TAB" #'copilot-accept-completion-by-line
          :i "C-l" #'copilot-accept-completion-by-word))
 (:after time
         (:map world-clock-mode-map
          :n "q" #'quit-window
          :n "gr" #'revert-buffer))
 (:after eww
         (:map eww-mode-map
          :n "S" #'+summarize-current-eww-buffer))
 (:after detached
         (:map detached-list-mode-map
          :n "m" #'detached-list-mark-session
          :n "u" #'detached-list-unmark-session
          :n "K" #'detached-list-kill-session
          :n "d" #'detached-list-delete-session
          :n "gr" #'detached-list-revert
          :n "y" #'detached-copy-session-command
          :n "Y" #'detached-copy-session-output
          :n "R" #'detached-list-rerun-session
          :n "C" #'detached-compile-session
          :n "RET" #'detached-list-open-session
          :n "a" #'detached-edit-session-annotation
          :n "q" #'detached-list-quit))
 (:after js
         (:map js-ts-mode-map
          :n "M-h" #'combobulate-mark-node-dwim)
         (:localleader
          (:map js-ts-mode-map
           :n ",h" #'combobulate-navigate-logical-previous
           :n ",l" #'combobulate-navigate-logical-next
           :n ",c" #'combobulate-clone-node-dwim)))
 (:after combobulate
         (:map combobulate-key-map
          :g "C-M-a" nil
          :g "C-M-d" nil
          :g "C-M-e" nil
          :g "C-M-h" nil
          :g "C-M-n" nil
          :g "C-M-p" nil
          :g "C-M-t" nil
          :g "C-M-u" nil
          :g "M-<up>" nil
          :g "M-<down>" nil
          :g "M-<left>" nil
          :g "M-<right>" nil
          :g "M-N" nil
          :g "M-P" nil
          :g "M-a" nil
          :g "M-e" nil
          :g "M-h" nil
          :g "M-k" nil
          :g "M-(" nil
          :n "<up>" #'combobulate-navigate-previous
          :n "<down>" #'combobulate-navigate-next
          :n "<left>" #'combobulate-navigate-up
          :n "<right>" #'combobulate-navigate-down))
 ;; (:after combobulate-mode
 ;;         (:map prog-mode-map
 ;;          :n "<up>" #'combobulate-navigate-previous
 ;;          :n "<down>" #'combobulate-navigate-next
 ;;          :n "<left>" #'combobulate-navigate-up
 ;;          :n "<right>" #'combobulate-navigate-down))
 (:after calc
         (:map calc-mode-map
          :g "C-o" #'casual-main-menu)))
