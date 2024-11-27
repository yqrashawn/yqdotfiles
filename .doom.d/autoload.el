;;; funcs.el -*- lexical-binding: t; -*-

;;;###autoload
(defun in-terminal-p ()
  (and (not (display-graphic-p)) (daemonp)))

;;;###autoload
(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

;;;###autoload
(defun yq/kill-this-buffer (&optional arg)
  "Kill the current buffer.
If the universal prefix argument is used then kill also the window."
  (interactive "P")
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (if (equal '(4) arg)
        (kill-buffer-and-window)
      (kill-buffer))))

;;;###autoload
(defun bjm/ivy-dired-recent-dirs ()
  "Present a list of recently used directories and open the selected one in dired"
  (interactive)
  (let ((recent-dirs (delete-dups
                      (mapcar
                       (lambda (file)
                         (if (not (string-match
                                   "/\\(rsh\\|ssh\\|telnet\\|su\\|sudo\\|sshx\\|krlogin\\|ksu\\|rcp\\|scp\\|rsync\\|scpx\\|fcp\\|nc\\|ftp\\|smb\\|adb\\):"
                                   file))
                             (if (file-directory-p file)
                                 file
                               (file-name-directory file))
                           (if (file-name-directory file)
                               (file-name-directory file))))
                       recentf-list))))
    (let ((dir (ivy-read
                "Directory: "
                recent-dirs
                ;; :re-builder #'ivy--regex
                :sort nil
                :initial-input nil)))
      (dired dir))))

;;;###autoload
(defun +clj-file-p ()
  (memq major-mode '(clojure-mode clojurescript-mode clojurec-mode)))

;;;###autoload
(defun spacemacs/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (save-excursion
    (if (and (= 1 (length (window-list)))
             (assoc ?_ register-alist))
        (jump-to-register ?_)
      (progn
        (window-configuration-to-register ?_)
        (delete-other-windows)))))

;;;###autoload
(defun yq/duplicate-line ()
  "Duplicate current line."
  (interactive)
  (kill-whole-line)
  (yank)
  (yank))

;;;###autoload
(defun yq/indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (indent-region
         (region-beginning)
         (region-end))
      (if (and (fboundp 'lispyvile-prettify) (memq major-mode '(clojure-mode emacs-lisp-mode)))
          (lispyville-prettify
           (point-min)
           (point-max))
        (evil-indent
         (point-min)
         (point-max))))
    (whitespace-cleanup)))

;;;###autoload
(defun switch-to-nth-buffer (n)
  "Switches to nth most recent buffer. Ignores a bunch of stuff."
  (catch 'tag
    (mapcar (lambda (b)
              (unless
                  (or
                   (minibufferp b)
                   (string-match "^ " (buffer-name b))
                   (string-match "\*" (buffer-name b))
                   (equal b (current-buffer)))
                (if (= n 1)
                    (progn
                      (switch-to-buffer b)
                      (throw 'tag nil))
                  (setq n (- n 1)))))
            (buffer-list))))

;;;###autoload
(defun diff-last-two-kills ()
  "Write the last two kills to temporary files and diff them."
  (interactive)
  (let ((old "/tmp/old-kill") (new "/tmp/new-kill"))
    (with-temp-file new
      (insert (current-kill 0 t)))
    (with-temp-file old
      (insert (current-kill 1 t)))
    (diff old new "-u" t)))

;;;###autoload
(defun +ivy-switch-buffer-next-line ()
  (interactive)
  (if (minibufferp) (ivy-next-line)
    (ivy-switch-buffer)))

;;;###autoload
(defun +ivy-switch-buffer-prev-line ()
  (interactive)
  (if (minibufferp) (ivy-previous-line)
    (ivy-switch-buffer)))

;;;###autoload
(defun yq/open-junk-file (&optional arg)
  (interactive "P")
  (let* ((fname (format-time-string "/tmp/junk/%Y/%m/%d-%H%M%S." (current-time)))
         (rel-fname (file-name-nondirectory fname))
         (junk-dir (file-name-directory fname))
         (default-directory junk-dir))
    (find-file rel-fname)))

;;;###autoload
(defun +imenu-comments ()
  "Imenu display comments."
  (interactive)
  (let* ((imenu-create-index-function 'evilnc-imenu-create-index-function))
    (imenu)))

;;;###autoload
(defun yq/open-with-call-alfred-osascript (file)
  (shell-command (concat "osascript -e '" (format "-- Search for the file
    tell application \"Alfred 4\"
      search \"%1$s\"
    end tell

    -- Show file actions
    tell application \"System Events\"
      -- Press \"tab\" to show file actions
      key code 48
    end tell'" file))))

;;;###autoload
(defun yq/open-with-alfred ()
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (yq/open-with-call-alfred-osascript (dired-get-filename nil t))
    (and (file-exists-p buffer-file-name) (yq/open-with-call-alfred-osascript buffer-file-name))))

;;;###autoload
(defun +jest-popup-debug ()
  (interactive)
  (require 'jest)
  (setq-local jest-executable (s-trim (s-replace "yarn" "ndb yarn" (s-replace "ndb" "" jest-executable))))
  (jest-popup))

;;;###autoload
(defun +jest-popup ()
  (interactive)
  (require 'jest)
  (setq-local jest-executable (s-trim (s-replace "ndb" "" jest-executable)))
  (jest-popup))

;;;###autoload
(defun +yq/undebug-on-event ()
  (interactive)
  (cancel-debug-on-entry)
  (setq debug-on-next-call nil)
  (setq debug-on-quit nil)
  (setq quit-flag nil)
  (setq inhibit-quit nil))

;;;###autoload (autoload '+vc/smerge-hydra/body "autoload" nil t)
(defhydra +vc/smerge-hydra (:hint nil
                            :pre (if (not smerge-mode) (smerge-mode 1))
                            ;; Disable `smerge-mode' when quitting hydra if
                            ;; no merge conflicts remain.
                            :post (smerge-auto-leave))
  "
                                                         [smerge]
  Movement   Keep           Diff              Other
  ╭─────────────────────────────────────────────────────────╯
     ^_g_^       [_b_] base       [_<_] upper/base    [_C_] Combine
     ^_C-k_^     [_u_] upper      [_=_] upper/lower   [_r_] resolve
     ^_k_ ↑^     [_l_] lower      [_>_] base/lower    [_R_] remove
     ^_j_ ↓^     [_a_] all        [_H_] hightlight    [_W_] Save
     ^_C-j_^     [_RET_] current  [_E_] ediff                 ╭──────────
     ^_G_^                                                │ [_q_] quit
"
  ("g" (progn (goto-char (point-min)) (smerge-next)))
  ("G" (progn (goto-char (point-max)) (smerge-prev)))
  ("C-j" smerge-next)
  ("C-k" smerge-prev)
  ("j" next-line)
  ("k" previous-line)
  ("b" smerge-keep-base)
  ("u" smerge-keep-upper)
  ("l" smerge-keep-lower)
  ("a" smerge-keep-all)
  ("RET" smerge-keep-current)
  ("\C-m" smerge-keep-current)
  ("<" smerge-diff-base-upper)
  ("=" smerge-diff-upper-lower)
  (">" smerge-diff-base-lower)
  ("H" smerge-refine)
  ("E" smerge-ediff)
  ("C" smerge-combine-with-next)
  ("r" smerge-resolve)
  ("R" smerge-kill-current)
  ("W" save-buffer :color blue :exit t)
  ("w" save-buffer :color blue)
  ("q" nil :color blue))

;;;###autoload (autoload 'hydra-smerge/body "autoload" nil t)
(defhydra hydra-smerge (:color pink :hint nil :post (smerge-auto-leave))
  "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_j_ext       _b_ase               _<_: upper/base        _C_ombine
_k_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _K_ill current
^^           _a_ll                _R_efine               _w_save buffer
^^           _RET_: current       _e_diff
"
  ("j" smerge-next)
  ("k" smerge-prev)
  ("b" smerge-keep-base)
  ("u" smerge-keep-upper)
  ("l" smerge-keep-lower)
  ("a" smerge-keep-all)
  ("RET" smerge-keep-current)
  ("\C-m" smerge-keep-current)
  ("<" smerge-diff-base-upper)
  ("=" smerge-diff-upper-lower)
  (">" smerge-diff-base-lower)
  ("r" smerge-refine)
  ("e" smerge-ediff)
  ("C" smerge-combine-with-next)
  ("R" smerge-resolve)
  ("K" smerge-kill-current)
  ("ZZ" (cmd! (save-buffer) (bury-buffer)) "Save and bury buffer" :color blue)
  ("w" save-buffer "Save buffer" :color blue)
  ("q" nil "cancel" :color blue)
  ("C-g" nil "cancel" :color blue))

;;;###autoload (autoload 'hydra-change-mode/body "autoload" nil t)
(defhydra hydra-change-mode (:hint nil :color pink)
  "
_e_  elisp    _c_  clojure   _t_  typescript
_j_  js2      _T_  text      _f_  fundamental
_g_  gfm      _o_ org        _m_ markdown
"
  ("e" emacs-lisp-mode :exit t)
  ("j" js2-mode :exit t)
  ("c" clojure-mode :exit t)
  ("T" text-mode :exit t)
  ("t" typescript-mode :exit t)
  ("f" fundamental-mode :exit t)
  ("m" markdown-mode :exit t)
  ("g" gfm-mode :exit t)
  ("o" org-mode :exit t)
  ("q" hydra-keyboard-quit :exit t)
  ("C-g" hydra-keyboard-quit :exit t))

;;;###autoload
(defun ++doom-apply-ansi-color-to-compilation-buffer-h ()
  "Applies ansi codes to the compilation buffers. Meant for
`compilation-filter-hook'."
  (with-silent-modifications
    (ansi-color-apply-on-region (point-min) (point-max))))

;;;###autoload
(defun +company-complete-selection-or-default ()
  (interactive)
  (let ((company-selection (or company-selection
                               company-selection-default
                               0)))
    (call-interactively #'company-complete-selection)))

;;;###autoload
(defun yq/toggle-company-tabnine ()
  (interactive)
  (when (fboundp 'company-tabnine-restart-server)
    (company-tabnine-restart-server)
    (if company-tabnine--disabled
        (progn
          ;; (setq company-idle-delay 0)
          (setq company-tabnine--disabled nil)
          ;; (when lsp-mode
          ;;   (call-interactively #'lsp-workspace-shutdown))
          (message "Turn on company-tabnine"))
      (progn
        ;; (setq company-idle-delay 0.2)
        (setq company-tabnine--disabled t)
        ;; (call-interactively #'revert-buffer)
        (message "Turn off company-tabnine")))))

;;;###autoload
(defun +tree-sitter-manybe-enable ()
  (require 'tree-sitter-langs)
  ;; Activate tree-sitter's improved syntax highlighting only if we are
  ;; using a major-mode that has a compatible tree-sitter syntax parser
  (if (and (boundp 'tree-sitter-major-mode-language-alist)
           (assq major-mode tree-sitter-major-mode-language-alist))
      (tree-sitter-mode)))

;;;###autoload
(defun +company-complete (&optional args)
  (interactive)
  (if (eq major-mode 'vterm-mode)
      (vterm--self-insert)
    (if (and (modulep! :editor evil)
             (modulep! :completion company)
             company-mode
             (eq (preceding-char) ?,))
        (progn (delete-char -1 nil)
               (+company/complete))
      (call-interactively #'self-insert-command))))

;;;###autoload
(defun +complete-at-point (&optional args)
  (interactive)
  (if (eq major-mode 'vterm-mode)
      (vterm--self-insert)
    (cond
     ((and (eq (preceding-char) ?,) (boundp 'pabbrev-marker) pabbrev-marker)
      (progn (delete-char -1 nil)
             (call-interactively #'pabbrev-expand-maybe)
             (pabbrev-delete-last-suggestion)))

     ((and (eq (preceding-char) ?,) (modulep! :editor evil) (modulep! :completion corfu) corfu-mode)
      (progn (delete-char -1 nil)
             (call-interactively #'completion-at-point)))

     ((and (eq (preceding-char) ?.) (bound-and-true-p copilot-mode) (not (copilot--overlay-visible)))
      (progn
        (delete-char -1 nil)
        (call-interactively #'copilot-complete)))

     (t (call-interactively #'self-insert-command)))))

;;;###autoload
(defun +yas-expand-when-inserting-dot (&optional args)
  (interactive)
  (if (eq major-mode 'vterm-mode) (vterm--self-insert)
    (if (eq (preceding-char) ?.)
        (if (and (not (delete-char -1 nil)) (yas-expand))
            t
          (progn
            (insert ?.)
            (insert ?.)))
      (insert ?.))))

;;;###autoload
(defun yq/vterm-toggle (arg)
  (interactive "P")
  (if (display-graphic-p)
      (and (modulep! :term vterm) (+vterm/toggle (not arg)))
    (and (fboundp 'yq/split-window-below-tmux) (yq/split-window-below-tmux (not arg)))))

;;;###autoload
(defun +thing-edit-gen-evil-op-f (el)
  (cmd!
   (let* ((op (and evil-this-operator (symbol-name evil-this-operator)))
          (del-op? (and op (string-match-p "delete" op)))
          (yank-op? (and op (string-match-p "yank" op)))
          (rst-op (cond (del-op? "-cut-")
                        (yank-op? "-copy-")
                        (t nil)))
          (f-name (and rst-op (concat "thing" rst-op (symbol-name el))))
          (new-f (and f-name (intern f-name))))
     ;; TODO: support evil register
     (when new-f
       (call-interactively new-f)
       (when (bound-and-true-p evil-mode)
         (evil-set-register ?0 (current-kill 0)))))))

;;;###autoload (autoload 'hydra-outline/body "autoload" nil t)
(defhydra hydra-outline (:color pink :hint nil)
  "
_b_ranch _j_next _k_prev _h_up
"

  ;; ("t" outline-toggle-children)
  ;; ("l" outline-toggle-children)
  ("b" outline-show-branches)
  ("n" outline-next-heading)
  ("p" outline-previous-heading)
  ("N" outline-previous-heading)
  ("j" outline-next-visible-heading)
  ("k" outline-previous-visible-heading)
  ("h" outline-up-heading)
  ("x" outline-hide-leaves)
  ("o" outline-hide-other)
  ("r" outline-show-all)
  ("q" nil "cancel" :color blue)
  ("C-g" nil "cancel" :color blue)
  ("<escape>" nil "cancel" :color blue))

;;;###autoload
(defun nuke-all-buffers ()
  (interactive)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows))

;;;###autoload
(defun yq-new-blot-post ()
  (interactive)
  ;; (require 's)
  (let* ((blot-folder "~/Dropbox/application/Blot/")
         (time-folder (format-time-string "%Y/%m/%d/"))
         (blog-dir (expand-file-name (concat blot-folder time-folder)))
         (title (replace-regexp-in-string
                 " +" " "
                 (read-string "Blog title: ")))
         (filename (replace-regexp-in-string " " "-" (downcase title)))
         (filename (concat "_" filename ".md"))
         (filename (concat blog-dir filename)))
    (make-empty-file filename t)
    (find-file filename)
    (insert (concat "sbt.\n\n# " title))
    (goto-char (point-min))
    (end-of-line)))

;;;###autoload
(defun yq-publish-blot-post ()
  (interactive)
  (let* ((cur-file-path (buffer-file-name (current-buffer)))
         (cur-dir (file-name-directory cur-file-path))
         (new-file-name (save-match-data
                          (and (string-match "\/_.*\.\\(txt\\|org\\|md\\)$" cur-file-path)
                               (substring (match-string 0 cur-file-path) 2)))))
    (cl-assert
     (string-match-p "\/Dropbox\/应用\/Blot\/" cur-file-path)
     nil "No in Blot folder")
    (cl-assert
     (string-match-p "\/Dropbox\/应用\/Blot\/.*\/_.*\.\\(txt\\|org\\|md\\)$" cur-file-path)
     nil "Invalid blog format, must be one of txt, org, md")
    (cl-assert
     (string-match-p "\/Dropbox\/应用\/Blot\/.*\/_.*\.\\(txt\\|org\\|md\\)$" cur-file-path)
     nil "Already published")
    (rename-file cur-file-path new-file-name)
    (kill-buffer (current-buffer))
    (find-file (concat cur-dir new-file-name))))
;;;###autoload
(defun ++popup-messages (arg)
  (interactive "P")
  (if arg
      (+popup-buffer (get-buffer "*Messages*"))
    (+popup-buffer (get-buffer "*Messages*") '((quit . t)))))

;; https://xenodium.com/emacs-dwim-do-what-i-mean/
;;;###autoload
(defun ar/org-insert-link-dwim ()
  "Like `org-insert-link' but with personal dwim preferences."
  (interactive)
  (let* ((point-in-link (org-in-regexp org-link-any-re 1))
         (clipboard-url (when (string-match-p "^http" (current-kill 0))
                          (current-kill 0)))
         (region-content (when (region-active-p)
                           (buffer-substring-no-properties (region-beginning)
                                                           (region-end)))))
    (cond ((and region-content clipboard-url (not point-in-link))
           (delete-region (region-beginning) (region-end))
           (insert (org-make-link-string clipboard-url region-content)))
          ((and clipboard-url (not point-in-link))
           (insert (org-make-link-string
                    clipboard-url
                    (read-string "title: "
                                 (with-current-buffer (url-retrieve-synchronously clipboard-url)
                                   (dom-text (car
                                              (dom-by-tag (libxml-parse-html-region
                                                           (point-min)
                                                           (point-max))
                                                          'title))))))))
          (t
           (call-interactively 'org-insert-link)))))

;;;###autoload
(defun +ansi-color-buffer ()
  (interactive)
  (with-silent-modifications
    (ansi-color-apply-on-region (point-min) (point-max))))

;;;###autoload
(defun +truncate-0x-hash ()
  "Truncates long 0x hash."
  (save-excursion
    (goto-char (match-beginning 0))
    (let ((start (re-search-forward "0x[a-fA-F0-9]\\{41\\}" nil t))
          (finish (re-search-forward "\\(\"\\|'\\)" (line-end-position) t)))
      (when (and start finish)
        (put-text-property (- start 29) (- finish 1) 'display "..."))))
  nil)

;;;###autoload
(defun ++notmuch-start ()
  (notmuch-search "tag:inbox" nil nil 1 nil))

;;;###autoload
(defun browse-url-mpv (url &optional single)
  (start-process "mpv" nil "mpv" (shell-quote-argument url)))

;;;###autoload
(defun =elfeed-dashboard ()
  "Activate (or switch to) `elfeed' in its workspace."
  (interactive)
  (if (modulep! :ui workspaces)
      (progn
        (+workspace-switch +rss-workspace-name t)
        (doom/switch-to-scratch-buffer)
        (elfeed-dashboard)
        (+workspace/display))
    (setq +rss--wconf (current-window-configuration))
    (delete-other-windows)
    (switch-to-buffer (doom-fallback-buffer))
    (elfeed-dashboard)))

;;;###autoload
(defun +ss ()
  (interactive))

;;;###autoload
(defun +sS ()
  (interactive))

;;;###autoload
(defun +lispy-modes-p ()
  (memq major-mode +lispy-modes))

;;;###autoload
(defun ++compile ()
  "Do compilaion in shell-mode with pwd"
  (interactive)
  (advice-add 'comint-send-invisible :around '+comint-send-invisible-with-sudo-pwd)
  (compilation-start compile-command t))

;;;###autoload
(defun ++password! (&rest args)
  (interactive)
  (require 'auth-source)
  (let ((match (car (auth-source-search :host "localhost" :user user-login-name))))
    (if match
        (let ((secret (plist-get match :secret)))
          (if (functionp secret)
              (funcall secret)
            secret))
      (error "Password not found for %S" params))))

;;;###autoload
(defun json->edn ()
  (interactive)
  (shell-command-on-region (region-beginning)
                           (region-end)
                           "jet --pretty --keywordize keyword --from json --to edn"
                           (current-buffer)
                           t))

;;;###autoload
(defun +lispy-special-p ()
  (when (and lispy-mode (memq major-mode '(clojure-mode clojurescript-mode clojurec-mode emacs-lisp-mode)))
    (or (lispy-left-p)
        (lispy-right-p)
        (string= (string (following-char)) "(")
        (string= (string (following-char)) ")"))))

;;;###autoload
(defun +cljr-project-has-dep? (dep-regex)
  (when (doom-project-root)
    (let ((s-matches-regex (-partial 's-matches? dep-regex)))
      (or
       (let ((dep-file (concat (doom-project-root) "deps.edn")))
         (and (f-exists? dep-file)
              (funcall s-matches-regex (f-read dep-file))))
       (let ((dep-file (concat (doom-project-root) "shadow-cljs.edn")))
         (and (f-exists? dep-file)
              (funcall s-matches-regex (f-read dep-file))))
       (let ((dep-file (concat (doom-project-root) "project.clj")))
         (and (f-exists? dep-file)
              (funcall s-matches-regex (f-read dep-file))))
       (let ((dep-file (concat (doom-project-root) "bb.edn")))
         (and (f-exists? dep-file)
              (funcall s-matches-regex (f-read dep-file))))))))

;;;###autoload
(defun +cljr--log-spy (prefix-info log! arg)
  (let* ((log-spy-str (if log! "log/spy!" "log/spy"))
         (log-spy-str (if prefix-info
                          (concat log-spy-str " :info")
                        log-spy-str)))
    (if (string= (symbol-at-point) log-spy-str)
        (progn
          (sp-kill-sexp)
          (paredit-splice-sexp))
      (progn (save-excursion
               (evil-emacs-state 1)
               (if (+lispy-special-p) (lispy-mark) (lispy-mark-symbol))
               (call-interactively 'lispy-parens)
               (unless (string= (string (following-char)) " ") (forward-char))
               (insert (if (string= (string (following-char)) " ") log-spy-str (concat log-spy-str " ")))
               (lispy-left 1)
               (evil-normal-state 1))
             (unless (+lispy-special-p) (lispy-left 1))))))

;;;###autoload
(defun log/--spy (arg)
  (interactive "P")
  (let ((log-spy-str "log/spy"))
    (if (eq (symbol-at-point) 'log/spy)
        (progn
          (sp-kill-sexp)
          (paredit-splice-sexp))
      (save-excursion
        (evil-emacs-state 1)
        (if (+lispy-special-p) (lispy-mark) (lispy-mark-symbol))
        (call-interactively 'lispy-parens)
        (unless (string= (string (following-char)) " ") (forward-char))
        (insert (if (string= (string (following-char)) " ") log-spy-str (concat log-spy-str " ")))
        (lispy-left 1)
        (evil-normal-state 1))))
  (unless (+lispy-special-p) (lispy-left 1)))

;;;###autoload
(defun +spy (arg)
  (interactive "P")
  (when (and lispy-mode (memq major-mode '(clojure-mode clojurescript-mode clojurec-mode)))
    (let* ((has-as-log? (ignore-errors (save-excursion (re-search-backward ":as log\\]"))))
           (telemere? (+cljr-project-has-dep? "com.taoensso/telemere"))
           (glogi? (+cljr-project-has-dep? "lambdaisland/glogi"))
           (timbre? (+cljr-project-has-dep? "timbre"))
           (pedestal? (+cljr-project-has-dep? "pedestal.log"))
           (tools-logging? (+cljr-project-has-dep? "tools.logging"))
           (f (apply-partially '+cljr--log-spy
                               (cond
                                (glogi? nil)
                                (telemere? nil)
                                (timbre? nil)
                                (pedestal? t)
                                nil)
                               (and telemere? (not glogi?)))))
      (cond
       (has-as-log? (funcall f arg))

       (glogi?
        (save-excursion
          (cljr--insert-in-ns ":require")
          (insert "[lambdaisland.glogi :as log]"))
        (funcall f arg))
       (telemere?
        (save-excursion
          (cljr--insert-in-ns ":require")
          (insert "[taoensso.telemere :as log]"))
        (funcall f arg))

       (timbre?
        (save-excursion
          (cljr--insert-in-ns ":require")
          (insert "[taoensso.timbre :as log]"))
        (funcall f arg))

       (pedestal?
        (save-excursion
          (cljr--insert-in-ns ":require")
          (insert "[io.pedestal.log :as log]"))
        (funcall f arg))

       (tools-logging?
        (save-excursion
          (cljr--insert-in-ns ":require")
          (insert "[clojure.tools.logging :as log]"))
        (funcall f arg)))))
  (when (and lispy-mode (memq major-mode '(emacs-lisp-mode)))
    (call-interactively 'log/--spy arg)))

;;;###autoload
(defun +clojure-clean-log-ns ()
  (interactive)
  (when (and lispy-mode (memq major-mode '(clojure-mode clojurescript-mode clojurec-mode)))
    (save-excursion
      (goto-char (point-max))
      (when (and
             (ignore-errors (save-excursion (re-search-backward ":as log\\]")))
             (not (ignore-errors (save-excursion (re-search-backward "(log/")))))
        (and (ignore-errors (re-search-backward "\\[taoensso.telemere :as log\\]")) (call-interactively #'lispyville-delete-whole-line))
        (goto-char (point-max))
        (and (ignore-errors (re-search-backward "\\[lambdaisland.glogi :as log\\]")) (call-interactively #'lispyville-delete-whole-line))
        (goto-char (point-max))
        (and (ignore-errors (re-search-backward "\\[taoensso.timbre :as log\\]")) (call-interactively #'lispyville-delete-whole-line))
        (goto-char (point-max))
        (and (ignore-errors (re-search-backward "\\[io.pedestal.log :as log\\]")) (call-interactively #'lispyville-delete-whole-line))))))

;; https://xenodium.com/png-to-icns-emacs-dwim-style/
;;;###autoload
(defun dwim-shell-command-convert-image-to-icns ()
  "Convert png to icns icon."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Convert png to icns icon"
   "
    # Based on http://stackoverflow.com/questions/12306223/how-to-manually-create-icns-files-using-iconutil
    # Note: png must be 1024x1024
    mkdir <<fne>>.iconset
    sips -z 16 16 '<<f>>' --out '<<fne>>.iconset/icon_16x16.png'
    sips -z 32 32 '<<f>>' --out '<<fne>>.iconset/icon_16x16@2x.png'
    sips -z 32 32 '<<f>>' --out '<<fne>>.iconset/icon_32x32.png'
    sips -z 64 64 '<<f>>' --out '<<fne>>.iconset/icon_32x32@2x.png'
    sips -z 128 128 '<<f>>' --out '<<fne>>.iconset/icon_128x128.png'
    sips -z 256 256 '<<f>>' --out '<<fne>>.iconset/icon_128x128@2x.png'
    sips -z 256 256 '<<f>>' --out '<<fne>>.iconset/icon_256x256@2x.png'
    sips -z 512 512 '<<f>>' --out '<<fne>>.iconset/icon_512x512.png'
    sips -z 512 512 '<<f>>' --out '<<fne>>.iconset/icon_256x256@2x.png'
    sips -z 1024 1024 '<<f>>' --out '<<fne>>.iconset/icon_512x512@2x.png'
    iconutil -c icns `<<fne>>.iconset'"
   :utils '("sips" "iconutil")
   :extensions "png"))

;;;###autoload
(defun +latest-modified-dir (path &optional filter-fn)
  (require 'f)
  (let* ((dirnames (f-directories path
                                  (or filter-fn 'identity)))
         (dirnames-with-attr (seq-map
                              (lambda (dirname)
                                (list dirname (float-time (file-attribute-modification-time (file-attributes dirname)))))
                              dirnames))
         (latest-dir (car
                      (car
                       (seq-sort
                        (lambda (dir-a dir-b)
                          (> (nth 1 dir-a)
                             (nth 1 dir-b)))
                        dirnames-with-attr)))))
    latest-dir))

;;;###autoload
(defun status-clear-current-app-data ()
  "Clear data for the latest apps running in the latest simulator

 based on last changed directory"
  (interactive)
  (require 'f)
  (require 's)
  (when IS-MAC
    (let* ((latest-device (+latest-modified-dir "~/Library/Developer/CoreSimulator/Devices/"
                                                (lambda (dirname)
                                                  (s-matches? "[A-Z0-9]\\{8\\}-[A-Z0-9]\\{4\\}-[A-Z0-9]\\{4\\}-[A-Z0-9]\\{4\\}-[A-Z0-9]\\{12\\}$" dirname))))
           (latest-appication (+latest-modified-dir
                               (concat latest-device "/data/Containers/Data/Application")
                               (lambda (dirname)
                                 (s-matches? "[A-Z0-9]\\{8\\}-[A-Z0-9]\\{4\\}-[A-Z0-9]\\{4\\}-[A-Z0-9]\\{4\\}-[A-Z0-9]\\{12\\}$" dirname))))
           (app-data (concat latest-appication "/Library")))
      (delete-directory app-data t t))))

;;;###autoload
(defun status-go-geth-log ()
  (interactive)
  (require 'f)
  (require 's)
  (when IS-MAC
    (let* ((latest-device (+latest-modified-dir "~/Library/Developer/CoreSimulator/Devices/"
                                                (lambda (dirname)
                                                  (s-matches? "[A-Z0-9]\\{8\\}-[A-Z0-9]\\{4\\}-[A-Z0-9]\\{4\\}-[A-Z0-9]\\{4\\}-[A-Z0-9]\\{12\\}$" dirname))))
           (latest-appication (+latest-modified-dir
                               (concat latest-device "/data/Containers/Data/Application")
                               (lambda (dirname)
                                 (s-matches? "[A-Z0-9]\\{8\\}-[A-Z0-9]\\{4\\}-[A-Z0-9]\\{4\\}-[A-Z0-9]\\{4\\}-[A-Z0-9]\\{12\\}$" dirname))))
           (eth-log (concat latest-appication "/Library/geth.log")))
      (find-file eth-log))))

;;;###autoload
(defun status-go-geth-logg ()
  (interactive)
  (require 'f)
  (require 's)
  (when IS-MAC
    (let* ((latest-device (+latest-modified-dir "~/Library/Developer/CoreSimulator/Devices/"
                                                (lambda (dirname)
                                                  (s-matches? "[A-Z0-9]\\{8\\}-[A-Z0-9]\\{4\\}-[A-Z0-9]\\{4\\}-[A-Z0-9]\\{4\\}-[A-Z0-9]\\{12\\}$" dirname))))
           (latest-appication (+latest-modified-dir
                               (concat latest-device "/data/Containers/Data/Application")
                               (lambda (dirname)
                                 (s-matches? "[A-Z0-9]\\{8\\}-[A-Z0-9]\\{4\\}-[A-Z0-9]\\{4\\}-[A-Z0-9]\\{4\\}-[A-Z0-9]\\{12\\}$" dirname))))
           (eth-log (concat
                     (+latest-modified-dir
                      (concat
                       (+latest-modified-dir
                        (concat latest-appication "/Library/Users/" user-login-name "/Library/Developer/CoreSimulator/Devices/")
                        (lambda (dirname)
                          (s-matches? "[A-Z0-9]\\{8\\}-[A-Z0-9]\\{4\\}-[A-Z0-9]\\{4\\}-[A-Z0-9]\\{4\\}-[A-Z0-9]\\{12\\}$" dirname)))
                       "/data/Containers/Data/Application/")
                      (lambda (dirname)
                        (s-matches? "[A-Z0-9]\\{8\\}-[A-Z0-9]\\{4\\}-[A-Z0-9]\\{4\\}-[A-Z0-9]\\{4\\}-[A-Z0-9]\\{12\\}$" dirname)))
                     "/Library")))
      (find-file eth-log))))

;;;###autoload
(defun +magit-toggle-performance ()
  (interactive)
  (require 'magit)
  (require 'magit-autorevert)
  (if magit-refresh-verbose
      (progn (setq!
              magit-refresh-status-buffer t
              magit-refresh-verbose nil
              auto-revert-buffer-list-filter nil
              magit-diff-highlight-indentation nil
              magit-diff-highlight-trailing t
              magit-diff-highlight-keywords t
              magit-diff-highlight-hunk-body t
              magit-diff-paint-whitespace t
              magit-diff-paint-whitespace-lines t
              magit-diff-refine-hunk t
              magit-revision-insert-related-refs 'mixed
              magit-section-visibility-indicator '(magit-fringe-bitmap> . magit-fringe-bitmapv)
              magit-revision-use-hash-sections 'quicker
              magit-diff-expansion-threshold 20)
             (pushnew! vc-handled-backends 'Git)
             (add-hook! 'magit-refs-sections-hook 'magit-insert-tags)
             (add-hook! 'server-switch-hook 'magit-commit-diff)
             (message "exit magit highperf"))
    (progn (setq!
            magit-refresh-status-buffer nil
            magit-refresh-verbose t
            auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffer-p
            magit-diff-highlight-indentation nil
            magit-diff-highlight-trailing nil
            magit-diff-highlight-keywords nil
            magit-diff-highlight-hunk-body nil
            magit-diff-paint-whitespace-lines nil
            magit-diff-paint-whitespace nil
            magit-diff-refine-hunk nil
            magit-revision-insert-related-refs nil
            vc-handled-backends nil
            magit-section-visibility-indicator nil
            magit-revision-use-hash-sections nil
            magit-diff-expansion-threshold 0.01)
           (delq! 'Git vc-handled-backends)
           (remove-hook! 'magit-refs-sections-hook 'magit-insert-tags)
           (remove-hook! 'server-switch-hook 'magit-commit-diff)
           (setq! magit-git-debug nil)
           (message "enter magit highperf"))))

;;;###autoload
(defun +doom/toggle-line-numbers ()
  "Toggle line numbers.

Cycles through regular, relative and no line numbers. The order depends on what
`display-line-numbers-type' is set to. If you're using Emacs 26+, and
visual-line-mode is on, this skips relative and uses visual instead.

See `display-line-numbers' for what these values mean."
  (interactive)
  (defvar doom--line-number-style display-line-numbers-type)
  (let* ((styles `(t ,(if visual-line-mode 'visual t) nil))
         (order (cons display-line-numbers-type (remq display-line-numbers-type styles)))
         (queue (memq doom--line-number-style order))
         (next (if (= (length queue) 1)
                   (car order)
                 (car (cdr queue)))))
    (setq doom--line-number-style next)
    (setq display-line-numbers next)
    (message "Switched to %s line numbers"
             (pcase next
               (`t "normal")
               (`nil "disabled")
               (_ (symbol-name next))))))

;;;###autoload
(defun ++projectile-switch-project-and-rename-workspace ()
  "Switch to a project's magit-status buffer and prompt for new workspace name

This is for per workspace each task setup"
  (interactive)
  (require 'projectile)
  (let ((projectile-current-project-on-switch 'keep)
        (+workspaces-switch-project-function #'magit-status))
    (call-interactively #'projectile-switch-project)))

;;;###autoload
(defun clerk-show ()
  (interactive)
  (when-let
      ((filename
        (buffer-file-name)))
    (save-buffer)
    (cider-interactive-eval
     (concat "(nextjournal.clerk/show! \"" filename "\")"))))

;;;###autoload
(defun +eat ()
  (interactive)
  (let* ((target-eat-buffer-name (if current-prefix-arg
                                     (format "*eat*<%s>" current-prefix-arg)
                                   "*eat*"))
         (target-eat-buffer (get-buffer target-eat-buffer-name))
         (target-eat-buffer-window (and target-eat-buffer (get-buffer-window target-eat-buffer-name))))
    (if target-eat-buffer-window
        (select-window target-eat-buffer-window)
      (progn
        (evil-window-vsplit)
        (eat shell-file-name current-prefix-arg)))))

;; im-tap from https://isamert.net/2023/08/14/elisp-editing-development-tips.html#im-tap
;;;###autoload
(defmacro log/spy (form)
  "Evaluate FORM and return its result.
Additionally, print a message to the *Messages* buffer showing
the form and its result.

This macro is useful for debugging and inspecting the
intermediate results of Elisp code without changing your code
structure. Just wrap the form with `im-tap' that you want to see
it's output without introducing an intermediate let-form."
  `(let ((result ,form))
     (message "[spy :: %s] → %s" ,(prin1-to-string form) result)
     result))

;;;###autoload
(defun log/debug (thing)
  "Like `im-tap' but uses `pp-display-expression' to display the
result instead of `message'."
  (pp-display-expression thing "*im-debug*")
  thing)

(defun ++clojure-keywordp (s)
  (and
   (stringp s)
   (s-starts-with? ":" s)
   (not (s-contains? " " s))))

;;;###autoload
(defun +lookup-status-mobile-re-frame-event-handler-defination (_thing)
  (when (++clojure-keywordp _thing)
    (project-search (regexp-quote (concat "{:events [" _thing "]}"))))
  t)

;; (setq-local +lookup-definition-functions '(+lookup-status-mobile-re-frame-event-handler-defination))

;;;###autoload
(defun +stm-reload ()
  (interactive)
  (require 'seq)
  (require 'cider)
  (let ((buf (seq-find
              (lambda (b) (eq (buffer-local-value 'major-mode b) 'clojurescript-mode))
              (projectile-project-buffers (expand-file-name "~/workspace/office/status-mobile")))))
    (when buf
      (with-current-buffer buf
        (when (and (cider-connected-p) (cider-current-repl 'cljs))
          (cider-interactive-eval "(status-im.setup.hot-reload/reload)" nil nil (cider--nrepl-pr-request-map)))))))

;;;###autoload
(defun +status-start-sessions ()
  (interactive)
  (let ((default-directory (expand-file-name "~/workspace/office/status-mobile"))
        (async-shell-command-display-buffer nil))
    (when (s-blank-p (shell-command-to-string "lsof -i :3449"))
      (detached-shell-command "make run-clojure" t))
    (when (s-blank-p (shell-command-to-string "lsof -i :4567"))
      (detached-shell-command "make run-re-frisk" t))
    (when (s-blank-p (shell-command-to-string "lsof -i :8081"))
      (detached-shell-command "make run-metro" t))))

;;;###autoload
(defun +status-start-session-lint-fix ()
  (interactive)
  (let ((default-directory (expand-file-name "~/workspace/office/status-mobile"))
        (async-shell-command-display-buffer nil))
    (detached-shell-command "make lint-fix" t)))

;;;###autoload
(defun +status-start-session-lint ()
  (interactive)
  (let ((default-directory (expand-file-name "~/workspace/office/status-mobile"))
        (async-shell-command-display-buffer nil))
    (detached-shell-command "make lint" t)))

;;;###autoload
(defun +whisper-insert ()
  (interactive)
  (whisper-run)
  (if (y-or-n-p "Trans?")
      (progn
        (whisper-run)
        (when (and (boundp #'gptel-mode) gptel-mode)
          (if (y-or-n-p "Query?")
              (progn (goto-char (point-max))
                     (gptel-send))
            (goto-char (point-max)))))
    (when (process-live-p whisper--recording-process)
      (kill-process whisper--recording-process))))

;;;###autoload
(defun +whisper-run ()
  (interactive)
  (cond
   ((eq current-prefix-arg 1)
    (+whisper-zh-lang-model))
   ((not current-prefix-arg)
    (+whisper-default-lang-model)))
  (call-interactively '+whisper-insert))

;;;###autoload
(defun +chat-with-ai ()
  (interactive)
  (cond
   ((and current-prefix-arg (and (boundp #'gptel-mode) gptel-mode))
    (progn
      (call-interactively #'gptel)
      (when-let ((buf (get-buffer gptel-default-session)))
        (with-current-buffer buf
          (call-interactively #'+whisper-run)))))
   ((and (boundp #'gptel-mode) gptel-mode)
    (call-interactively #'+whisper-run))
   (t
    (progn
      (call-interactively #'gptel)
      (when-let ((buf (get-buffer gptel-default-session)))
        (with-current-buffer buf
          (call-interactively #'+whisper-run)))))))

(defvar +xcrun-devices-history nil)

;;;###autoload
(defun +stm-install-ios-build (devices)
  (interactive
   (let ((devices (seq-reduce
                   (lambda (acc v)
                     (a-assoc-1 acc (a-get v "name") v))
                   (+xcrun-devices)
                   (a-hash-table))))
     (list (completing-read-multiple "Devices: " devices nil nil nil '+xcrun-devices-history))))
  (let ((device-ids (seq-reduce (lambda (acc v)
                                  (if (seq-contains-p devices (a-get v "name"))
                                      (cons (a-get v "udid") acc)
                                    acc))
                                (+xcrun-devices)
                                nil))
        (app-path (if (boundp '+xcrun-install-app-path)
                      +xcrun-install-app-path
                    (concat (projectile-project-root) "build/Build/Products/Debug-iphonesimulator/StatusIm.app"))))
    (seq-doseq (id device-ids)
      (async-shell-command
       (format "/usr/bin/open -a Simulator --args -CurrentDeviceUDID %s && /usr/bin/xcrun simctl install %s %s" id id app-path)))))

;;;###autoload
(defun +cider-project-reload-exec ()
  (interactive)
  (require 'seq)
  (require 'cider)
  (when (and
         (boundp '+cider-project-reload-exec-cmd-clj)
         (cider-connected-p)
         (cider-current-repl 'clj))
    (cider-interactive-eval +cider-project-reload-exec-cmd-clj nil nil (cider--nrepl-pr-request-map)))
  (when (and
         (boundp '+cider-project-reload-exec-cmd-cljs)
         (cider-connected-p)
         (cider-current-repl 'cljs))
    (cider-interactive-eval +cider-project-reload-exec-cmd-cljs nil nil (cider--nrepl-pr-request-map))))

;;;###autoload
(defun +cider-inspect-tap ()
  "View taps queue."
  (interactive)
  (cider-inspect-expr "(view!)" "queue"))

;;;###autoload
(defun +terminal-here ()
  (interactive)
  (call-process-shell-command
   (format! "tmux new-window -n '%s' -c '%s'"
            (or buffer-file-name default-directory)
            (if buffer-file-name
                (file-name-directory buffer-file-name)
              default-directory))
   nil 0)
  (call-process-shell-command "open -a kitty.app" nil 0))
