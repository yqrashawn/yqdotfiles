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
(defun corgi/cider-eval-last-sexp-and-replace ()
  "Alternative to cider-eval-last-sexp-and-replace, but kills
clojure logical sexp instead of ELisp sexp, and pprints the
result."
  (interactive)
  (let ((last-sexp (cider-last-sexp)))
    ;; we have to be sure the evaluation won't result in an error
    (cider-nrepl-sync-request:eval last-sexp)
    ;; seems like the sexp is valid, so we can safely kill it
    (let ((opoint (point)))
      (clojure-backward-logical-sexp)
      (kill-region (point) opoint))
    (cider-interactive-eval last-sexp
                            (cider-eval-pprint-with-multiline-comment-handler
                             (current-buffer)
                             (set-marker (make-marker) (point))
                             ""
                             " "
                             "")
                            nil
                            (cider--nrepl-print-request-map fill-column))))

;;;###autoload
(defun corgi/cider-pprint-eval-last-sexp-insert ()
    (interactive)
    (let ((cider-comment-prefix "")
          (cider-comment-continued-prefix " ")
          (cider-comment-postfix ""))
      (cider-pprint-eval-last-sexp-to-comment)))

;;;###autoload
(defun corgi/cider-pprint-register (register)
  "Evaluate a Clojure snippet stored in a register.
Will ask for the register when used interactively. Put `#_clj' or
`#_cljs' at the start of the snippet to force evaluation to go to
a specific REPL type, no matter the mode (clojure-mode or
clojurescript-mode) of the current buffer."
  (interactive (list (register-read-with-preview "Eval register: ")))
  (let ((reg (get-register register)))
    (cond
      ((string-match-p "^#_cljs" reg)
        (with-current-buffer (car (cider-repls 'cljs))
          (cider--pprint-eval-form reg)))
      ((string-match-p "^#_clj" reg)
        (with-current-buffer (car (cider-repls 'clj))
          (cider--pprint-eval-form reg)))
      (t
        (cider--pprint-eval-form reg)))))

;;;###autoload
(defun corgi/cider-quit-all ()
  "Quit all current CIDER REPLs."
  (interactive)
  (require 'cider)
  (let ((repls (seq-remove (lambda (r)
                             (equal r (get-buffer "*babashka-repl*")))
                           (seq-mapcat #'cdr (sesman-current-sessions 'CIDER)))))
    (seq-do #'cider--close-connection repls))
  ;; if there are no more sessions we can kill all ancillary buffers
  (cider-close-ancillary-buffers)
  ;; need this to refresh sesman browser
  (run-hooks 'sesman-post-command-hook))

;;;###autoload
(defun corgi/cider-pprint-eval-register (register)
  "Evaluate a Clojure snippet stored in a register.

Will ask for the register when used interactively. Put `#_clj' or
`#_cljs' at the start of the snippet to force evaluation to go to
a specific REPL type, no matter the mode (clojure-mode or
clojurescript-mode) of the current buffer.

You can use {{...}} to insert emacs-lisp code that willg get
evaluated, like `(println \"{{buffer-file-name}}\")'.
"
  (interactive (list (register-read-with-preview "Eval register: ")))
  (let ((reg (replace-regexp-in-string
              "{{\\([^}]+\\)}}"
              (lambda (s)
                (eval
                 (read
                  (match-string 1 s))))
              (get-register register))))
    (cond
     ((string-match-p "^#_cljs" reg)
      (with-current-buffer (car (cider-repls 'cljs))
        (cider--pprint-eval-form reg)))
     ((string-match-p "^#_clj" reg)
      (with-current-buffer (car (cider-repls 'clj))
        (cider--pprint-eval-form reg)))
     (t
      (cider--pprint-eval-form reg)))))

;;;###autoload
(defun corgi/cider-jack-in-babashka (&optional project-dir)
  "Start a utility CIDER REPL backed by Babashka, not related to a
specific project."
  (interactive)
  (require 'cider)
  (let ((project-dir (or project-dir user-emacs-directory)))
    (nrepl-start-server-process
     project-dir
     "bb --nrepl-server 0"
     (lambda (server-buffer)
       (cider-nrepl-connect
        (list :repl-buffer server-buffer
              :repl-type 'clj
              :host (plist-get nrepl-endpoint :host)
              :port (plist-get nrepl-endpoint :port)
              :project-dir project-dir
              :session-name "babashka"
              :repl-init-function (lambda ()
                                    (setq-local cljr-suppress-no-project-warning t
                                                cljr-suppress-middleware-warnings t)
                                    (rename-buffer "*babashka-repl*"))))))))

(defun corgi/cider-modeline-info ()
  (when (derived-mode-p 'clojure-mode)
    (let ((source-project-name (projectile-project-name)))
      (if-let* ((repls (ignore-errors (cider-repls (cider-repl-type-for-buffer)))))
          (thread-last
            repls
            (seq-map
             (lambda (repl)
               (with-current-buffer repl
                 (if (equal (buffer-name repl) "*babashka-repl*")
                     (propertize "bb" 'face '(:background "green"
                                              :foreground "black"))
                   (let ((info (concat
                                (when-let ((repl-project-name (cider--project-name nrepl-project-dir)))
                                  (when (not (equal repl-project-name source-project-name))
                                    (concat ":" repl-project-name)))
                                (pcase (plist-get nrepl-endpoint :host)
                                  ("localhost" "")
                                  ("127.0.0.1" "")
                                  (x (concat ":" x)))
                                ;;(format ":%d" (plist-get nrepl-endpoint :port))
                                )))
                     (cl-case cider-repl-type
                       (clj (propertize (concat "clj" info) 'face '(:background "#5881D8"
                                                                    :foreground "white")))
                       (cljs (propertize (concat "cljs" info) 'face '(:background "#f7df1e"
                                                                      :foreground "black")))
                       (pending-cljs (propertize (concat "pending-cljs" info) 'face '(:background "#f7df1e"
                                                                                      :foreground "black")))))))))
            (s-join " "))
        (propertize "<not connected>" 'face '(:background "red"
                                              :foreground "white"))))))

(defun corgi/enable-cider-connection-indicator-in-current-buffer ()
  (when (not (seq-find (lambda (e) (eq e '(:eval (corgi/cider-modeline-info)))) mode-line-format))
    (setq mode-line-format
          (seq-mapcat
           (lambda (e)
             (if (eq 'mode-line-modes e)
                 '(" " (:eval (corgi/cider-modeline-info)) " " mode-line-modes)
               (list e)))
           mode-line-format))))

;;;###autoload
(defun corgi/enable-cider-connection-indicator ()
  "In Clojure buffers show an indicator in the modeline for which
CIDER REPLs the current buffer is linked to, with color coding
for clj/cljs/bb, and extra info if the link goes to a different
project or host."
  (interactive)
  (add-hook 'clojure-mode-hook #'corgi/enable-cider-connection-indicator-in-current-buffer)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq 'clojure-mode major-mode)
        (corgi/enable-cider-connection-indicator-in-current-buffer)))))

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
  (interactive)
  (when 'symex-editing-mode
    (call-interactively #'symex-replace)))

;;;###autoload
(defun +sS ()
  (interactive)
  (when 'symex-editing-mode
    (call-interactively #'symex-change-delimiter)))

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
  (when (and lispy-mode (memq major-mode '(clojure-mode clojurescript-mode clojurec-mode)))
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
(defun +cljr--log-spy (arg)
  (interactive "P")
  (let ((log-spy-str (if (and (boundp '+cljr--log-spy-with-error)
                           +cljr--log-spy-with-error)
                       "log/spy :info"
                       "log/spy")))
    (save-excursion
      (evil-emacs-state 1)
      (if (+lispy-special-p) (lispy-mark) (lispy-mark-symbol))
      (call-interactively 'lispy-parens)
      (unless (string= (string (following-char)) " ") (forward-char))
      (insert (if (string= (string (following-char)) " ") log-spy-str  (concat log-spy-str " ")))
      (lispy-left 1)
      (evil-normal-state 1)))
  (unless (+lispy-special-p) (lispy-left 1)))

;;;###autoload
(defun +cljr-log-spy (arg)
  (interactive "P")
  (when (and lispy-mode (memq major-mode '(clojure-mode clojurescript-mode clojurec-mode)))
    (let ((has-as-log? (ignore-errors (save-excursion (re-search-backward ":as log\\]"))))
          (glogi? (+cljr-project-has-dep? "lambdaisland/glogi"))
          (timbre? (+cljr-project-has-dep? "timbre"))
          (pedestal? (+cljr-project-has-dep? "pedestal.log"))
          (tools-logging? (+cljr-project-has-dep? "tools.logging")))
      (cond
       (has-as-log? (call-interactively '+cljr--log-spy arg))

       (glogi?
        (save-excursion
          (cljr--insert-in-ns ":require")
          (insert "[lambdaisland.glogi :as log]"))
        (call-interactively '+cljr--log-spy arg))

       (timbre?
        (save-excursion
          (cljr--insert-in-ns ":require")
          (insert "[taoensso.timbre :as log]"))
        (call-interactively '+cljr--log-spy arg))

       (pedestal?
        (save-excursion
          (cljr--insert-in-ns ":require")
          (insert "[io.pedestal.log :as log]"))
        (call-interactively '+cljr--log-spy arg))

       (tools-logging?
        (save-excursion
          (cljr--insert-in-ns ":require")
          (insert "[clojure.tools.logging :as log]"))
        (call-interactively '+cljr--log-spy arg))))))

;;;###autoload
(defun +clojure-clean-log-ns ()
  (interactive)
  (when (and lispy-mode (memq major-mode '(clojure-mode clojurescript-mode clojurec-mode)))
    (save-excursion
      (goto-char (point-max))
      (when (and
             (ignore-errors (save-excursion (re-search-backward ":as log\\]")))
             (not (ignore-errors (save-excursion (re-search-backward "(log/")))))
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
(defun +remove-clojure-in-apheleia-mode-alist ()
  (setq! apheleia-mode-alist
        (seq-filter (lambda (a) (not (or (eq (car a) 'clojurescript-mode)
                                       (eq (car a) 'clojure-mode)
                                       (eq (car a) 'clojurec-mode))))
          apheleia-mode-alist)))

;;;###autoload
(defun +toggle-zprint-as-clojure-formatter ()
  (interactive)
  (if (seq-some (lambda (a) (eq (cdr a) 'zprint)) apheleia-mode-alist)
      (progn
        (+remove-clojure-in-apheleia-mode-alist)
        (pushnew! apheleia-mode-alist
          '(clojure-mode . cljfmt)
          '(clojurec-mode . cljfmt)
          '(clojurescript-mode . cljfmt))
        (message "Using cljfmt"))
    (progn
      (+remove-clojure-in-apheleia-mode-alist)
      (pushnew! apheleia-mode-alist
        '(clojure-mode . zprint)
        '(clojurec-mode . zprint)
        '(clojurescript-mode . zprint))
      (message "Using zprint"))))

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
    (call-interactively #'projectile-switch-project)
    (call-interactively #'+workspace/rename)))

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
