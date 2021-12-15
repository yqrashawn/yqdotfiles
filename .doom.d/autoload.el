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
      (message "Turn off company-tabnine"))))

;;;###autoload
;; (defun +tree-sitter-manybe-enable ()
;;   (require 'tree-sitter-langs)
;;   ;; Activate tree-sitter's improved syntax highlighting only if we are
;;   ;; using a major-mode that has a compatible tree-sitter syntax parser
;;   (if (and (boundp 'tree-sitter-major-mode-language-alist)
;;            (assq major-mode tree-sitter-major-mode-language-alist))
;;       (progn
;;         (tree-sitter-mode)
;;         (tree-sitter-hl-mode))))

;;;###autoload
(defun +company-complete (&optional args)
  (interactive)
  (if (eq major-mode 'vterm-mode)
      (vterm--self-insert)
    (if (and company-mode (eq (preceding-char) ?,))
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
(defun yq/vterm-toggle (arg)
  (interactive "P")
  (if (display-graphic-p)
      (and (featurep! :term vterm) (+vterm/toggle (not arg)))
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