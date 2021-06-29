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
  (if (and (= 1 (length (window-list)))
           (assoc ?_ register-alist))
      (jump-to-register ?_)
    (progn
      (window-configuration-to-register ?_)
      (delete-other-windows))))

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
      (counsel-find-file rel-fname)))

;;;###autoload
(defun counsel-imenu-comments ()
  "Imenu display comments."
  (interactive)
  (let* ((imenu-create-index-function 'evilnc-imenu-create-index-function))
    (counsel-imenu)))

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
_g_  gfm      _m_ markdown
"
  ("e" emacs-lisp-mode :exit t)
  ("j" js2-mode :exit t)
  ("c" clojure-mode :exit t)
  ("T" text-mode :exit t)
  ("t" typescript-mode :exit t)
  ("f" fundamental-mode :exit t)
  ("m" markdown-mode :exit t)
  ("g" gfm-mode :exit t)
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
        (setq company-idle-delay 0)
        (setq company-tabnine--disabled nil)
        (message "Turn on company-tabnine"))
    (progn
      (setq company-idle-delay 0.2)
      (setq company-tabnine--disabled t)
      (message "Turn off company-tabnine"))))