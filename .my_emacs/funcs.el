;;; Code:
(defun yq/what-major-mode ()
  "Reveal current major mode."
  (interactive)
  (message "%s" major-mode))



(defun my/what-face (point)
  "Reveal face at POINT."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" point))))
;; word-count
(defun word-count nil "Count words in buffer" (interactive)
       (shell-command-on-region (point-min) (point-max) "wc -w"))

(defun count-words (start end)
  "Print number of words in the region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (count-matches "\\sw+"))))

(defun dcaps-to-scaps ()
  "Convert word in DOuble CApitals to Single Capitals."
  (interactive)
  (and (= ?w (char-syntax (char-before)))
       (save-excursion
         (and (if (called-interactively-p 'interactive)
                  (skip-syntax-backward "w")
                (= -3 (skip-syntax-backward "w")))
              (let (case-fold-search)
                (looking-at "\\b[[:upper:]]\\{2\\}[[:lower:]]"))
              (capitalize-word 1)))))

(define-minor-mode dubcaps-mode
  "Toggle `dubcaps-mode'.  Converts words in DOuble CApitals to
Single Capitals as you type."
  :init-value nil
  :lighter ("")
  (if dubcaps-mode
      (add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)
    (remove-hook 'post-self-insert-hook #'dcaps-to-scaps 'local)))

(add-hook 'text-mode-hook #'dubcaps-mode)

(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))
(bind-key "C-c d" 'delete-file-and-buffer)



(defun align-whitespace (start end)
  "Align columns by whitespace"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t))



(defun edit-current-file-as-root ()
  "Edit as root the file associated with the current buffer"
  (interactive)
  (if (buffer-file-name)
      (progn
        (setq file (concat "/sudo:root@localhost:" (buffer-file-name)))
        (find-file file))
    (message "Buffer is not associated to a file.")))



(defun move-file (new-location)
  "Write this file to NEW-LOCATION, and delete the old one."
  (interactive (list (if buffer-file-name
                         (read-file-name "Move file to: ")
                       (read-file-name "Move file to: "
                                       default-directory
                                       (expand-file-name (file-name-nondirectory (buffer-name))
                                                         default-directory)))))
  (when (file-exists-p new-location)
    (delete-file new-location))
  (let ((old-location (buffer-file-name)))
    (write-file new-location t)
    (when (and old-location
               (file-exists-p new-location)
               (not (string-equal old-location new-location)))
      (delete-file old-location))))

;; start directory
(defvar my/move-file-here-start-dir (expand-file-name "~/Downloads"))

(defun my/move-file-here ()
  "Move file from somewhere else to here.

The file is taken from a start directory set by
`my/move-file-here-start-dir' and moved to the current directory
if invoked in dired, or else the directory containing current
buffer. The user is presented with a list of files in the start
directory, from which to select the file to move, sorted by most
recent first.

The short filename is copied to clipboard.

Quickly move a file to the current directory | Pragmatic Emacs
http://pragmaticemacs.com/emacs/quickly-move-a-file-to-the-current-directory/
"
  (interactive)
  (let (file-list target-dir file-list-sorted start-file start-file-full)
    ;; clean directories from list but keep times
    (setq file-list
          (-remove (lambda (x) (nth 1 x))
                   (directory-files-and-attributes my/move-file-here-start-dir)))

    ;; get target directory
    ;; http://ergoemacs.org/emacs/emacs_copy_file_path.html
    (setq target-dir
          (if (equal major-mode 'dired-mode)
              (expand-file-name default-directory)
            (if (null (buffer-file-name))
                (user-error "ERROR: current buffer is not associated with a file.")
              (file-name-directory (buffer-file-name)))))

    ;; sort list by most recent
    ;; http://stackoverflow.com/questions/26514437/emacs-sort-list-of-directories-files-by-modification-date
    (setq file-list-sorted
          (mapcar #'car
                  (sort file-list
                        #'(lambda (x y) (time-less-p (nth 6 y) (nth 6 x))))))

    ;; use ivy to select start-file
    (setq start-file (ivy-read
                      (concat "Move selected file to " target-dir ":")
                      file-list-sorted
                      :re-builder #'ivy--regex
                      :sort nil
                      :initial-input nil))

    ;; add full path to start file and end-file
    (setq start-file-full
          (expand-file-name start-file my/move-file-here-start-dir))
    (setq end-file
          (expand-file-name (file-name-nondirectory start-file) target-dir))
    (rename-file start-file-full end-file)
    ;; copy short filename to clipboard
    (kill-new start-file)
    (gui-set-selection 'PRIMARY start-file)
    (message "moved %s to %s" start-file-full end-file)))

(defalias 'mfh 'my/move-file-here "move file from download directory to cwd")



(defun endless/visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (let ((repo (magit-get "remote" (magit-get-tracked-remote) "url")))
    (if (not repo)
        (setq repo (magit-get "remote" (magit-get-push-remote) "url")))
    (if (string-match "github\\.com" repo)
        (visit-gh-pull-request repo)
      (visit-bb-pull-request repo))))

(defun visit-gh-pull-request (repo)
  "Visit the current branch's PR on Github."
  (interactive)
  (message repo)
  (browse-url
   (replace-regexp-in-string
    "\\(\\.git\\)" "/pulls"
    repo)))

;; old code
;; (defun visit-gh-pull-request (repo)
;;   "Visit the current branch's PR on Github."
;;   (interactive)
;;   (message repo)
;;   (browse-url
;;    (replace-regexp-in-string
;;     "\\(\\.git\\)" "/pulls"
;;     repo)
;;    (format "https://github.com/%s/pull/new/%s"
;;            (replace-regexp-in-string
;;             "\\`.+github\\.com:\\(.+\\)\\(\\.git\\)?\\'" "\\1"
;;             repo)
;;            (magit-get-current-branch))))
;; (setq repo "https://github.com/yqrashawn/yqdotfiles.git")

;; Bitbucket pull requests are kinda funky, it seems to try to just do the
;; right thing, so there's no branches to include.
;; https://bitbucket.org/<username>/<project>/pull-request/new
(defun visit-bb-pull-request (repo)
  (message repo)
  (browse-url
   (format "https://bitbucket.org/%s/pull-request/new?source=%s&t=1"
           (replace-regexp-in-string
            "\\`.+bitbucket\\.org:\\(.+\\)\\.git\\'" "\\1"
            repo)
           (magit-get-current-branch))))

(eval-after-load "magit"
  '(magit-define-popup-action 'magit-remote-popup ?V "View PR" #'endless/visit-pull-request-url))



;; as tower C-u SPC-g-s
(eval-after-load "projectile"
  '(setq magit-repository-directories
         (mapcar #'directory-file-name
                 (cl-remove-if-not
                  (lambda (project)
                    (file-directory-p (concat project "/.git/")))
                  (projectile-relevant-known-projects)))
         magit-repository-directories-depth 1))



;; Get current system's name
(defun my-insert-system-name()
  (interactive)
  "Get current system's name"
  (insert (format "%s" system-name)))


;; Get current system type
(defun my-insert-system-type()
  (interactive)
  "Get current system type"
  (insert (format "%s" system-type)))


;; Check if system is Darwin/Mac OS X
(defun my-system-type-is-darwin ()
  "Return true if system is darwin-based (Mac OS X)"
  (string-equal system-type "darwin"))


;; Check if system is Microsoft Windows
(defun my-system-type-is-windows ()
  "Return true if system is Windows-based (at least up to Win7)"
  (string-equal system-type "windows-nt"))

(when (my-system-type-is-darwin)
  (defun locate-make-mdfind-command-line (search-string)
    (list "mdfind" (concat "kMDItemDisplayName=*" search-string "*")))
  (defun spotlight ()
    "Search for files by name using spotlight"
    (interactive)
    (let ((locate-command "mdfind")
          (locate-make-command-line 'locate-make-mdfind-command-line))
      (call-interactively 'locate nil)))
  (defun spotlight-full ()
    "Search using spotlight"
    (interactive)
    (let ((locate-command "mdfind"))
      (call-interactively 'locate nil))))



;; http://www.wilfred.me.uk/.emacs.d/init.html#org1752acf
(defun yq/start-scratch-html-file (file-name)
  "Create a test HTML file in ~/Downloads/scratch/FILE-NAME to play around with."
  (interactive "sName of scratch HTML file: ")
  (yq/start-scratch-file (format "%s.html" file-name))
  (erase-buffer)
  (insert "<!DOCTYPE html>
<html>
    <head>
        <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">
        <title>
        </title>
        <style type=\"text/css\">
        </style>
    </head>
    <body>

    </body>
</html>")
  (forward-line -2)
  (move-end-of-line nil))

(defun start--file (path)
  "Create a file at PATH, creating any containing directories as necessary.
Visit the file after creation."
  (make-directory (file-name-directory path) t)
  (find-file path))

(defun yq/start-scratch-file (file-name)
  "Create a file in ~/scratch for the given FILE-NAME."
  (interactive "sName of scratch file: ")
  (start--file (expand-file-name (format "~/Downloads/scratch/%s" file-name))))

;; (defun ivy-insert-org-entity ()
;;   "Insert an org-entity using ivy."
;;   (interactive)
;;   (ivy-read "Entity: " (loop for element in (append org-entities org-entities-user)
;;                              when (not (stringp element))
;;                              collect
;;                              (cons
;;                               (format "%10s | %s | %s | %s"
;;                                       (car element) ;name
;;                                       (nth 1 element) ; latex
;;                                       (nth 3 element) ; html
;;                                       (nth 6 element)) ;utf-8
;;                               element))
;;             :require-match t
;;             :action '(1
;;                       ("u" (lambda (element) (insert (nth 6 (cdr element)))) "utf-8")
;;                       ("o" (lambda (element) (insert "\\" (cadr element))) "org-entity")
;;                       ("l" (lambda (element) (insert (nth 1 (cdr element)))) "latex")
;;                       ("h" (lambda (element) (insert (nth 3 (cdr element)))) "html"))))



(defun duplicate-line ()
  "Duplicate current line."
  (interactive)
  (kill-whole-line)
  (yank)
  (yank))



(defun untabify-all ()
  "Untabify the current buffer, unless `untabify-this-buffer' is nil."
  (interactive)
  (untabify (point-min) (point-max)))

(define-key global-map (kbd "H-i") 'open-terminal-here)



(defun open-terminal-here ()
  "Go to present working dir and focus iterm"
  (interactive)
  (do-applescript
   (concat
    " tell application \"iTerm2\"\n"
    "   tell the current session of current window\n"
    (format "     write text \"cd %s\" \n"
            ;; string escaping madness for applescript
            (replace-regexp-in-string
             "\\\\" "\\\\\\\\"
             (shell-quote-argument (or default-directory "~"))))
    "   end tell\n"
    " end tell\n"
    " do shell script \"open -a iTerm\"\n")))



(defun yqrashawn-copy-file-name-to-clipboard ()
  "Put the current file name into the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))



(defun xah-css-insert-random-color-hsl ()
  "Insert a random color string of CSS HSL format.
Sample output: hsl(100,24%,82%);
URL `http://ergoemacs.org/emacs/emacs_CSS_colors.html'
Version 2015-06-11"
  (interactive)
  (insert (format "hsl(%d,%d%%,%d%%);" (random 360) (random 100) (random 100))))

(use-package helpful
  :init (progn
          ;; (spacemacs/set-leader-keys "hdv" 'helpful-variable)
          ;;      (spacemacs/set-leader-keys "hdf" 'helpful-function)
          ;;      (spacemacs/set-leader-keys "hdo" 'helpful-at-point)
          ;;      (spacemacs/set-leader-keys "hdk" 'helpful-key)
          ;;      (spacemacs/set-leader-keys "hdj" 'helpful-command)
          ;;      (spacemacs/set-leader-keys "hdh" 'helpful-callable)
          ;;      (spacemacs/set-leader-keys "hdi" 'helpful-symbol)
          (evil-define-key 'normal helpful-mode-map "q" 'kill-this-buffer)))



;; (use-package beacon
;;   :diminish ""
;;   :config
;;   (setq beacon-color "firebrick")
;;   (setq beacon-size 20)         ; smaller than default 40
;;   (setq beacon-blink-delay 0.1) ; faster than default 0.3 ms
;;   (setq beacon-blink-when-focused t)
;;   (beacon-mode 1))



(use-package writegood-mode
  :defer
  :commands (writegood-grade-level writegood-reading-ease))



(use-package langtool
  :bind (("C-x 4 w" . langtool-check)
         ("C-x 4 W" . langtool-check-done)
         ("C-x 4 l" . langtool-switch-default-language)
         ("C-x 4 4" . langtool-show-message-at-point)
         ("C-x 4 c" . langtool-correct-buffer))
  :defer
  :init
  (setq langtool-java-bin "/usr/bin/java")
  ;; (setq langtool-java-user-arguments "--add-modules java.xml.bind")
  (setq langtool-java-user-arguments nil)
  (setq langtool-user-arguments "--add-modules java.xml.bind")
  (setq langtool-language-tool-jar
        "/usr/local/Cellar/languagetool/3.9/libexec/languagetool-commandline.jar")
  (setq langtool-default-language "en-US")
  (setq langtool-mother-tongue "en-US")
  (setq langtool-disabled-rules '("WHITESPACE_RULE"
                                  "EN_UNPAIRED_BRACKETS"))
  :config
  (defun langtool--invoke-process (file begin finish &optional lang)
    (when (listp mode-line-process)
      (add-to-list 'mode-line-process '(t langtool-mode-line-message)))
    ;; clear previous check
    (langtool--clear-buffer-overlays)
    (cl-destructuring-bind (command args)
        (langtool--basic-command&args)
      ;; Construct arguments pass to jar file.
      (setq args (append
                  args
                  (list "-c" (langtool--java-coding-system
                              buffer-file-coding-system)
                        "-l" (or lang langtool-default-language)
                        "-d" (langtool--disabled-rules))))
      (setq newargs (list "--add-modules" "java.xml.bind"))
      (setq args (append newargs args))
      (when langtool-mother-tongue
        (setq args (append args (list "-m" langtool-mother-tongue))))
      (setq args (append args (langtool--custom-arguments 'langtool-user-arguments)))
      (setq args (append args (list (langtool--process-file-name file))))
      (langtool--debug "Command" "%s: %s" command args)
      (let* ((buffer (langtool--process-create-buffer))
             (proc (langtool--with-java-environ
                    (apply 'start-process "LanguageTool" buffer command args))))
        (set-process-filter proc 'langtool--process-filter)
        (set-process-sentinel proc 'langtool--process-sentinel)
        (process-put proc 'langtool-source-buffer (current-buffer))
        (process-put proc 'langtool-region-begin begin)
        (process-put proc 'langtool-region-finish finish)
        (setq langtool-buffer-process proc)
        (setq langtool-mode-line-message
              (list " LanguageTool"
                    (propertize ":run" 'face compilation-info-face)))))))



(use-package ssh-config-mode
  :mode (("\\.ssh/config\\'"      . ssh-config-mode)
         ("/sshd?_config\\'"      . ssh-config-mode)
         ("/known_hosts\\'"       . ssh-known-hosts-mode)
         ("/authorized_keys2?\\'" . ssh-authorized-keys-mode))
  :bind (:map ssh-config-mode-map
              ("C-p" . ssh-config-host-prev)
              ("C-n" . ssh-config-host-next))
  :defer t
  :config
  (add-hook 'ssh-config-mode-hook 'turn-on-font-lock))



;; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
   Dwim means: region, org-src-block, org-subtree, or defun,
   whichever applies first. Narrowing to org-src-block actually
   calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is
already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if you
         ;; don't want it.
         (cond ((ignore-errors (org-edit-src-code))
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

;; replace downcase region
(global-set-key "\C-x\C-l" 'narrow-or-widen-dwim)



(use-package twittering-mode
  :defer t
  :commands twit
  :init
  (add-hook 'twittering-edit-mode-hook (lambda () (flyspell-mode)))
  :config
  (setq twittering-use-master-password t
        twittering-icon-mode t
        twittering-use-icon-storage t
        twittering-timer-interval 120
        twittering-convert-fix-size 52
        twittering-initial-timeline-spec-string '(":home")
        twittering-edit-skeleton 'inherit-any
        twittering-display-remaining t
        twittering-timeline-footer  " ⤵ "
        twittering-timeline-header  " ⤵ ")

  ;; better status format
  (setq twittering-status-format
        "%FOLD{%RT{%FACE[bold]{RT}}%i%s  %r %@{}\n%FILL[          ]{%T%RT{\nretweeted by %s @%C{%Y-%m-%d %H:%M:%S}}}}")

  ;; mode line symbol
  (add-hook 'twittering-mode-hook
            '(lambda () (setq mode-name "♪")))
  ;; hydra
  (defhydra hydra-twittering (:color blue :hint nil)
    "
                                                                    ╭────────────┐
     Tweets                User                        Timeline     │ Twittering │
  ╭─────────────────────────────────────────────────────────────────┴────────────╯
    _k_  [_t_] post tweet      _p_  [_f_] follow                  ^_g_^      [_u_] update
    ^↑^  [_X_] delete tweet    ^↑^  [_F_] unfollow              ^_S-SPC_^    [_._] new
    ^ ^  [_r_] retweet         ^ ^  [_d_] direct message          ^^↑^^      [^@^] current user
    ^↓^  [_R_] retweet & edit  ^↓^  [_i_] profile (browser)   _h_ ←   → _l_  [_a_] toggle
    _j_  [_b_] favorite        _n_   ^ ^                          ^^↓^^
    ^ ^  [_B_] unfavorite      ^ ^   ^ ^                         ^_SPC_^
    ^ ^  [_RET_] reply         ^ ^   ^ ^                          ^_G_^
    ^ ^  [_T_] show Thread
    ^ ^  [_y_] yank url          Items                     Do
    ^ ^  [_Y_] yank tweet     ╭───────────────────────────────────────────────────────
    ^ ^  [_e_] edit mode        _<backtab>_ ← _o_pen → _<tab>_    [_q_] exit
    ^ ^   ^ ^                   ^         ^   ^ ^      ^     ^    [_/_] search
  --------------------------------------------------------------------------------
       "
    ("\\" nil "quit")
    ("q"          twittering-kill-buffer)
    ("e"          twittering-edit-mode)
    ("j"          twittering-goto-next-status :color red)
    ("k"          twittering-goto-previous-status :color red)
    ("h"          twittering-switch-to-next-timeline :color red)
    ("l"          twittering-switch-to-previous-timeline :color red)
    ("g"          beginning-of-buffer)
    ("G"          end-of-buffer)
    ("t"          twittering-update-status-interactive)
    ("X"          twittering-delete-status)
    ("RET"        twittering-reply-to-user)
    ("r"          twittering-native-retweet)
    ("R"          twittering-organic-retweet)
    ("d"          twittering-direct-message)
    ("u"          twittering-current-timeline)
    ("b"          twittering-favorite)
    ("B"          twittering-unfavorite)
    ("f"          twittering-follow)
    ("F"          twittering-unfollow)
    ("i"          twittering-view-user-page)
    ("/"          twittering-search)
    ("."          twittering-visit-timeline)
    ("@"          twittering-other-user-timeline)
    ("T"          twittering-toggle-or-retrieve-replied-statuses)
    ("o"          twittering-click)
    ("<tab>"        twittering-goto-next-thing :color red)
    ("<backtab>"  twittering-goto-previous-thing :color red)
    ("n"          twittering-goto-next-status-of-user :color red)
    ("p"          twittering-goto-previous-status-of-user :color red)
    ("SPC"        twittering-scroll-up :color red)
    ("S-SPC"      twittering-scroll-down :color red)
    ("y"          twittering-push-uri-onto-kill-ring)
    ("Y"          twittering-push-tweet-onto-kill-ring)
    ("a"          twittering-toggle-activate-buffer))
  ;; set the new bindings
  (bind-keys :map twittering-mode-map
             ("\\" . hydra-twittering/body)
             ("q" . twittering-kill-buffer)
             ("Q" . twittering-edit-mode)
             ("j" . twittering-goto-next-status)
             ("k" . twittering-goto-previous-status)
             ("h" . twittering-switch-to-next-timeline)
             ("l" . twittering-switch-to-previous-timeline)
             ("g" . beginning-of-buffer)
             ("G" . end-of-buffer)
             ("t" . twittering-update-status-interactive)
             ("X" . twittering-delete-status)
             ("RET" . twittering-reply-to-user)
             ("r" . twittering-native-retweet)
             ("R" . twittering-organic-retweet)
             ("d" . twittering-direct-message)
             ("u" . twittering-current-timeline)
             ("b" . twittering-favorite)
             ("B" . twittering-unfavorite)
             ("f" . twittering-follow)
             ("F" . twittering-unfollow)
             ("i" . twittering-view-user-page)
             ("/" . twittering-search)
             ("." . twittering-visit-timeline)
             ("@" . twittering-other-user-timeline)
             ("T" . twittering-toggle-or-retrieve-replied-statuses)
             ("o" . twittering-click)
             ("TAB" . twittering-goto-next-thing)
             ("<backtab>" . twittering-goto-previous-thing)
             ("n" . twittering-goto-next-status-of-user)
             ("p" . twittering-goto-previous-status-of-user)
             ("SPC" . twittering-scroll-up)
             ("S-SPC" . twittering-scroll-down)
             ("y" . twittering-push-uri-onto-kill-ring)
             ("Y" . twittering-push-tweet-onto-kill-ring)
             ("a" . twittering-toggle-activate-buffer)))



(use-package js-codemod
  :commands (js-codemod-mod-region))



(eval-after-load 'grep
  '(progn
     (dolist (v '("node_modules"
                  "bower_components"
                  ".sass_cache"
                  "sampleModels"
                  "release"
                  "build"
                  "dist"
                  ".cache"
                  ".npm"))
       (add-to-list 'grep-find-ignored-directories v))
     (dolist (v '("*.min.js"
                  "*.bundle.js"
                  "*.gbim"
                  "*.min.css"
                  "*.log"))
       (add-to-list 'grep-find-ignored-files v))))



;; https://github.com/tarsius/magit-rockstar/blob/master/magit-rockstar.el#L134
(defun magit-branch-pull-request (number &optional branch checkout)
  "Create a new branch from a Github pull request and show its log.
Read \"NR[:BRANCH-NAME] from the user.  If BRANCH-NAME is not
provided use \"pr-NR\".  Set \"master\" as the upstream.
Assume all pull requests can be found on \"origin\".  With a
prefix argument checkout branch instead of showing its log."
  (interactive
   (let ((input (magit-read-string
                 "Branch pull request (NR[:BRANCH-NAME])"
                 (magit-section-when 'magithub-issue
                   (number-to-string
                    (plist-get (magit-section-value it) :number))))))
     (if (string-match "\\([1-9][0-9]*\\)\\(?::\\(.+\\)\\)?" input)
         (list (match-string 1 input)
               (match-string 2 input)
               current-prefix-arg)
       (user-error "Invalid input"))))
  (unless branch
    (setq branch (format "pr-%s" number)))
  (magit-call-git "fetch" "origin" (format "pull/%s/head:%s" number branch))
  (magit-set-branch*merge/remote branch "master")
  (if checkout
      (magit-run-git "checkout" branch)
    (apply #'magit-log (list branch) (magit-log-arguments))))

(defun endless/add-PR-fetch ()
  "If refs/pull is not defined on a GH repo, define it."
  (let ((fetch-address
         "+refs/pull/*/head:refs/pull/origin/*")
        (magit-remotes
         (magit-get-all "remote" "origin" "fetch")))
    (unless (or (not magit-remotes)
                (member fetch-address magit-remotes))
      (when (string-match
             "github" (magit-get "remote" "origin" "url"))
        (magit-git-string
         "config" "--add" "remote.origin.fetch"
         fetch-address)))))

(add-hook 'magit-mode-hook #'endless/add-PR-fetch)



(use-package magithub
  :after magit
  :config (setq magithub-features t
                magithub-feature-autoinject t
                magithub-dir "~/Dropbox/sync/magithub"))

(defun spacemacs//short-key-state (modeon)
  "Set evil-evilified-state explicitly."
  (if modeon
      (evil-evilified-state)
    (evil-normal-state)))

(use-package realgud
  :defer t
  :commands (realgud:gdb realgud:pdb)
  :init
  (progn
    (spacemacs/set-leader-keys "de" 'realgud:cmd-eval-dwim)
    (advice-add 'realgud-short-key-mode-setup
                :before #'spacemacs//short-key-state)

    (evilified-state-evilify-map realgud:shortkey-mode-map
      :eval-after-load realgud
      :mode realgud-short-key-mode
      :bindings
      "s" 'realgud:cmd-next
      "i" 'realgud:cmd-step
      "b" 'realgud:cmd-break
      "B" 'realgud:cmd-clear
      "o" 'realgud:cmd-finish
      "c" 'realgud:cmd-continue
      "e" 'realgud:cmd-eval
      "r" 'realgud:cmd-restart
      "q" 'realgud:cmd-quit
      "S" 'realgud-window-cmd-undisturb-src))
  :config (realgud-safe-mode  -1))



(eval-after-load 'hippie-exp
  ;; Modified version of try-expand-line from hippie-exp.el v1.6 by Anders Holst
  ;; Changed to return the completion instead of calling he-substitute-string
  '(progn (defun my-try-expand-line (old)
            "Try to complete the current line to an entire line in the buffer.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible completions of the same
string).  It returns t if a new completion is found, nil otherwise."
            (let ((expansion ())
                  (strip-prompt (and (get-buffer-process (current-buffer))
                                     comint-use-prompt-regexp
                                     comint-prompt-regexp)))
              (if (not old)
                  (progn
                    (he-init-string (he-line-beg strip-prompt) (point))
                    (set-marker he-search-loc he-string-beg)
                    (setq he-search-bw t)))

              (if (not (equal he-search-string ""))
                  (save-excursion
                    (save-restriction
                      (if hippie-expand-no-restriction
                          (widen))
                      ;; Try looking backward unless inhibited.
                      (if he-search-bw
                          (progn
                            (goto-char he-search-loc)

                            (setq expansion (he-line-search he-search-string
                                                            strip-prompt t))
                            (set-marker he-search-loc (point))
                            (if (not expansion)
                                (progn
                                  (set-marker he-search-loc he-string-end)
                                  (setq he-search-bw ())))))

                      (if (not expansion) ; Then look forward.
                          (progn
                            (goto-char he-search-loc)
                            (setq expansion (he-line-search he-search-string
                                                            strip-prompt nil))
                            (set-marker he-search-loc (point)))))))

              (if (not expansion)
                  (progn
                    (if old (he-reset-string))
                    ())
                (progn
                  expansion))))

          (defun get-hippie-expand-lines ()
            (let (completions-list candidate)
              (setq candidate (my-try-expand-line nil))
              (if candidate
                  (progn
                    (push candidate completions-list)
                    (while (progn
                             (setq candidate (my-try-expand-line t))
                             (if candidate
                                 (push candidate completions-list)
                               nil))))
                nil)
              completions-list))

          (defun company-hippie-line (command &optional arg &rest ignored)
            (interactive (list 'interactive))

            (cl-case command
              (interactive (company-begin-backend 'company-hippie-line))
              (prefix (and (company-grab-symbol)
                           (not (looking-at "[:word:]"))
                           (let (p1 p2)
                             (save-excursion
                               (end-of-line)
                               (setq p2 (point))
                               (back-to-indentation)
                               (setq p1 (point)))
                             (buffer-substring-no-properties p1 p2))))
              (candidates (get-hippie-expand-lines))))

          ;; Uncomment if you want to get line completion popups whenever found
          ;; (add-to-list 'company-backends 'company-hippie-line)
          (global-set-key (kbd "C-x l") 'company-hippie-line)))

(defun eww-more-readable ()
  "Makes eww more pleasant to use. Run it after eww buffer is loaded."
  (interactive)
  (setq eww-header-line-format nil)               ;; removes page title
  (setq mode-line-format nil)                     ;; removes mode-line
  (set-window-margins (get-buffer-window) 20 20)  ;; increases size of margins
  (redraw-display)                                ;; apply mode-line changes
  (eww-reload 'local))                            ;; apply eww-header changes

(use-package nov
  :defer
  :commands nav-mode
  :mode ("\\.epub\\'" . nov-mode))

;; pdf view mode midnite theme
(setq pdf-view-midnight-colors '("#ff9900" . "#0a0a12")) ; set the amber profile as default (see below)

(defun bms/pdf-no-filter ()
  "View pdf without colour filter."
  (interactive)
  (pdf-view-midnight-minor-mode -1))


;; change midnite mode colours functions
(defun bms/pdf-midnite-original ()
  "Set pdf-view-midnight-colors to original colours."
  (interactive)
  (setq pdf-view-midnight-colors '("#839496" . "#002b36")) ; original values
  (pdf-view-midnight-minor-mode))


(defun bms/pdf-midnite-amber ()
  "Set pdf-view-midnight-colors to amber on dark slate blue."
  (interactive)
  (setq pdf-view-midnight-colors '("#ff9900" . "#0a0a12")) ; amber
  (pdf-view-midnight-minor-mode))


(defun bms/pdf-midnite-green ()
  "Set pdf-view-midnight-colors to green on black."
  (interactive)
  (setq pdf-view-midnight-colors '("#00B800" . "#000000")) ; green
  (pdf-view-midnight-minor-mode))

(defun bms/pdf-midnite-colour-schemes ()
  "Midnight mode colour schemes bound to keys"
        (local-set-key (kbd "!") (quote bms/pdf-no-filter))
        (local-set-key (kbd "@") (quote bms/pdf-midnite-amber))
        (local-set-key (kbd "#") (quote bms/pdf-midnite-green)
            (local-set-key (kbd "$") (quote bms/pdf-midnite-original))))

(setq counsel-find-file-occur-cmd "ls | grep -i -E '%s' | gxargs -d '\n' ls")

(setq yq/large-screen-default-font '("Hack"
                                     ;;"Menlo"
                                     ;; "Anonymous Pro for Powerline"
                                     ;; "InconsolataG for Powerline"
                                     ;; "Source Code Pro for Powerline"
                                     :size 15
                                     :weight normal
                                     :width normal
                                     :powerline-scale 1.1))

(defun yq/default-font-large-screen ()
  "font for large screen"
  (interactive)
  (spacemacs/set-default-font yq/large-screen-default-font))

(defun treemacs-toggle ()
  "If a treemacs buffer exists and is visible hide it.
If a treemacs buffer exists, but is not visible bring it to the foreground
and select it.
If no treemacs buffer exists call `treemacs'."
  (interactive)
  (-pcase (treemacs--current-visibility)
    ['visible
     (treemacs--select-visible)
     (if (one-window-p)
         (switch-to-buffer (other-buffer))
       (bury-buffer))]
    ['exists
     (treemacs)]
    ['none
     (treemacs)]
    [_ (error "[Treemacs] Invalid visibility value: %s" (treemacs--current-visibility))]))

(cond
 ((executable-find "aspell")
  ;; you may also need `ispell-extra-args'
  (setq ispell-program-name "aspell"))
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")))
(setq wakatime-api-key "99569b07-e1f8-4458-aeb1-fd3fef58ff49")
(global-wakatime-mode)
