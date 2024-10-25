;;; org.el -*- lexical-binding: t; -*-

;; fix orgit problem
(require 'ol)
(setq org-journal-file-format "%Y-%m-%d-%a.org"
      org-noter-notes-search-path (expand-file-name "~/Dropbox/ORG/notes")
      org-roam-directory (expand-file-name "~/Dropbox/ORG")
      ;; org-roam-verbose nil
      org-journal-dir (expand-file-name "~/Dropbox/ORG"))

(after! org
  (setq! org-log-done 'time
         org-roam-dailies-directory ""
         org-startup-with-inline-images "inlineimages"
         org-cycle-emulate-tab nil
         org-cycle-global-at-bob t
         org-hide-block-startup t
         org-agenda-dim-blocked-tasks nil
         org-agenda-inhibit-startup t)
  (pushnew! org-tags-exclude-from-inheritance "project")
  (pushnew! org-agenda-files (concat org-roam-directory org-roam-dailies-directory))
  (defadvice! +org-agenda--quit (&rest _)
    :before #'org-agenda--quit
    (org-save-all-org-buffers))
  (add-hook! 'auto-save-hook #'org-save-all-org-buffers)
  (set-popup-rule!
    "^\\*Capture\\*$\\|CAPTURE-.*$"
    :actions '(display-buffer-at-bottom)
    :size 0.25
    :quit nil
    :select t
    :modeline t
    :autosave 'ignore)
  (add-hook! 'org-mode-hook 'org-modern-mode)
  (defadvice! +org-self-insert-command (x)
    "In org-mode, insert ~ when press `, vice visa
insert ~/ normally"
    :after #'org-self-insert-command
    ;; ` 96
    ;; ~ 126
    ;; / 47
    (cond
     ((and
       (eq (char-before) 47)
       (eq (char-before (- (point) 1)) 96))
      (progn
        (delete-char -2)
        (insert "~/")))
     ((eq (char-before) 96)
      (progn
        (delete-char -1)
        (delete-char 1)
        (insert 126)
        (insert 126)
        (backward-char 1)))
     ((eq (char-before) 126)
      (progn
        (delete-char -1)
        (insert 96))))))

(setq +org-roam-open-buffer-on-find-file nil)

(after! org-roam
  (pushnew! org-default-properties "ROAM_ALIASES" "ROAM_REFS" "CREATED" "ROAM_EXCLUDE")
  (pushnew! org-roam-file-exclude-regexp (expand-file-name "~/Dropbox/ORG/logseq/"))
  (setq!
   org-roam-protocol-store-links t
   ;; org-roam-dailies-capture-templates
   ;;    `(("d" "Default daily template" entry
   ;;       "* %<%H:%M:%S> %?"
   ;;       :if-new (file+head "%<%Y-%m-%d %a>.org"
   ;;                          ":PROPERTIES:
   ;; :CREATED: <%<%Y-%m-%d %a %H:%M>>
   ;; :CATEGORY: Daily
   ;; :END:
   ;; #+TITLE: %<%Y-%m-%d %a>
   ;; ")))
   ;;    org-roam-capture-ref-templates
   ;;    '(("r" "ref" plain "%?"
   ;;       :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
   ;;                          ":PROPERTIES:
   ;; :CREATED: <%<%Y-%m-%d %a %H:%M>>
   ;; :CATEGORY: Roam Ref
   ;; :END:
   ;; #+FILETAGS: %^g
   ;; #+TITLE: ${title}\n
   ;; * Description")
   ;;        :unnarrowed t))
   ))

(use-package! orgbox
  :commands (orgbox orgbox-schedule)
  :init
  (setq! orgbox-start-time-of-day "9:30"
         orgbox-start-time-of-weekends "11:00"
         orgbox-start-time-of-evening "20:00"))

;; (use-package! vulpea
;;   :defer t
;;   ;; hook into org-roam-db-autosync-mode you wish to enable
;;   ;; persistence of meta values (see respective section in README to
;;   ;; find out what meta means)
;;   :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable)))

(defun +org-has-todo-p ()
  "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
  (org-element-map
      (org-element-parse-buffer 'headline)
      'headline
    (lambda (h)
      (eq (org-element-property :todo-type h)
          'todo))
    nil 'first-match))

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

(use-package! todoist
  :commands (todoist)
  :init
  (setq! todoist-token +todoist-token
         todoist-use-scheduled-instead-of-deadline t
         todoist-backing-buffer (concat doom-cache-dir "todoist.org")))

(use-package! denote
  :defer t
  :init
  (setq! denote-directory (expand-file-name "~/Dropbox/sync/denote")
         denote-file-type 'markdown-yaml)

  (defun +denote-daily-note-file-name ()
    (concat denote-directory "/" (format-time-string "%Y%m%dT000000" (current-time)) "==dailynote--daily-note__dailynote.md"))

  (defun +denote-daily-note ()
    (interactive)
    (let ((daily-note-file (+denote-daily-note-file-name)))
      (if (f-exists? daily-note-file)
          (find-file daily-note-file)
        (progn
          (denote "Daily Note" '("dailynote") nil nil (format-time-string "%Y%m%dT000000" (current-time)) nil "dailynote")
          (save-buffer)))
      daily-note-file))

  (defun +side-notes-toggle-daily-note ()
    (interactive)
    (let ((daily-note-file (+denote-daily-note-file-name)))
      (unless (f-exists? daily-note-file)
        (+denote-daily-note)
        (bury-buffer))
      (setq! side-notes-file daily-note-file)
      (call-interactively #'side-notes-toggle-notes))))
