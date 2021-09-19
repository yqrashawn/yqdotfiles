;;; org.el -*- lexical-binding: t; -*-

(setq org-journal-file-format "%Y-%m-%d-%a.org"
      org-noter-notes-search-path (expand-file-name "~/Dropbox/ORG/notes")
      org-roam-directory (expand-file-name "~/Dropbox/ORG")
      org-roam-verbose nil
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
  (defadvice! +org-agenda--quit (&rest _) :before #'org-agenda--quit (org-save-all-org-buffers))
  (add-hook! 'auto-save-hook #'org-save-all-org-buffers)
  (set-popup-rule!
    "^\\*Capture\\*$\\|CAPTURE-.*$"
    :actions '(display-buffer-at-bottom)
    :size 0.25
    :quit nil
    :select t
    :modeline t
    :autosave 'ignore))

(setq +org-roam-open-buffer-on-find-file nil)
(after! org-roam
  (pushnew! org-default-properties "ROAM_ALIASES" "ROAM_REFS" "CREATED")
  (setq!
   org-roam-protocol-store-links t
   org-roam-dailies-capture-templates
   `(("d" "Default daily template" entry
      "* %<%H:%M:%S> %?"
      :if-new (file+head "%<%Y-%m-%d %a>.org"
                         ":PROPERTIES:
:CREATED: <%<%Y-%m-%d %a %H:%M>>
:CATEGORY: Daily
:END:
#+TITLE: %<%Y-%m-%d %a>
")))
   org-roam-capture-ref-templates
   '(("r" "ref" plain "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         ":PROPERTIES:
:CREATED: <%<%Y-%m-%d %a %H:%M>>
:CATEGORY: Roam Ref
:END:
#+FILETAGS: %^g
#+TITLE: ${title}

* Description")
      :unnarrowed t))))

(use-package! orgbox
  :commands (orgbox orgbox-schedule)
  :init
  (setq! orgbox-start-time-of-day "9:30"
         orgbox-start-time-of-weekends "11:00"
         orgbox-start-time-of-evening "20:00"))

(use-package! vulpea
  ;; hook into org-roam-db-autosync-mode you wish to enable
  ;; persistence of meta values (see respective section in README to
  ;; find out what meta means)
  :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable)))


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