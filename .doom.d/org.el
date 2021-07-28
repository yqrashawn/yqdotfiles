;;; org.el -*- lexical-binding: t; -*-

(setq org-journal-file-format "%Y-%m-%d-%a.org"
      org-noter-notes-search-path (expand-file-name "~/Dropbox/ORG/notes")
      org-roam-directory (expand-file-name "~/Dropbox/ORG")
      org-journal-dir (expand-file-name "~/Dropbox/ORG"))

(after! org
  (setq! org-log-done 'time
         org-startup-with-inline-images "inlineimages"
         org-cycle-emulate-tab nil
         org-cycle-global-at-bob t
         org-hide-block-startup t)
  (pushnew! org-agenda-files (concat org-roam-directory org-roam-dailies-directory)))

(setq +org-roam-open-buffer-on-find-file nil)
(after! org-roam
  (pushnew! org-default-properties "ROAM_ALIASES" "ROAM_REFS")
  (setq! org-roam-dailies-capture-templates
         `(("d" "default" entry
            "* %?"
            :if-new (file+head "%<%Y-%m-%d %a>.org"
                               "#+title: %<%Y-%m-%d %a>\n")))))

(use-package! orgbox
  :commands (orgbox orgbox-schedule)
  :init
  (setq! orgbox-start-time-of-day "9:30"
         orgbox-start-time-of-weekends "11:00"
         orgbox-start-time-of-evening "20:00"))
