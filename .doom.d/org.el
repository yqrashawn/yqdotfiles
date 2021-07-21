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
         org-hide-block-startup t))
