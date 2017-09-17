(setq org-directory "~/Dropbox/org")
;; (setq org-mobile-directory "~/Dropbox/org/MobileOrg")
(setq org-mobile-directory "~/Dropbox/应用/MobileOrg")
(setq org-mobile-inbox-for-pull "~/Dropbox/org/flagged.org")
(setq org-agenda-files '("~/Dropbox/org"))
(setq org-default-notes-file '("~/Dropbox/org/notes.org"))
(setq-default dotspacemacs-configuration-layers
              '((org :variables org-projectile-file "plans.org")))
(with-eval-after-load 'org-agenda
  (require 'org-projectile)
  (setq org-agenda-files (append org-agenda-files (org-projectile:todo-files))))

(defvar current-file-reference ""  "Global variable to store the current file reference")

(defun store-file-line-and-col ()
  "Stores the current file, line and column point is at in a string in format \"file-name:line-number-column-number\". Insert the string using \"insert-file-reference\"."
  (interactive)
  (setq current-file-reference (format "%s:%d:%d" (buffer-file-name) (line-number-at-pos) (current-column))))
(defun store-file-and-line ()
  "Stores the current file and line oint is at in a string in format \"file-name:line-number\". Insert the string using \"insert-file-reference\"."
  (interactive)
  (setq current-file-reference (format "[[file:%s::%d][]]" (buffer-file-name) (line-number-at-pos))))

(defun insert-file-reference ()
  "Inserts the value stored for current-file-reference at point."
  (interactive)
  (if (string= "" current-file-reference)
      (message "No current file/line/column set!")
    (insert current-file-reference)))
(setq org-opml-src (concat user-home-directory ".my_emacs/org-opml/"))

(setq org-capture-templates
      (quote
       (("l" "Capture from the Internet with link" entry
         (file+olp "~/Dropbox/org/notes.org" "capture" "read later")
         "*** TODO %? %^L %^G\n%U")
        ("s" "Some day" entry
         (file+olp "~/Dropbox/org/notes.org" "capture" "some day")
         "*** TODO %? %^L %^G\n%U")
        ("n" "notes" entry
         (file+olp "~/Dropbox/org/notes.org" "capture" "note")
         "*** %?\n%U")
        ("f" "file TODOs" entry
         (file "~/Dropbox/org/gtd.org")
         "* TODO %? %^G\n %a\n%U")
        ("t" "TODOs" entry
         (file "~/Dropbox/org/gtd.org")
         "* TODO %? %^G\n%U"))))
;; (use-package ox-opml
;;   :ensure t
;;   :mode "\\.org\\'"
;;   :load-path org-opml-src)

;; (use-package org-opml
;;   :ensure t
;;   :load-path org-opml-src)
