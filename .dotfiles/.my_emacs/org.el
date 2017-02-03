(setq org-directory '("~/Dropbox/org/*.org"))
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
