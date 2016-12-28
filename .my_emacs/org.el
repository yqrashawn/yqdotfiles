(setq org-directory '("~/Dropbox/org/*.org"))
(setq org-agenda-files '("~/Dropbox/org"))
(setq org-default-notes-file '("~/Dropbox/org/notes.org"))
(setq-default dotspacemacs-configuration-layers
              '((org :variables org-projectile-file "plans.org")))
(with-eval-after-load 'org-agenda
  (require 'org-projectile)
  (setq org-agenda-files (append org-agenda-files (org-projectile:todo-files))))
