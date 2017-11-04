;; no confirm before evaluate code in src block
(setq org-confirm-babel-evaluate nil)

;; src block have same indentation with #+BEGIN_SRC
(setq org-edit-src-content-indentation 0)

;; Insert immediate timestamp
(defun my-insert-timestamp()
  "Insert the current time in yyyy-mm-dd format."
  (interactive "*")
  (if (eq major-mode 'org-mode)
      (progn
        (org-insert-time-stamp nil t nil)
        (insert " ")
        )
    (insert (format-time-string "%Y-%m-%d" (current-time)))
    )
  )

;; (bind-key "t" #'my-insert-timestamp my-map)

(defun my-insert-timestamp-inactive()
  "Insert the current time in yyyy-mm-dd format."
  (interactive "*")
  (if (eq major-mode 'org-mode)
      (progn
        (org-insert-time-stamp nil t t)
        (insert " ")
        )
    (insert (format-time-string "%Y-%m-%d" (current-time)))
    )
  )

;; (bind-key "T" #'my-insert-timestamp-inactive my-map)
(defun my-insert-datestamp()
  "Insert the current date in yyyy-mm-dd format."
  (interactive "*")
  (if (eq major-mode 'org-mode)
      (progn
        (org-insert-time-stamp nil nil nil)
        (insert " ")
        )
    (insert (format-time-string "%Y-%m-%d" (current-time)))
    )
  )

;; (bind-key "d" #'my-insert-datestamp my-map)

(defun my-insert-datestamp-inactive()
  "Insert the current date in yyyy-mm-dd format."
  (interactive "*")
  (if (eq major-mode 'org-mode)
      (progn
        (org-insert-time-stamp nil nil t)
        (insert " ")
        )
    (insert (format-time-string "%Y-%m-%d" (current-time)))
    )
  )

;; (bind-key "D" #'my-insert-datestamp-inactive my-map)

(defun my-sparse-tree-with-tag-filter()
  "asks for a tag and generates sparse tree for all open tasks in current Org buffer
  that are associated with this tag"
  (interactive "*")
  (setq tag-for-filter
        (org-trim
         (org-icompleting-read "Tags: "
                               'org-tags-completion-function
                               nil nil nil 'org-tags-history))
        )
  (org-occur
   (concat "^\\*+ \\(NEXT\\|TODO\\|WAITING\\|STARTED\\) .+:"
           tag-for-filter
           ":")
   )
  )
;; (bind-key "F" #'my-sparse-tree-with-tag-filter my-map)

;; automatically change status of a heading to DONE when all children are done
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
(setq org-todo-keyword-faces
      (quote (("TODO"      :foreground "lightblue"    :weight bold)
              ("NEXT"      :foreground "red"          :weight bold)
              ("STARTED"   :foreground "red"          :weight bold)
              ("DONE"      :foreground "forest green" :weight bold)
              ("WAITING"   :foreground "orange"       :weight bold)
              ("TEAM"      :foreground "orange"       :weight bold)
              ("SOMEDAY"   :foreground "magenta"      :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("QUOTE"     :foreground "red"          :weight bold)
              ("QUOTED"    :foreground "magenta"      :weight bold)
              ("APPROVED"  :foreground "forest green" :weight bold)
              ("EXPIRED"   :foreground "forest green" :weight bold)
              ("REJECTED"  :foreground "forest green" :weight bold)
              ("OPEN"      :foreground "blue"         :weight bold)
              ("CLOSED"    :foreground "forest green" :weight bold)
              ("PHONE"     :foreground "forest green" :weight bold))))

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED"
               ("ARCHIVE" . t))
              ("WAITING"
               ("WAITING" . t))
              (done
               ("WAITING"))
              ("TODO"
               ("WAITING")
               ("CANCELLED"))
              ("NEXT"
               ("WAITING"))
              ("STARTED"
               ("WAITING"))
              ("DONE"
               ("WAITING")
               ("CANCELLED")))))
(setq org-log-note-headings '((done . "CLOSING NOTE %t")
                              (state . "State %-12s from %-12S %T")
                              (note . "Note taken on %t")
                              (reschedule . "Rescheduled from %S on %t")
                              (delschedule . "Not scheduled, was %S on %t")
                              (redeadline . "New deadline from %S on %t")
                              (deldeadline . "Removed deadline, was %S on %t")
                              (refile . "Refiled on %t")
                              (clock-out . "")))
(setq org-log-note-headings '((done . "CLOSING DONE NOTE %t")
                              (state . "State %-12s from %-12S %t")
                              (note . "Note taken on %t")
                              (reschedule . "Rescheduled from %S on %t")
                              (delschedule . "Not scheduled, was %S on %t")
                              (redeadline . "New deadline from %S on %t")
                              (deldeadline . "Removed deadline, was %S on %t")
                              (refile . "Refiled on %t")
                              (clock-out . "")))
(setq org-todo-keywords (quote
                         (
                          (sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "WAITING(w@/!)" "SOMEDAY(S!)" "|" "DONE(d!/!)" "CANCELLED(c@/!)"))))
;; https://github.com/novoid/dot-emacs/blob/28c146f785c1d87dc821514e8448e3dfe82e56ce/config.org
(setq org-log-done (quote time))
(setq org-log-into-drawer t)
(setq org-log-redeadline (quote note));; record when the deadline date of a tasks is modified
(setq org-log-reschedule (quote time))
(setq org-return-follows-link t)
(setq org-remove-highlights-with-change nil)
(setq org-read-date-prefer-future 'time)
(setq org-list-demote-modify-bullet (quote (("+" . "-")
                                            ("*" . "-")
                                            ("1." . "-")
                                            ("1)" . "-"))))
(setq require-final-newline nil)
(setq mode-require-final-newline nil)
(setq org-adapt-indentation nil);; do not indent drawers/body according to heading level
(setq org-startup-indented t)

(setq org-enforce-todo-dependencies t)
(setq org-insert-heading-respect-content nil)
(setq org-reverse-note-order nil)
(setq org-deadline-warning-days 1)
(setq org-blank-before-new-entry (quote ((heading . t)
                                         (plain-list-item . nil))))
(setq org-todo-repeat-to-state "NEXT")
(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-hierarchical-todo-statistics t)

(evil-define-key 'insert org-mode-map (kbd "C-y") 'org-yank)
(setq org-yank-adjusted-subtrees t)

(setq org-show-context-detail
      '((agenda . lineage) ;; instead of "local"
        (bookmark-jump . lineage)
        (isearch . lineage)
        (default . ancestors)))

;; (setq org-journal-file-format "%Y-%m-%d")

(setq org-barin-path "~/Dropbox/ORG/brain")
(setq org-brain-visualize-default-choices 'root)
;; (setq org-brain-show-resources nil)
;; (setq org-brain-show-text nil)

(setq org-directory "~/Dropbox/ORG")
(setq org-agenda-files (list "~/Dropbox/ORG"))
(setq org-agenda-skip-unavailable-files t)
(setq org-mobile-directory "~/Dropbox/应用/MobileOrg")
(setq org-mobile-inbox-for-pull "~/Dropbox/ORG/flagged.org")
(setq org-default-notes-file '("~/Dropbox/ORG/notes.org"))

(with-eval-after-load 'org-agenda
  (require 'org-projectile)
  (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files))))

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
         (file+olp "~/Dropbox/ORG/notes.org" "capture" "read later")
         "*** TODO %? %^L %^G\n%U")
        ("s" "Some day" entry
         (file+olp "~/Dropbox/ORG/notes.org" "capture" "some day")
         "*** TODO %? %^L %^G\n%U")
        ("n" "notes" entry
         (file+olp "~/Dropbox/ORG/notes.org" "capture" "note")
         "*** %?\n   %U")
        ("c" "code snipptes" entry
         (file+olp "~/Dropbox/ORG/snipptes.org" "snipptes")
         "*** %?\n%U")
        ("f" "file TODOs" entry
         (file "~/Dropbox/ORG/gtd.org")
         "* TODO %? \n %a\n%U")
        ("t" "TODOs" entry
         (file+olp "~/Dropbox/ORG/gtd.org" "misc")
         "* TODO %? \n%U"))))

(push '("b" "Brain" plain (function org-brain-goto-end)
        "* %i%?" :empty-lines 1)
      org-capture-templates)