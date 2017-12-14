(setq org-agenda-files (list "~/Dropbox/ORG"))
(setq org-agenda-skip-unavailable-files t)
(with-eval-after-load 'org-agenda
  (add-to-list 'org-agenda-custom-commands
               '("P" "Printed agenda"
                 ((agenda "" ((org-agenda-ndays 7)                      ;; overview of appointments
                              (org-agenda-start-on-weekday nil)         ;; calendar begins today
                              (org-agenda-repeating-timestamp-show-all t)
                              (org-agenda-entry-types '(:timestamp :sexp))))
                  (agenda "" ((org-agenda-ndays 1)                      ;; daily agenda
                              (org-deadline-warning-days 7)             ;; 7 day advanced warning for deadlines
                              (org-agenda-todo-keyword-format "[ ]")
                              (org-agenda-scheduled-leaders '("" ""))
                              (org-agenda-prefix-format "%t%s")))
                  (todo "TODO"                                          ;; todos sorted by context
                        ((org-agenda-files (org-projectile-todo-files))
                         (org-agenda-prefix-format "[ ] %T: ")
                         (org-agenda-sorting-strategy '(tag-up priority-down))
                         (org-agenda-todo-keyword-format "")
                         (org-agenda-overriding-header "\nTasks by Context\n------------------\n"))))
                 ((org-agenda-with-colors nil)
                  (org-agenda-compact-blocks t)
                  (org-agenda-remove-tags t)
                  (ps-number-of-columns 2)
                  (ps-landscape-mode t))))
  (add-to-list 'org-agenda-custom-commands
               '("d" "Upcoming deadlines" agenda ""
                 ((org-agenda-entry-types '(:deadline))
                  (org-agenda-ndays 1)
                  (org-deadline-warning-days 30)
                  (org-agenda-time-grid nil))))
  (add-to-list 'org-agenda-custom-commands '("w" . "Work stuff"))
  (add-to-list 'org-agenda-custom-commands '("wt" "Agenda and work todo" ((agenda "") (tags-todo "work"))))
  (add-to-list 'org-agenda-custom-commands
               '("B" "Working Weekly Todo"
                 ((tags-todo "work")
                  (tags-todo "MAYBE+work") ;; review someday/maybe items
                  (tags-todo "WAITING+work"))
                 nil
                 ("~/agendas/work/todo.pdf"
                  "~/agendas/work/todo.csv"
                  "~/agendas/work/todo.txt"
                  "~/agendas/work/todo.html")))

  (add-to-list 'org-agenda-custom-commands
               '("ww" "Working Weekly Review"
                 ((agenda "" ((org-agenda-ndays 7))) ;; review upcoming deadlines and appointments
                  ;; type "l" in the agenda to review logged items
                  (stuck "work" ((org-agenda-files (org-projectile-todo-files)))) ;; review stuck projects
                  (tags-todo "work")
                  (tags-todo "MAYBE+work") ;; review someday/maybe items
                  (tags-todo "WAITING+work")) nil ("~/agendas/week/work.pdf" "~/agendas/week/work.csv" "~/agendas/week/work.txt" "~/agendas/week/work.html"))) ;; review waiting items
  (add-to-list 'org-agenda-custom-commands
               '("wp" . "Working Priority")) ;; review waiting items
  (add-to-list 'org-agenda-custom-commands
               '("wpa" "Working Priority A" tags-todo "+PRIORITY=\"A\"+work")) ;; review waiting items
  (add-to-list 'org-agenda-custom-commands
               '("wpb" "Working Priority B" tags-todo "+PRIORITY=\"B\"+work")) ;; review waiting items
  (add-to-list 'org-agenda-custom-commands
               '("wpb" "Working Priority C" tags-todo "+PRIORITY=\"C\"+work")) ;; review waiting items
  (add-to-list 'org-agenda-custom-commands '("h" . "Home stuff"))
  (add-to-list 'org-agenda-custom-commands '("ht" "Agenda and home todo" ((agenda "") (tags-todo "home"))))
  (add-to-list 'org-agenda-custom-commands
               '("hw" "Home Weekly Review"
                 ((agenda "" ((org-agenda-ndays 7))) ;; review upcoming deadlines and appointments
                  ;; type "l" in the agenda to review logged items
                  (stuck "" ((org-agenda-files (list "~/Dropbox/ORG")))) ;; review stuck projects
                  (tags-todo "home")
                  (tags-todo "MAYBE+home") ;; review someday/maybe items
                  (tags-todo "WAITING+home")))) ;; review waiting items

  ;; (add-to-list org-agenda-custom-commands
  ;;              '("pc" "C items" tags-todo "+PRIORITY=\"C\""))
  (add-to-list 'org-agenda-custom-commands '("r" "Read later" ((tags-todo "read")) nil ("~/agendas/work/readlater.html" "~/agendas/work/readlater.txt")))

  ;; (require 'org-projectile)
  ;; (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
  )