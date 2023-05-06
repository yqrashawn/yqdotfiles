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
  (add-hook! 'org-mode-hook 'org-modern-mode))

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

(defadvice! +todoist ()
  :before #'todoist
  (setq! todoist-token
    (-> (auth-source-search :host "todoist.com" :user "namy.19@gmail.com")
      car
      (plist-get :api_token))))


(use-package! denote :defer t
  :init
  (setq! denote-directory (expand-file-name "~/Dropbox/sync/denote")))
