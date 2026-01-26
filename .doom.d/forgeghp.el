;;; .nixpkgs/.doom.d/forgeghp.el -*- lexical-binding: t; -*-

;;; forgeghp.el --- GitHub Projects integration for Forge -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: yqrashawn
;; Keywords: tools, vc
;; Package-Requires: ((emacs "29.1") (forge "0.5.0") (transient "0.7.0"))

;;; Commentary:

;; This package integrates GitHub Projects v2 with Forge.
;; When a repository has `forgeghp-owner` and `forgeghp-number` git config
;; values set, it displays project metadata (Status, Priority, etc.) in
;; forge-issue-mode and forge-pullreq-mode buffers.

;;; Code:

(require 'forge)
(require 'forge-topic)
(require 'transient)
(require 'json)

;;; Configuration Detection

(defun forgeghp-get-owner ()
  "Get GitHub Project owner from git config."
  (magit-get "forgeghp" "owner"))

(defun forgeghp-get-project-number ()
  "Get GitHub Project number from git config."
  (when-let ((num (magit-get "forgeghp" "number")))
    (string-to-number num)))

(defun forgeghp-repo-linked-p ()
  "Check if current repo is linked to a GitHub Project."
  (and (forgeghp-get-owner)
       (forgeghp-get-project-number)))

(defun forgeghp-get-project-id ()
  "Get GitHub Project GraphQL ID via gh CLI.
Returns cached value if available."
  (when-let ((owner (forgeghp-get-owner))
             (number (forgeghp-get-project-number)))
    (or (forgeghp--cache-get 'project-id)
        (when-let* ((output (forgeghp--run-gh-command-sync
                             "project" "list" "--owner" owner "--format" "json"))
                    (projects (json-read-from-string output))
                    (project (seq-find
                              (lambda (p)
                                (= (alist-get 'number p) number))
                              (alist-get 'projects projects))))
          (let ((id (alist-get 'id project)))
            (forgeghp--cache-put 'project-id id)
            id)))))

;;; Cache Management (global per-repo, per-session)

(defvar forgeghp--cache-table (make-hash-table :test 'equal)
  "Global cache keyed by (owner . project-number).
Each value is an alist of (KEY . VALUE) pairs.")

(defun forgeghp--cache-key ()
  "Get cache key for current repo's project."
  (when-let ((owner (forgeghp-get-owner))
             (number (forgeghp-get-project-number)))
    (cons owner number)))

(defun forgeghp--cache-get (key)
  "Get cached value for KEY in current project."
  (when-let ((cache-key (forgeghp--cache-key)))
    (alist-get key (gethash cache-key forgeghp--cache-table))))

(defun forgeghp--cache-put (key value)
  "Cache VALUE for KEY in current project."
  (when-let ((cache-key (forgeghp--cache-key)))
    (let ((project-cache (gethash cache-key forgeghp--cache-table)))
      (setf (alist-get key project-cache) value)
      (puthash cache-key project-cache forgeghp--cache-table))))

(defun forgeghp-refresh-cache ()
  "Clear project cache for current repo and refresh buffer."
  (interactive)
  (when-let ((cache-key (forgeghp--cache-key)))
    (remhash cache-key forgeghp--cache-table))
  (when (derived-mode-p 'forge-topic-mode)
    (forge-topic-refresh-buffer)))

;;; gh CLI Wrapper

(defun forgeghp--strip-ansi-codes (str)
  "Strip ANSI escape codes from STR."
  ;; Strip ESC[...m (color codes), ESC[?...h/l (cursor control), etc.
  (replace-regexp-in-string "\033\\[\\?*[0-9;]*[a-zA-Z]" "" str))

(defun forgeghp--extract-json (str)
  "Extract JSON from STR, skipping progress messages before it.
gh outputs progress messages before the actual JSON payload."
  (when str
    (let* ((cleaned (forgeghp--strip-ansi-codes str))
           ;; Remove carriage returns, bell, and other control chars
           (cleaned (replace-regexp-in-string "[\r\007\015]" "" cleaned))
           ;; Find first { or [ (start of JSON)
           (json-start (string-match "[{[]" cleaned)))
      (if json-start
          (substring cleaned json-start)
        ;; No JSON found, return nil (indicates error)
        (progn
          (message "forgeghp: No JSON found in output: %s" 
                   (substring cleaned 0 (min 100 (length cleaned))))
          nil)))))

(defun forgeghp--run-gh-command-sync (&rest args)
  "Run gh CLI command synchronously with ARGS.
Returns stdout as string or nil on error."
  (condition-case err
      (with-temp-buffer
        (let* ((process-environment (cons "NO_COLOR=1" process-environment))
               (exit-code (apply #'call-process "gh" nil t nil args))
               (raw-output (buffer-string)))
          (if (zerop exit-code)
              (forgeghp--extract-json raw-output)
            (progn
              (message "forgeghp: gh command failed with exit code %d: %s"
                       exit-code
                       (substring raw-output 0 (min 200 (length raw-output))))
              nil))))
    (error
     (message "forgeghp: gh command error: %s" (error-message-string err))
     nil)))

(defun forgeghp--run-gh-command-async (callback &rest args)
  "Run gh CLI command asynchronously with ARGS.
Call CALLBACK with output string on success, nil on error."
  (let ((buffer (generate-new-buffer " *forgeghp-gh*"))
        (process-environment (cons "NO_COLOR=1" process-environment)))
    (make-process
     :name "forgeghp-gh"
     :buffer buffer
     :command (cons "gh" args)
     :sentinel
     (lambda (proc _event)
       (when (eq (process-status proc) 'exit)
         (with-current-buffer buffer
           (funcall callback
                    (if (zerop (process-exit-status proc))
                        (forgeghp--extract-json (buffer-string))
                      nil)))
         (kill-buffer buffer))))))

;;; Project Item Fetching

(defun forgeghp-get-item-by-issue-number (number callback)
  "Find project item matching issue/PR NUMBER.
Call CALLBACK with item alist or nil if not found."
  (when-let ((owner (forgeghp-get-owner))
             (proj-num (forgeghp-get-project-number)))
    ;; Check cache first
    (if-let ((cached (forgeghp--cache-get 'items)))
        (funcall callback (forgeghp--find-item-in-list cached number))
      ;; Fetch and cache
      (forgeghp--run-gh-command-async
       (lambda (output)
         (when output
           (let* ((data (json-read-from-string output))
                  (items (alist-get 'items data)))
             (forgeghp--cache-put 'items items)
             (funcall callback (forgeghp--find-item-in-list items number)))))
       "project" "item-list" (number-to-string proj-num)
       "--owner" owner
       "--format" "json"
       "--limit" "100"))))

(defun forgeghp--find-item-in-list (items number)
  "Find item in ITEMS list matching issue/PR NUMBER via content.url."
  (when-let* ((repo (forge-get-repository :tracked))
              (url-pattern (format "https://github.com/%s/\\(issues\\|pull\\)/%d"
                                   (oref repo slug)
                                   number)))
    (seq-find
     (lambda (item)
       (when-let ((content (alist-get 'content item))
                  (url (alist-get 'url content)))
         (string-match-p url-pattern url)))
     items)))

(defun forgeghp-fetch-item-data (topic callback)
  "Fetch project item data for TOPIC (issue/pullreq).
Call CALLBACK with item alist or nil if not in project."
  (forgeghp-get-item-by-issue-number (oref topic number) callback))

;;; Field Definitions

(defun forgeghp-get-field-definitions (callback)
  "Fetch project field definitions.
Call CALLBACK with fields alist."
  (when-let ((owner (forgeghp-get-owner))
             (number (forgeghp-get-project-number)))
    ;; Check cache
    (if-let ((cached (forgeghp--cache-get 'fields)))
        (funcall callback cached)
      ;; Fetch
      (forgeghp--run-gh-command-async
       (lambda (output)
         (when output
           (let* ((data (json-read-from-string output))
                  (fields (alist-get 'fields data)))
             (forgeghp--cache-put 'fields fields)
             (funcall callback fields))))
       "project" "field-list" (number-to-string number)
       "--owner" owner
       "--format" "json"))))

(defun forgeghp--get-field-by-name (fields name)
  "Get field definition from FIELDS with NAME."
  (seq-find (lambda (f) (string= (alist-get 'name f) name)) fields))

(defun forgeghp--get-field-option-name (fields field-name option-id)
  "Get option name from FIELDS for FIELD-NAME with OPTION-ID."
  (when-let* ((field (forgeghp--get-field-by-name fields field-name))
              (options (alist-get 'options field)))
    (when-let ((option (seq-find (lambda (o) (string= (alist-get 'id o) option-id))
                                 options)))
      (alist-get 'name option))))

;;; Header Insertion Functions

(defun forgeghp-insert-project-status (&optional topic)
  "Insert project Status field for TOPIC in Forge buffer."
  (setq topic (or topic forge-buffer-topic))
  (when (and (forgeghp-repo-linked-p) topic)
    (magit-insert-section (topic-project-status)
      (insert (string-pad "Proj Status :" 11))
      (let ((placeholder (magit--propertize-face "loading..." 'magit-dimmed))
            (target-buffer (current-buffer)))
        (insert placeholder)
        (insert ?\n)
        ;; Async fetch
        (forgeghp-fetch-item-data
         topic
         (lambda (item)
           (forgeghp-get-field-definitions
            (lambda (fields)
              (let ((status (and item (alist-get 'status item))))
                (with-current-buffer target-buffer
                  (let ((inhibit-read-only t))
                    (save-excursion
                      (goto-char (point-min))
                      (when (re-search-forward (regexp-quote placeholder) nil t)
                        (replace-match
                         (if status
                             (propertize status 'face 'forge-topic-label)
                           (propertize "[Not in project]" 'face 'magit-dimmed))
                         t t))))))))))))))

(defun forgeghp-insert-project-priority (&optional topic)
  "Insert project Priority field for TOPIC in Forge buffer."
  (setq topic (or topic forge-buffer-topic))
  (when (and (forgeghp-repo-linked-p) topic)
    (magit-insert-section (topic-project-priority)
      (insert (string-pad "Proj Priority: " 11))
      (let ((placeholder (magit--propertize-face "loading..." 'magit-dimmed))
            (target-buffer (current-buffer)))
        (insert placeholder)
        (insert ?\n)
        (forgeghp-fetch-item-data
         topic
         (lambda (item)
           (forgeghp-get-field-definitions
            (lambda (fields)
              (let* ((priority-field (forgeghp--get-field-by-name fields "Priority"))
                     (priority-id (and item (alist-get 'priority item)))
                     (priority-name (and priority-id
                                         (forgeghp--get-field-option-name
                                          fields "Priority" priority-id))))
                (with-current-buffer target-buffer
                  (let ((inhibit-read-only t))
                    (save-excursion
                      (goto-char (point-min))
                      (when (re-search-forward (regexp-quote placeholder) nil t)
                        (replace-match
                         (if priority-name
                             (propertize priority-name 'face 'forge-topic-label)
                           (propertize "none" 'face 'magit-dimmed))
                         t t))))))))))))))

(defun forgeghp-insert-project-metadata (&optional topic)
  "Insert all project metadata for TOPIC."
  (forgeghp-insert-project-status topic)
  (forgeghp-insert-project-priority topic))

;;; Interactive Edit Commands

(defun forgeghp-set-status ()
  "Set project Status field for current topic."
  (interactive)
  (when-let ((topic (forge-current-topic))
             (project-id (forgeghp-get-project-id)))
    (forgeghp--read-field-option-async
     "Status" "Select status: "
     (lambda (status-name)
       (when status-name
         (forgeghp-fetch-item-data
          topic
          (lambda (item)
            (if (not item)
                (message "Topic not in project")
              (forgeghp-get-field-definitions
               (lambda (fields)
                 (when-let* ((field (forgeghp--get-field-by-name fields "Status"))
                             (field-id (alist-get 'id field))
                             (option (seq-find
                                      (lambda (o) (string= (alist-get 'name o) status-name))
                                      (alist-get 'options field)))
                             (option-id (alist-get 'id option))
                             (item-id (alist-get 'id item)))
                   (forgeghp--edit-item-field
                    project-id item-id field-id option-id
                    (lambda (success)
                      (if success
                          (progn
                            (message "Updated status to: %s" status-name)
                            (forgeghp-refresh-cache))
                        (message "Failed to update status")))))))))))))))

(defun forgeghp-set-priority ()
  "Set project Priority field for current topic."
  (interactive)
  (when-let ((topic (forge-current-topic))
             (project-id (forgeghp-get-project-id)))
    (forgeghp--read-field-option-async
     "Priority" "Select priority: "
     (lambda (priority-name)
       (when priority-name
         (forgeghp-fetch-item-data
          topic
          (lambda (item)
            (if (not item)
                (message "Topic not in project")
              (forgeghp-get-field-definitions
               (lambda (fields)
                 (when-let* ((field (forgeghp--get-field-by-name fields "Priority"))
                             (field-id (alist-get 'id field))
                             (option (seq-find
                                      (lambda (o) (string= (alist-get 'name o) priority-name))
                                      (alist-get 'options field)))
                             (option-id (alist-get 'id option))
                             (item-id (alist-get 'id item)))
                   (forgeghp--edit-item-field
                    project-id item-id field-id option-id
                    (lambda (success)
                      (if success
                          (progn
                            (message "Updated priority to: %s" priority-name)
                            (forgeghp-refresh-cache))
                        (message "Failed to update priority")))))))))))))))

(defun forgeghp--read-field-option-async (field-name prompt callback)
  "Read option from FIELD-NAME with PROMPT, call CALLBACK with result.
CALLBACK is called with the selected option name, or nil if user quit."
  (forgeghp-get-field-definitions
   (lambda (fields)
     (if-let* ((field (forgeghp--get-field-by-name fields field-name))
               (options (alist-get 'options field))
               (names (mapcar (lambda (o) (alist-get 'name o)) options)))
         ;; Use condition-case to catch quit signal
         (condition-case nil
             (let ((result (completing-read prompt names nil t)))
               (funcall callback result))
           (quit
            ;; User pressed C-g, call callback with nil
            (funcall callback nil)))
       ;; No field found
       (message "Field %s not found in project" field-name)
       (funcall callback nil)))))

(defun forgeghp--edit-item-field (project-id item-id field-id option-id callback)
  "Edit project item field via gh CLI.
PROJECT-ID, ITEM-ID, FIELD-ID, OPTION-ID are GraphQL IDs.
Call CALLBACK with t on success, nil on error."
  (forgeghp--run-gh-command-async
   (lambda (output)
     (funcall callback (not (null output))))
   "project" "item-edit"
   "--project-id" project-id
   "--id" item-id
   "--field-id" field-id
   "--option-id" option-id))

;;; Transient Menu

;;;###autoload (autoload 'forgeghp-topic-menu "forgeghp" nil t)
(transient-define-prefix forgeghp-topic-menu ()
  "Edit GitHub Project fields for current topic."
  [:description "GitHub Project"
                ("s" "set status" forgeghp-set-status)
                ("p" "set priority" forgeghp-set-priority)
                ("r" "refresh cache" forgeghp-refresh-cache)])

;;; Section Maps

(defvar-keymap forge-topic-project-status-section-map
  :parent forge-common-map
  "<remap> <magit-edit-thing>" #'forgeghp-set-status)

(defvar-keymap forge-topic-project-priority-section-map
  :parent forge-common-map
  "<remap> <magit-edit-thing>" #'forgeghp-set-priority)

;;; Inline Insertion for magit-status-mode

(defun forgeghp--insert-topic-project-fields (topic &optional separate)
  "Insert project status and priority inline for TOPIC in magit-status.
If SEPARATE is non-nil, insert a space before the first badge.
Returns t if any badges were inserted, nil otherwise."
  (when (forgeghp-repo-linked-p)
    (let ((inserted nil))
      ;; Fetch data asynchronously, but we need to return synchronously
      ;; Check if we have cached data first
      (when-let ((cached-items (forgeghp--cache-get 'items))
                 (cached-fields (forgeghp--cache-get 'fields))
                 (item (forgeghp--find-item-in-list cached-items (oref topic number))))
        ;; Insert status badge
        (when-let ((status (alist-get 'status item)))
          (when (or separate (not inserted))
            (insert " ")
            (setq inserted t))
          (let ((start (point)))
            (insert status)
            (let ((o (make-overlay start (point))))
              (overlay-put o 'priority 2)
              (overlay-put o 'evaporate t)
              (overlay-put o 'font-lock-face
                           '((:background "#6e7681" :foreground "#ffffff")
                             forge-topic-label))
              (overlay-put o 'help-echo "GitHub Project Status"))))
        
        ;; Insert priority badge
        (when-let* ((priority-id (alist-get 'priority item))
                    (priority-name (forgeghp--get-field-option-name
                                    cached-fields "Priority" priority-id)))
          (when (or separate (not inserted))
            (insert " ")
            (setq inserted t))
          (let ((start (point)))
            (insert priority-name)
            (let ((o (make-overlay start (point))))
              (overlay-put o 'priority 2)
              (overlay-put o 'evaporate t)
              (overlay-put o 'font-lock-face
                           '((:background "#8b5cf6" :foreground "#ffffff")
                             forge-topic-label))
              (overlay-put o 'help-echo "GitHub Project Priority")))))
      
      ;; If no cached data, trigger async fetch (will be available on next refresh)
      (unless inserted
        (forgeghp-fetch-item-data topic (lambda (_item) nil)))
      
      inserted)))

;;; Integration Hooks

;;;###autoload
(defun forgeghp-setup ()
  "Set up GitHub Project integration with Forge.
Add this to your config:
  (with-eval-after-load 'forge
    (forgeghp-setup))"
  ;; Add to issue headers (detail view)
  (add-hook 'forge-issue-headers-hook #'forgeghp-insert-project-metadata t)
  ;; Add to pullreq headers (detail view)
  (add-hook 'forge-pullreq-headers-hook #'forgeghp-insert-project-metadata t)
  
  ;; Advise forge--insert-topic to add inline badges in magit-status
  (advice-add 'forge--insert-topic :after #'forgeghp--insert-topic-inline-advice))

(defun forgeghp--insert-topic-inline-advice (topic &optional _width)
  "Advice function to insert project badges inline after labels.
This is called after `forge--insert-topic' completes."
  (when (forgeghp-repo-linked-p)
    (save-excursion
      ;; Move back to find where we just inserted the topic
      ;; We're at the end of the line after labels, before \n
      (beginning-of-line)
      (when (re-search-forward (regexp-quote (oref topic slug)) (line-end-position) t)
        ;; Now we're after the slug, labels should be after this
        ;; Look for the end of labels (before newline)
        (goto-char (line-end-position))
        ;; Back up before the newline to insert badges
        (backward-char 1)
        (forgeghp--insert-topic-project-fields topic t)))))

(provide 'forgeghp)
;;; forgeghp.el ends here
