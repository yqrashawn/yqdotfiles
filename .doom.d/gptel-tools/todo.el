;;; .nixpkgs/.doom.d/gptel-tools/todo.el -*- lexical-binding: t; coding: utf-8 -*-

;;; Todo tool for GPTEL inspired by Claude Code's builtin todo tool

(defvar gptelt-todo-list nil
  "The current todo list as an elisp list of plists.
Each todo is a plist: (:id ID :content CONTENT :status STATUS :priority PRIORITY :created-at TIMESTAMP :updated-at TIMESTAMP :completed-at TIMESTAMP :tags LIST)")

(defun gptelt-todo--project-name ()
  "Get current project name for todo persistence."
  ;; Use projectile to get project name
  (projectile-project-name (++workspace-current-project-root)))

(defun gptelt-todo--todo-file ()
  "Return the absolute path to the todo persistence file for this project."
  (let ((dir (expand-file-name "gptelt-todo" user-emacs-directory)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (expand-file-name
     (format "%s.todo.el" (gptelt-todo--project-name))
     dir)))

(defvar gptelt-todo--testing-mode nil
  "When non-nil, disable file persistence for testing.")

(defun gptelt-todo--save ()
  "Persist the current todo list to file."
  (unless gptelt-todo--testing-mode
    (with-current-buffer (find-file-noselect (gptelt-todo--todo-file))
      (erase-buffer)
      (prin1 gptelt-todo-list (current-buffer))
      (let ((inhibit-message t)) (save-buffer)))))

(defun gptelt-todo--load ()
  "Load todo list from file into memory."
  (unless gptelt-todo--testing-mode
    (let ((file (gptelt-todo--todo-file)))
      (setq gptelt-todo-list
            (if (file-exists-p file)
                (with-temp-buffer
                  (insert-file-contents file)
                  (condition-case nil
                      (read (current-buffer))
                    (error nil)))
              nil)))))

(defun gptelt-todo--ensure-loaded ()
  ;; (unless gptelt-todo-list
  ;;   (gptelt-todo--load))
  (gptelt-todo--load))

(defun gptelt-todo--make-id ()
  "Generate a new unique todo id based on timestamp."
  (let ((ts (format-time-string "%Y%m%d%H%M%S%N")))
    (substring ts 0 20)))

;;; API: todo_write
(defvar gptelt-todo-valid-status '(pending in_progress completed))
(defvar gptelt-todo-valid-priority '(high medium low))

(defun gptelt-todo--count-unfinished ()
  "Count pending and in_progress todos."
  (length (seq-filter (lambda (item)
                        (memq (plist-get item :status) '(pending in_progress)))
                      gptelt-todo-list)))

(defun gptelt-todo--status-emoji (status)
  "Return emoji for todo status."
  (pcase status
    ('pending "1️⃣")
    ('in_progress "🚧")
    ('completed "✅")
    (_ "❓")))

(defun gptelt-todo-write (todos)
  "Add or update todo items from a list or vector.

TODOS: list or vector of plists, each with :content (required), :status (optional), :priority (optional), :id (optional).
Returns the updated todo list."
  (gptelt-todo--ensure-loaded)
  ;; Accept both lists and vectors, convert vectors to lists
  (when (vectorp todos)
    (setq todos (append todos nil)))
  (unless (listp todos)
    (error "Todos must be a list or vector"))
  (dolist (todo todos)
    (let ((content (if (listp todo)
                       (or (plist-get todo :content) (cdr (assoc 'content todo)))
                     (error "Each todo must be a list")))
          (status (if (listp todo)
                      (or (plist-get todo :status) (cdr (assoc 'status todo)))
                    nil))
          (priority (if (listp todo)
                        (or (plist-get todo :priority) (cdr (assoc 'priority todo)))
                      nil))
          (id (if (listp todo)
                  (or (plist-get todo :id) (cdr (assoc 'id todo)))
                nil)))
      (unless (and content (stringp content) (> (length content) 0))
        (error "Todo content must be non-empty string"))
      (let ((status (or (and status (intern (downcase (format "%s" status)))) 'pending))
            (priority (or (and priority (intern (downcase (format "%s" priority)))) 'medium)))
        (unless (memq status gptelt-todo-valid-status)
          (error "Invalid todo status: %s" status))
        (unless (memq priority gptelt-todo-valid-priority)
          (error "Invalid todo priority: %s" priority))
        (let* ((new-id (or id (gptelt-todo--make-id)))
               (current-time (float-time))
               (existing-item (seq-find (lambda (item)
                                          (or (and id (equal (plist-get item :id) id))
                                              (equal (plist-get item :content) content)))
                                        gptelt-todo-list))
               (tags (if (listp todo)
                         (or (plist-get todo :tags) (cdr (assoc 'tags todo)))
                       nil))
               (new-item (list :id new-id
                               :content content
                               :status status
                               :priority priority
                               :created-at (or (and existing-item (plist-get existing-item :created-at)) current-time)
                               :updated-at current-time
                               :completed-at (and (eq status 'completed)
                                                  (or (and existing-item (plist-get existing-item :completed-at))
                                                      current-time))
                               :tags (or tags (and existing-item (plist-get existing-item :tags))))))
          (let ((existing-item (seq-find (lambda (item)
                                           (or (and id (equal (plist-get item :id) id))
                                               (equal (plist-get item :content) content)))
                                         gptelt-todo-list)))
            (if existing-item
                ;; update existing
                (setq gptelt-todo-list
                      (mapcar (lambda (item)
                                (if (or (and id (equal (plist-get item :id) id))
                                        (equal (plist-get item :content) content))
                                    new-item
                                  item))
                              gptelt-todo-list))
              ;; insert new
              (push new-item gptelt-todo-list)))))))
  (gptelt-todo--save)
  (let ((unfinished-count (gptelt-todo--count-unfinished)))
    (message "[%d] todos updated" unfinished-count)
    (when (= unfinished-count 0)
      (gptel-todo-clear-all)))
  "Todos have been modified successfully. Ensure that you continue to use the todo list to track your progress. Please proceed with the current tasks if applicable")

(comment
  (gptelt-todo-write '((:content "test task" :status "pending"))))

;;; API: todo_read
(defun gptelt-todo-read (&optional status priority)
  "Return the current todo list, optionally filtered.

STATUS: Optional filter by status (pending, in_progress, completed)
PRIORITY: Optional filter by priority (high, medium, low)

Returns a formatted string with progress indicator and filtered todos."
  (gptelt-todo--ensure-loaded)
  (if (seq-empty-p gptelt-todo-list)
      "No tasks"
    (let* ((filtered-list
            (seq-filter
             (lambda (item)
               (and (or (null status)
                        (eq (plist-get item :status)
                            (intern (downcase (format "%s" status)))))
                    (or (null priority)
                        (eq (plist-get item :priority)
                            (intern (downcase (format "%s" priority)))))))
             gptelt-todo-list))
           (total-count (length gptelt-todo-list))
           (completed-count (length (seq-filter
                                     (lambda (item)
                                       (eq (plist-get item :status) 'completed))
                                     gptelt-todo-list)))
           (filtered-count (length filtered-list)))
      (if (seq-empty-p filtered-list)
          (format "No tasks matching filters (status=%s, priority=%s)"
                  (or status "any") (or priority "any"))
        (concat
         (format "# Todo List (%d/%d completed, showing %d)\n\n"
                 completed-count total-count filtered-count)
         (mapconcat
          (lambda (item)
            (let ((status (plist-get item :status))
                  (priority (plist-get item :priority))
                  (content (plist-get item :content))
                  (tags (plist-get item :tags))
                  (created (plist-get item :created-at))
                  (completed (plist-get item :completed-at)))
              (format "%s [%s] %s%s%s"
                      (gptelt-todo--status-emoji status)
                      (upcase (symbol-name priority))
                      content
                      (if tags (format " [%s]" (mapconcat 'identity tags ", ")) "")
                      (if completed
                          (format " (completed: %s)"
                                  (format-time-string "%Y-%m-%d %H:%M" completed))
                        (if created
                            (format " (created: %s)"
                                    (format-time-string "%Y-%m-%d %H:%M" created))
                          "")))))
          filtered-list
          "\n"))))))

(comment
  (gptelt-todo-read))

(defun gptel-todo-clear-all ()
  "Clear all todos from the list."
  (interactive)
  (setq gptelt-todo-list nil)
  (gptelt-todo--save)
  (message "All todos cleared"))

(defun gptelt-todo-clear-completed ()
  "Remove all completed todos from the list."
  (gptelt-todo--ensure-loaded)
  (let ((completed-count (length (seq-filter
                                  (lambda (item)
                                    (eq (plist-get item :status) 'completed))
                                  gptelt-todo-list))))
    (setq gptelt-todo-list
          (seq-filter
           (lambda (item)
             (not (eq (plist-get item :status) 'completed)))
           gptelt-todo-list))
    (gptelt-todo--save)
    (format "Cleared %d completed task%s"
            completed-count
            (if (= completed-count 1) "" "s"))))

(defun gptelt-todo-clear-all-completed ()
  "Check all persisted .todo.el files and clear completed tasks in them."
  (interactive)
  (let ((todo-dir (expand-file-name "gptelt-todo" user-emacs-directory))
        (total-cleared 0))
    (unless (file-directory-p todo-dir)
      (error "Todo directory does not exist: %s" todo-dir))
    (dolist (file (directory-files todo-dir t "\\.todo\\.el$"))
      (let ((todo-list (with-temp-buffer
                         (insert-file-contents file)
                         (condition-case nil
                             (read (current-buffer))
                           (error nil)))))
        (when todo-list
          (let* ((completed-count (length (seq-filter
                                           (lambda (item)
                                             (eq (plist-get item :status) 'completed))
                                           todo-list)))
                 (filtered-list (seq-filter
                                 (lambda (item)
                                   (not (eq (plist-get item :status) 'completed)))
                                 todo-list)))
            (when (> completed-count 0)
              (with-current-buffer (find-file-noselect file)
                (erase-buffer)
                (prin1 filtered-list (current-buffer))
                (let ((inhibit-message t)) (save-buffer)))
              (setq total-cleared (+ total-cleared completed-count)))))))
    ;; (message "Cleared %d completed task%s from all todo files"
    ;;          total-cleared
    ;;          (if (= total-cleared 1) "" "s"))
    ))

(defvar gptelt-todo--idle-timer nil
  "Timer for auto-clearing completed todos.")

(defun gptelt-todo-setup-auto-clear ()
  "Setup idle timer to auto-clear completed todos after 30 minutes of idle time."
  (when gptelt-todo--idle-timer
    (cancel-timer gptelt-todo--idle-timer))
  (setq gptelt-todo--idle-timer
        (run-with-idle-timer (* 30 60) t #'gptelt-todo-clear-all-completed)))

(after! gptel
  (gptelt-todo-setup-auto-clear))

(defun gptelt-todo-cancel-auto-clear ()
  "Cancel the auto-clear idle timer."
  (interactive)
  (when gptelt-todo--idle-timer
    (cancel-timer gptelt-todo--idle-timer)
    (setq gptelt-todo--idle-timer nil)
    (message "Auto-clear completed todos disabled")))

(comment
  (gptelt-todo-clear-completed)
  (gptel-todo-clear-all))

(comment
  (gptelt-todo-write '((:content "task1" :status pending :priority low)
                       (:content "task2" :status pending :priority low)))
  (gptelt-todo-write [(:content "task1" :status pending :priority low)
                      (:content "task2" :status pending :priority low)])
  (gptelt-todo-read))

;;; Tool registration
(when (fboundp 'gptelt-make-tool)
  (gptelt-make-tool
   :name "todo_write"
   :function #'gptelt-todo-write
   :description (concat "Create or update todo entries from an array. Persists todos per project."
                        "\n\nPARAMETER STRUCTURE:\n"
                        "{\n"
                        "  \"todos\": array (required) - Array of todo objects\n"
                        "}\n\n"
                        "TODO OBJECT STRUCTURE:\n"
                        "{\n"
                        "  \"content\": \"string\" (required) - Todo description\n"
                        "  \"status\": \"string\" (optional, default: \"pending\") - One of: pending, in_progress, completed\n"
                        "  \"priority\": \"string\" (optional, default: \"medium\") - One of: high, medium, low\n"
                        "  \"activeForm\": \"string\" (optional) - Present continuous form (e.g., \"Running tests\")\n"
                        "  \"tags\": array of strings (optional) - Categorization tags\n"
                        "  \"id\": \"string\" (optional) - Todo ID for updating existing todo\n"
                        "}")
   :args '((:name "todos"
            :type array
            :items
            (:type object
             :properties
             (:content
              (:type string
               :description "Todo content string")
              :status
              (:type string
               :optional t
               :description "Status: pending, in_progress, completed")
              :priority
              (:type string
               :optional t
               :description "Priority: high, medium, low")
              :activeForm
              (:type string
               :optional t
               :description "More info about current task")
              :tags
              (:type array
               :items (:type string)
               :optional t
               :description "Array of tag strings for categorization")
              :id
              (:type string
               :optional t
               :description "Todo id to update (for updating instead of creating)"))
             :required ["content"]
             :description "Todo object with content (required), status (optional), priority (optional), id (optional)")
            :description "Array of todo objects"))
   :category "todo"
   :confirm nil
   :include t)
  (gptelt-make-tool
   :name "todo_read"
   :function #'gptelt-todo-read
   :description "Read the current todo list with optional filtering. Returns formatted output with progress indicator."
   :args '((:name "status"
            :type string
            :optional t
            :description "Filter by status: pending, in_progress, or completed")
           (:name "priority"
            :type string
            :optional t
            :description "Filter by priority: high, medium, or low"))
   :category "todo"
   :confirm nil
   :include t)
  (gptelt-make-tool
   :name "todo_clear_completed"
   :function #'gptelt-todo-clear-completed
   :description "Remove all completed todos from the list. Returns count of cleared tasks."
   :args '()
   :category "todo"
   :confirm nil
   :include t))

;;; todo.el ends here
