;;; .nixpkgs/.doom.d/gptel-tools/todo.el -*- lexical-binding: t; coding: utf-8 -*-

;;; Todo tool for GPTEL inspired by Claude Code's builtin todo tool

(defvar gptelt-todo-list nil
  "The current todo list as an elisp list of plists.
Each todo is a plist: (:id ID :content CONTENT :status STATUS :priority PRIORITY)")

(defun gptelt-todo--project-name ()
  "Get current project name for todo persistence."
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
                  (read (current-buffer)))
              nil)))))

(defun gptelt-todo--ensure-loaded ()
  ;; (unless gptelt-todo-list
  ;;   (gptelt-todo--load))
  (gptelt-todo--load))

(defun gptelt-todo--make-id ()
  "Generate a new unique todo id."
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
  "Add or update todo items from a list.
TODOS: list of plists, each with :content (required), :status (optional), :priority (optional), :id (optional).
Returns the updated todo list."
  (gptelt-todo--ensure-loaded)
  (unless (listp todos)
    (error "Todos must be a list"))
  (dolist (todo todos)
    (unless (plistp todo)
      (error "Each todo must be a plist"))
    (let ((content (plist-get todo :content))
          (status (plist-get todo :status))
          (priority (plist-get todo :priority))
          (id (plist-get todo :id)))
      (unless (and content (stringp content) (> (length content) 0))
        (error "Todo content must be non-empty string"))
      (let ((status (or (and status (intern (downcase (format "%s" status)))) 'pending))
            (priority (or (and priority (intern (downcase (format "%s" priority)))) 'medium)))
        (unless (memq status gptelt-todo-valid-status)
          (error "Invalid todo status: %s" status))
        (unless (memq priority gptelt-todo-valid-priority)
          (error "Invalid todo priority: %s" priority))
        (let ((new-id (or id (gptelt-todo--make-id)))
              (new-item (list :id (or id (gptelt-todo--make-id)) :content content :status status :priority priority)))
          (if id
              ;; update existing
              (setq gptelt-todo-list
                    (mapcar (lambda (item)
                              (if (equal (plist-get item :id) id)
                                  new-item
                                item))
                            gptelt-todo-list))
            ;; insert new
            (push new-item gptelt-todo-list))))))
  (gptelt-todo--save)
  (let ((unfinished-count (gptelt-todo--count-unfinished)))
    (message "[%d] todos updated" unfinished-count)
    (when (= unfinished-count 0)
      (gptel-todo-clear-all)))
  gptelt-todo-list)

(comment
  (gptelt-todo-write '((:content "test task" :status pending))))

;;; API: todo_read
(defun gptelt-todo-read ()
  "Return the current todo list as a lisp list (each item is a plist)."
  (gptelt-todo--ensure-loaded)
  (if (seq-empty-p gptelt-todo-list)
      "No task"
    gptelt-todo-list))

(comment
  (gptelt-todo-read))

(defun gptel-todo-clear-all ()
  "Clear all todos from the list."
  (interactive)
  (setq gptelt-todo-list nil)
  (gptelt-todo--save)
  (message "All todos cleared"))

(comment
  (gptel-todo-clear-all))

(comment
  (gptelt-todo-write '((:content "task1" :status pending :priority low)
                       (:content "task2" :status pending :priority low)))
  (gptelt-todo-read))

;;; Tool registration
(when (fboundp 'gptelt-make-tool)
  (gptelt-make-tool
   :name "todo_write"
   :function #'gptelt-todo-write
   :description "Create or update todo entries from an array. Params: todos (array of objects with content, status, priority, id). Returns the updated todo list."
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
   :description "Read the current todo list. Returns a lisp list of plists."
   :args '()
   :category "todo"
   :confirm nil
   :include t))
