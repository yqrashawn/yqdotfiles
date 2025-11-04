;;; .nixpkgs/.doom.d/gptel-tools/todo-test.el -*- lexical-binding: t; -*-

(require 'ert)

(comment
  (ert "gptelt-todo-"))

(defvar gptelt-todo-test-backup nil
  "Backup of original todo list for test isolation.")

(defun gptelt-todo-test-setup ()
  "Set up test environment by backing up current todo list."
  (setq gptelt-todo-test-backup gptelt-todo-list)
  (setq gptelt-todo-list nil)
  (setq gptelt-todo--testing-mode t))

(defun gptelt-todo-test-teardown ()
  "Restore original todo list after test."
  (setq gptelt-todo-list gptelt-todo-test-backup)
  (setq gptelt-todo--testing-mode nil))

(ert-deftest gptelt-todo-write-basic-test ()
  "Test basic todo creation with gptelt-todo-write."
  (gptelt-todo-test-setup)
  (unwind-protect
      (progn
        (gptelt-todo-write '((:content "Test task")))
        (should (= (length gptelt-todo-list) 1))
        (let ((todo (car gptelt-todo-list)))
          (should (plist-get todo :id))
          (should (string= (plist-get todo :content) "Test task"))
          (should (eq (plist-get todo :status) 'pending))
          (should (eq (plist-get todo :priority) 'medium))))
    (gptelt-todo-test-teardown)))

(ert-deftest gptelt-todo-write-vector-test ()
  "Test todo creation with gptelt-todo-write using a vector input."
  (gptelt-todo-test-setup)
  (unwind-protect
      (progn
        (gptelt-todo-write [(:content "Vector Task 1")
                            (:content "Vector Task 2" :status "completed" :priority "high")])
        (should (= (length gptelt-todo-list) 2))
        (should (string= (plist-get (car gptelt-todo-list) :content) "Vector Task 2"))
        (should (eq (plist-get (car gptelt-todo-list) :status) 'completed))
        (should (eq (plist-get (car gptelt-todo-list) :priority) 'high))
        (should (string= (plist-get (cadr gptelt-todo-list) :content) "Vector Task 1"))
        (should (eq (plist-get (cadr gptelt-todo-list) :status) 'pending))
        (should (eq (plist-get (cadr gptelt-todo-list) :priority) 'medium)))
    (gptelt-todo-test-teardown)))

(ert-deftest gptelt-todo-write-with-status-test ()
  "Test todo creation with specific status."
  (gptelt-todo-test-setup)
  (unwind-protect
      (progn
        (gptelt-todo-write '((:content "In progress task" :status "in_progress" :priority "high")))
        (should (= (length gptelt-todo-list) 1))
        (let ((todo (car gptelt-todo-list)))
          (should (eq (plist-get todo :status) 'in_progress))
          (should (eq (plist-get todo :priority) 'high))))
    (gptelt-todo-test-teardown)))

(ert-deftest gptelt-todo-write-update-test ()
  "Test updating existing todo by ID."
  (gptelt-todo-test-setup)
  (unwind-protect
      (progn
        (gptelt-todo-write '((:content "Initial task")))
        (let* ((id (plist-get (car gptelt-todo-list) :id)))
          (gptelt-todo-write `((:content "Updated task" :status "in_progress" :priority "low" :id ,id)))
          (should (= (length gptelt-todo-list) 1))
          (let ((todo (car gptelt-todo-list)))
            (should (string= (plist-get todo :id) id))
            (should (string= (plist-get todo :content) "Updated task"))
            (should (eq (plist-get todo :status) 'in_progress)))))
    (gptelt-todo-test-teardown)))

(ert-deftest gptelt-todo-read-empty-test ()
  "Test reading empty todo list."
  (gptelt-todo-test-setup)
  (unwind-protect
      (let ((result (gptelt-todo-read)))
        (should (string= result "No tasks")))
    (gptelt-todo-test-teardown)))

(ert-deftest gptelt-todo-read-with-items-test ()
  "Test reading todo list with items."
  (gptelt-todo-test-setup)
  (unwind-protect
      (progn
        (gptelt-todo-write '((:content "Task 1")
                             (:content "Task 2" :status "in_progress")))
        (let ((result (gptelt-todo-read)))
          (should (stringp result))
          (should (string-match-p "Task 1" result))
          (should (string-match-p "Task 2" result))))
    (gptelt-todo-test-teardown)))

(ert-deftest gptelt-todo-auto-clear-test ()
  "Test auto-clear functionality when all tasks are completed."
  (gptelt-todo-test-setup)
  (unwind-protect
      (progn
        (gptelt-todo-write '((:content "Task 1")))
        (let* ((task1-id (plist-get (car gptelt-todo-list) :id)))
          (gptelt-todo-write `((:content "Task 1 completed" :status "completed" :priority "medium" :id ,task1-id)))
          (should (null gptelt-todo-list))))
    (gptelt-todo-test-teardown)))

(ert-deftest gptelt-todo-count-unfinished-test ()
  "Test counting unfinished todos."
  (gptelt-todo-test-setup)
  (unwind-protect
      (progn
        (gptelt-todo-write '((:content "Pending task" :status "pending")
                             (:content "In progress task" :status "in_progress")
                             (:content "Completed task" :status "completed")))
        (should (= (gptelt-todo--count-unfinished) 2)))
    (gptelt-todo-test-teardown)))

(ert-deftest gptelt-todo-invalid-status-test ()
  "Test error handling for invalid status."
  (gptelt-todo-test-setup)
  (unwind-protect
      (should-error (gptelt-todo-write '((:content "Test task" :status "invalid_status"))))
    (gptelt-todo-test-teardown)))

(ert-deftest gptelt-todo-invalid-priority-test ()
  "Test error handling for invalid priority."
  (gptelt-todo-test-setup)
  (unwind-protect
      (should-error (gptelt-todo-write '((:content "Test task" :status "pending" :priority "invalid_priority"))))
    (gptelt-todo-test-teardown)))

(ert-deftest gptelt-todo-empty-content-test ()
  "Test error handling for empty content."
  (gptelt-todo-test-setup)
  (unwind-protect
      (should-error (gptelt-todo-write '((:content ""))))
    (gptelt-todo-test-teardown)))

(ert-deftest gptelt-todo-read-with-filter-status-test ()
  "Test reading todo list filtered by status."
  (gptelt-todo-test-setup)
  (unwind-protect
      (progn
        (gptelt-todo-write '((:content "Pending task" :status "pending")
                             (:content "In progress task" :status "in_progress")
                             (:content "Completed task" :status "completed")))
        (let ((result (gptelt-todo-read "pending" nil)))
          (should (stringp result))
          (should (string-match-p "Pending task" result))
          (should-not (string-match-p "In progress task" result))
          (should-not (string-match-p "Completed task" result))))
    (gptelt-todo-test-teardown)))

(ert-deftest gptelt-todo-read-with-filter-priority-test ()
  "Test reading todo list filtered by priority."
  (gptelt-todo-test-setup)
  (unwind-protect
      (progn
        (gptelt-todo-write '((:content "High priority task" :priority "high")
                             (:content "Medium priority task" :priority "medium")
                             (:content "Low priority task" :priority "low")))
        (let ((result (gptelt-todo-read nil "high")))
          (should (stringp result))
          (should (string-match-p "High priority task" result))
          (should-not (string-match-p "Medium priority task" result))
          (should-not (string-match-p "Low priority task" result))))
    (gptelt-todo-test-teardown)))

(ert-deftest gptelt-todo-read-formatted-output-test ()
  "Test that todo_read returns formatted output with progress indicator."
  (gptelt-todo-test-setup)
  (unwind-protect
      (progn
        (gptelt-todo-write '((:content "Task 1" :status "pending")
                             (:content "Task 2" :status "completed")))
        (let ((result (gptelt-todo-read)))
          (should (stringp result))
          (should (string-match-p "# Todo List (1/2 completed, showing 2)" result))
          (should (string-match-p "Task 1" result))
          (should (string-match-p "Task 2" result))))
    (gptelt-todo-test-teardown)))

(ert-deftest gptelt-todo-clear-completed-test ()
  "Test clearing completed todos."
  (gptelt-todo-test-setup)
  (unwind-protect
      (progn
        (gptelt-todo-write '((:content "Pending task" :status "pending")
                             (:content "Completed task 1" :status "completed")
                             (:content "Completed task 2" :status "completed")))
        (should (= (length gptelt-todo-list) 3))
        (let ((result (gptelt-todo-clear-completed)))
          (should (stringp result))
          (should (string-match-p "Cleared 2 completed tasks" result))
          (should (= (length gptelt-todo-list) 1))
          (should (string= (plist-get (car gptelt-todo-list) :content) "Pending task"))))
    (gptelt-todo-test-teardown)))

(ert-deftest gptelt-todo-with-tags-test ()
  "Test todo creation with tags."
  (gptelt-todo-test-setup)
  (unwind-protect
      (progn
        (gptelt-todo-write '((:content "Tagged task" :tags ["bug" "urgent"])))
        (should (= (length gptelt-todo-list) 1))
        (let ((todo (car gptelt-todo-list)))
          (should (equal (plist-get todo :tags) ["bug" "urgent"]))))
    (gptelt-todo-test-teardown)))

(ert-deftest gptelt-todo-timestamps-test ()
  "Test that timestamps are created and updated correctly."
  (gptelt-todo-test-setup)
  (unwind-protect
      (progn
        (gptelt-todo-write '((:content "Task with timestamps") (:content "Another task")))
        (let* ((todo (car gptelt-todo-list))
               (created-at (plist-get todo :created-at))
               (updated-at (plist-get todo :updated-at)))
          (should (numberp created-at))
          (should (numberp updated-at))
          (should (= created-at updated-at))
          (should (null (plist-get todo :completed-at)))
          (sleep-for 0.1)
          (let* ((id (plist-get todo :id)))
            (gptelt-todo-write `((:content "Updated task" :status "in_progress" :id ,id)))
            (let ((updated-todo (seq-find (lambda (item) (string= (plist-get item :id) id)) gptelt-todo-list)))
              (should (= (plist-get updated-todo :created-at) created-at))
              (should (> (plist-get updated-todo :updated-at) updated-at))
              (should (null (plist-get updated-todo :completed-at)))))))
    (gptelt-todo-test-teardown)))

;;; todo-test.el ends here
