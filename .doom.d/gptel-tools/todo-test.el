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
      (let ((result (gptelt-todo-write "Test task")))
        (should (plist-get result :id))
        (should (string= (plist-get result :content) "Test task"))
        (should (eq (plist-get result :status) 'pending))
        (should (eq (plist-get result :priority) 'medium))
        (should (= (length gptelt-todo-list) 1)))
    (gptelt-todo-test-teardown)))

(ert-deftest gptelt-todo-write-with-status-test ()
  "Test todo creation with specific status."
  (gptelt-todo-test-setup)
  (unwind-protect
      (let ((result (gptelt-todo-write "In progress task" "in_progress" "high")))
        (should (eq (plist-get result :status) 'in_progress))
        (should (eq (plist-get result :priority) 'high)))
    (gptelt-todo-test-teardown)))

(ert-deftest gptelt-todo-write-update-test ()
  "Test updating existing todo by ID."
  (gptelt-todo-test-setup)
  (unwind-protect
      (let* ((initial (gptelt-todo-write "Initial task"))
             (id (plist-get initial :id))
             (updated (gptelt-todo-write "Updated task" "in_progress" "low" id)))
        (should (string= (plist-get updated :id) id))
        (should (string= (plist-get updated :content) "Updated task"))
        (should (eq (plist-get updated :status) 'in_progress))
        (should (= (length gptelt-todo-list) 1)))
    (gptelt-todo-test-teardown)))

(ert-deftest gptelt-todo-read-empty-test ()
  "Test reading empty todo list."
  (gptelt-todo-test-setup)
  (unwind-protect
      (let ((result (gptelt-todo-read)))
        (should (string= result "No task")))
    (gptelt-todo-test-teardown)))

(ert-deftest gptelt-todo-read-with-items-test ()
  "Test reading todo list with items."
  (gptelt-todo-test-setup)
  (unwind-protect
      (progn
        (gptelt-todo-write "Task 1")
        (gptelt-todo-write "Task 2" "in_progress")
        (let ((result (gptelt-todo-read)))
          (should (listp result))
          (should (= (length result) 2))))
    (gptelt-todo-test-teardown)))

(ert-deftest gptelt-todo-auto-clear-test ()
  "Test auto-clear functionality when all tasks are completed."
  (gptelt-todo-test-setup)
  (unwind-protect
      (progn
        (gptelt-todo-write "Task 1")
        (let* ((task1-id (plist-get (car gptelt-todo-list) :id)))
          (gptelt-todo-write "Task 1 completed" "completed" "medium" task1-id)
          (should (null gptelt-todo-list))))
    (gptelt-todo-test-teardown)))

(ert-deftest gptelt-todo-count-unfinished-test ()
  "Test counting unfinished todos."
  (gptelt-todo-test-setup)
  (unwind-protect
      (progn
        (gptelt-todo-write "Pending task" "pending")
        (gptelt-todo-write "In progress task" "in_progress")
        (gptelt-todo-write "Completed task" "completed")
        (should (= (gptelt-todo--count-unfinished) 2)))
    (gptelt-todo-test-teardown)))

(ert-deftest gptelt-todo-invalid-status-test ()
  "Test error handling for invalid status."
  (gptelt-todo-test-setup)
  (unwind-protect
      (should-error (gptelt-todo-write "Test task" "invalid_status"))
    (gptelt-todo-test-teardown)))

(ert-deftest gptelt-todo-invalid-priority-test ()
  "Test error handling for invalid priority."
  (gptelt-todo-test-setup)
  (unwind-protect
      (should-error (gptelt-todo-write "Test task" "pending" "invalid_priority"))
    (gptelt-todo-test-teardown)))

(ert-deftest gptelt-todo-empty-content-test ()
  "Test error handling for empty content."
  (gptelt-todo-test-setup)
  (unwind-protect
      (should-error (gptelt-todo-write ""))
    (gptelt-todo-test-teardown)))
