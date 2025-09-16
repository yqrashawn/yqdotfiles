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
      (let ((result (gptelt-todo-write '((:content "Test task")))))
        (should (listp result))
        (should (= (length result) 1))
        (let ((todo (car result)))
          (should (plist-get todo :id))
          (should (string= (plist-get todo :content) "Test task"))
          (should (eq (plist-get todo :status) 'pending))
          (should (eq (plist-get todo :priority) 'medium)))
        (should (= (length gptelt-todo-list) 1)))
    (gptelt-todo-test-teardown)))

(ert-deftest gptelt-todo-write-with-status-test ()
  "Test todo creation with specific status."
  (gptelt-todo-test-setup)
  (unwind-protect
      (let ((result (gptelt-todo-write '((:content "In progress task" :status "in_progress" :priority "high")))))
        (should (listp result))
        (should (= (length result) 1))
        (let ((todo (car result)))
          (should (eq (plist-get todo :status) 'in_progress))
          (should (eq (plist-get todo :priority) 'high))))
    (gptelt-todo-test-teardown)))

(ert-deftest gptelt-todo-write-update-test ()
  "Test updating existing todo by ID."
  (gptelt-todo-test-setup)
  (unwind-protect
      (let* ((initial (gptelt-todo-write '((:content "Initial task"))))
             (id (plist-get (car initial) :id))
             (updated (gptelt-todo-write `((:content "Updated task" :status "in_progress" :priority "low" :id ,id)))))
        (should (= (length updated) 1))
        (let ((todo (car updated)))
          (should (string= (plist-get todo :id) id))
          (should (string= (plist-get todo :content) "Updated task"))
          (should (eq (plist-get todo :status) 'in_progress)))
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
        (gptelt-todo-write '((:content "Task 1")
                             (:content "Task 2" :status "in_progress")))
        (let ((result (gptelt-todo-read)))
          (should (listp result))
          (should (= (length result) 2))))
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
