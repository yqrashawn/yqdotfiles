;;; .nixpkgs/.doom.d/gptel-tools/ask-user-question-test.el -*- lexical-binding: t; -*-

(require 'ert)

;;; Validation tests

(ert-deftest gptelt-auq-validate-nil-input ()
  "Validate nil input returns error."
  (let ((result (gptelt-auq--validate-input-parameters nil)))
    (should (null (car result)))
    (should (stringp (cdr result)))))

(ert-deftest gptelt-auq-validate-empty-input ()
  "Validate empty list returns error."
  (let ((result (gptelt-auq--validate-input-parameters '())))
    (should (null (car result)))
    (should (stringp (cdr result)))))

(ert-deftest gptelt-auq-validate-too-many-questions ()
  "Validate too many questions returns error."
  (let ((questions (make-list 6 '((:question . "q") (:options . (((:label . "a")) ((:label . "b"))))))))
    (let ((result (gptelt-auq--validate-input-parameters questions)))
      (should (null (car result)))
      (should (string-match-p "Too many" (cdr result))))))

(ert-deftest gptelt-auq-validate-valid-input ()
  "Validate valid input returns questions."
  (let* ((questions '(((:question . "Test?")
                       (:options . (((:label . "A")) ((:label . "B")))))))
         (result (gptelt-auq--validate-input-parameters questions)))
    (should (car result))
    (should (null (cdr result)))))

;;; Queue mechanism tests

(ert-deftest gptelt-auq-queue-enqueues-when-active ()
  "When active, new requests are queued instead of executed."
  (let ((gptelt-auq--active-p t)
        (gptelt-auq--queue nil)
        (callback-called nil))
    (gptelt-auq-ask-user-question
     (lambda (result) (setq callback-called result))
     '(((:question . "Test?")
        (:options . (((:label . "A")) ((:label . "B")))))))
    ;; Should be queued, not executed
    (should (= 1 (length gptelt-auq--queue)))
    (should (null callback-called))
    ;; Clean up
    (setq gptelt-auq--active-p nil
          gptelt-auq--queue nil)))

(ert-deftest gptelt-auq-queue-multiple-requests ()
  "Multiple requests while active are all queued in order."
  (let ((gptelt-auq--active-p t)
        (gptelt-auq--queue nil))
    (gptelt-auq-ask-user-question
     (lambda (_) nil)
     '(((:question . "First?")
        (:options . (((:label . "A")) ((:label . "B")))))))
    (gptelt-auq-ask-user-question
     (lambda (_) nil)
     '(((:question . "Second?")
        (:options . (((:label . "C")) ((:label . "D")))))))
    ;; Both should be queued
    (should (= 2 (length gptelt-auq--queue)))
    ;; FIFO order
    (should (string= "First?"
                     (cdr (assoc :question (car (cdr (car gptelt-auq--queue)))))))
    (should (string= "Second?"
                     (cdr (assoc :question (car (cdr (cadr gptelt-auq--queue)))))))
    ;; Clean up
    (setq gptelt-auq--active-p nil
          gptelt-auq--queue nil)))

(ert-deftest gptelt-auq-process-queue-clears-active ()
  "process-queue sets active to nil when queue is empty."
  (let ((gptelt-auq--active-p t)
        (gptelt-auq--queue nil))
    (gptelt-auq--process-queue)
    (should (null gptelt-auq--active-p))))

(ert-deftest gptelt-auq-validation-error-drains-queue ()
  "Validation errors call callback and drain queue."
  (let ((gptelt-auq--active-p nil)
        (gptelt-auq--queue nil)
        (first-result nil))
    ;; First request with invalid input (no questions)
    (gptelt-auq-ask-user-question
     (lambda (result) (setq first-result result))
     nil)
    ;; first-result should have error
    (should first-result)
    (let ((parsed (json-read-from-string first-result)))
      (should (alist-get 'isError parsed)))
    ;; active should be cleared
    (should (null gptelt-auq--active-p))))

(ert-deftest gptelt-auq-skip-no-pending ()
  "gptelt-auq-skip errors when no question is pending."
  (let ((gptelt-auq--pending nil))
    (should-error (gptelt-auq-skip) :type 'user-error)))

(ert-deftest gptelt-auq-skip-with-pending ()
  "gptelt-auq-skip sends error to callback and clears state.
When not inside recursive-edit, process-queue is called synchronously."
  (let* ((callback-result nil)
         (gptelt-auq--active-p t)
         (gptelt-auq--queue nil)
         (gptelt-auq--in-recursive-edit nil)
         (gptelt-auq--pending
          (list :callback (lambda (r) (setq callback-result r))
                :question-text "Test question?"
                :all-numbered '("1. A" "2. B")
                :numbered-to-orig '(("1. A" . "A") ("2. B" . "B"))
                :prompt "Select: "
                :multi-select nil
                :option-pairs '(("A" . "desc") ("B" . "desc"))
                :header "Test"
                :remaining-questions nil
                :results-so-far nil)))
    (gptelt-auq-skip)
    ;; Pending should be cleared
    (should (null gptelt-auq--pending))
    ;; Active should be cleared (process-queue was called synchronously)
    (should (null gptelt-auq--active-p))
    ;; Callback should have been called with error JSON
    (should (stringp callback-result))
    (let ((parsed (json-read-from-string callback-result)))
      (should (eq t (alist-get 'isError parsed))))))

(ert-deftest gptelt-auq-queue-status-reports ()
  "queue-status reports correct state."
  (let ((gptelt-auq--active-p nil)
        (gptelt-auq--queue nil)
        (gptelt-auq--pending nil))
    (should (string= (gptelt-auq-queue-status)
                     "AUQ: active=no, pending=no, queued=0"))))

(ert-deftest gptelt-auq-finish-drains-queue ()
  "finish-with-results calls callback and drains queue (non-recursive-edit)."
  (let ((gptelt-auq--active-p t)
        (gptelt-auq--queue nil)
        (gptelt-auq--in-recursive-edit nil)
        (finish-result nil))
    (gptelt-auq--finish-with-results
     '(("Q?" . "A"))
     (lambda (r) (setq finish-result r)))
    ;; Should have result
    (should finish-result)
    (let ((parsed (json-read-from-string finish-result)))
      (should (alist-get 'answers parsed)))
    ;; Active should be cleared (synchronous process-queue)
    (should (null gptelt-auq--active-p))))

(ert-deftest gptelt-auq-finish-uses-timer-in-recursive-edit ()
  "finish-with-results uses timer for queue processing in recursive-edit."
  (let ((gptelt-auq--active-p t)
        (gptelt-auq--queue nil)
        (gptelt-auq--in-recursive-edit t)
        (finish-result nil))
    (gptelt-auq--finish-with-results
     '(("Q?" . "A"))
     (lambda (r) (setq finish-result r)))
    ;; Callback should have been called
    (should finish-result)
    ;; Active should still be t because process-queue was deferred to timer
    (should gptelt-auq--active-p)
    ;; Clean up
    (setq gptelt-auq--active-p nil
          gptelt-auq--in-recursive-edit nil)))

;;; Defer-by-default tests

(ert-deftest gptelt-auq-defer-by-default-stores-pending ()
  "When defer-by-default is t, question is stored in pending without prompting.
We mock recursive-edit to prevent actually entering it in tests."
  (let ((gptelt-auq-defer-by-default t)
        (gptelt-auq--active-p nil)
        (gptelt-auq--pending nil)
        (gptelt-auq--queue nil)
        (gptelt-auq--in-recursive-edit nil)
        (callback-called nil)
        (recursive-edit-entered nil))
    ;; Mock recursive-edit to just set a flag instead of blocking
    (cl-letf (((symbol-function 'recursive-edit)
               (lambda () (setq recursive-edit-entered t))))
      (gptelt-auq-ask-user-question
       (lambda (r) (setq callback-called r))
       '(((:question . "Test?")
          (:header . "H")
          (:options . (((:label . "A") (:description . "desc A"))
                       ((:label . "B") (:description . "desc B"))))))))
    ;; Should be deferred
    (should gptelt-auq--pending)
    (should (string= "Test?" (plist-get gptelt-auq--pending :question-text)))
    ;; Callback should NOT have been called yet
    (should (null callback-called))
    ;; recursive-edit should have been entered
    (should recursive-edit-entered)
    ;; in-recursive-edit flag should be cleaned up by unwind-protect
    (should (null gptelt-auq--in-recursive-edit))
    ;; Clean up
    (setq gptelt-auq--active-p nil
          gptelt-auq--pending nil)))

(ert-deftest gptelt-auq-defer-off-does-not-store-pending ()
  "When defer-by-default is nil, pending is not set before completing-read.
We can't test completing-read itself, so we just verify that with
defer off and a validation error, pending stays nil."
  (let ((gptelt-auq-defer-by-default nil)
        (gptelt-auq--active-p nil)
        (gptelt-auq--pending nil)
        (gptelt-auq--queue nil)
        (error-result nil))
    ;; Invalid input - no questions
    (gptelt-auq-ask-user-question
     (lambda (r) (setq error-result r))
     nil)
    ;; Pending should still be nil (error path, no defer)
    (should (null gptelt-auq--pending))
    ;; Error callback should have fired
    (should error-result)))

(ert-deftest gptelt-auq-recursive-edit-not-nested ()
  "When already in recursive-edit, defer path skips entering another one."
  (let ((gptelt-auq-defer-by-default t)
        (gptelt-auq--active-p nil)
        (gptelt-auq--pending nil)
        (gptelt-auq--queue nil)
        (gptelt-auq--in-recursive-edit t) ; already in recursive-edit
        (recursive-edit-count 0))
    (cl-letf (((symbol-function 'recursive-edit)
               (lambda () (cl-incf recursive-edit-count))))
      (gptelt-auq-ask-user-question
       (lambda (_r) nil)
       '(((:question . "Nested test?")
          (:options . (((:label . "A")) ((:label . "B"))))))))
    ;; recursive-edit should NOT have been called again
    (should (= 0 recursive-edit-count))
    ;; But pending should still be stored
    (should gptelt-auq--pending)
    ;; Clean up
    (setq gptelt-auq--active-p nil
          gptelt-auq--pending nil
          gptelt-auq--in-recursive-edit nil)))

;;; ask-user-question-test.el ends here
