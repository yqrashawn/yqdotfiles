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
        ;; Unbind async-response-fn to simulate stdio transport,
        ;; otherwise the HTTP binding from the current session
        ;; would cause recursive-edit to be skipped.
        (mcp-server-lib--async-response-fn nil)
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

(ert-deftest gptelt-auq-http-transport-skips-recursive-edit ()
  "When mcp-server-lib--async-response-fn is bound (HTTP transport),
recursive-edit is skipped so the call stack unwinds and the HTTP handler
can return :async-pending and start chunked keepalive."
  (let ((gptelt-auq-defer-by-default t)
        (gptelt-auq--active-p nil)
        (gptelt-auq--pending nil)
        (gptelt-auq--queue nil)
        (gptelt-auq--in-recursive-edit nil)
        (recursive-edit-count 0)
        ;; Simulate HTTP transport by binding the async response fn
        (mcp-server-lib--async-response-fn (lambda (_response) nil)))
    (cl-letf (((symbol-function 'recursive-edit)
               (lambda () (cl-incf recursive-edit-count))))
      (gptelt-auq-ask-user-question
       (lambda (_r) nil)
       '(((:question . "HTTP test?")
          (:options . (((:label . "A")) ((:label . "B"))))))))
    ;; recursive-edit should NOT have been called
    (should (= 0 recursive-edit-count))
    ;; Pending should still be stored for the buffer UI
    (should gptelt-auq--pending)
    (should (string= "HTTP test?" (plist-get gptelt-auq--pending :question-text)))
    ;; in-recursive-edit should remain nil (never entered)
    (should (null gptelt-auq--in-recursive-edit))
    ;; Clean up
    (setq gptelt-auq--active-p nil
          gptelt-auq--pending nil)))

;;; Interactive buffer tests

(ert-deftest gptelt-auq-show-help-buffer-creates-buttons ()
  "show-help-buffer creates clickable buttons for each option."
  (let ((gptelt-auq--pending nil))
    (gptelt-auq--show-help-buffer "Test?" "H" '(("A" . "desc A") ("B" . "desc B")))
    (with-current-buffer gptelt-auq--help-buffer-name
      ;; Buffer should have the minor mode enabled
      (should gptelt-auq-buffer-mode)
      ;; Should contain button text
      (goto-char (point-min))
      (should (search-forward "1. A" nil t))
      (should (search-forward "2. B" nil t))
      (should (search-forward "3. Other" nil t))
      ;; Should show key hints
      (goto-char (point-min))
      (should (search-forward "[1-N] select" nil t)))
    (gptelt-auq--cleanup-help-buffer)))

(ert-deftest gptelt-auq-show-help-buffer-multi-select ()
  "show-help-buffer shows checkboxes in multi-select mode."
  (let ((gptelt-auq--pending nil))
    (gptelt-auq--show-help-buffer "Pick?" "H" '(("X" . "") ("Y" . "")) t)
    (with-current-buffer gptelt-auq--help-buffer-name
      (goto-char (point-min))
      (should (search-forward "[ ]" nil t))
      (should (search-forward "toggle" nil t)))
    (gptelt-auq--cleanup-help-buffer)))

(ert-deftest gptelt-auq-select-option-no-pending ()
  "select-option errors when no question is pending."
  (let ((gptelt-auq--pending nil))
    (should-error (gptelt-auq--select-option 1) :type 'user-error)))

(ert-deftest gptelt-auq-select-option-out-of-range ()
  "select-option errors for out-of-range index."
  (let ((gptelt-auq--pending
         (list :option-pairs '(("A" . "") ("B" . ""))
               :multi-select nil)))
    ;; max valid is 3 (2 options + Other)
    (should-error (gptelt-auq--select-option 4) :type 'user-error)
    (should-error (gptelt-auq--select-option 0) :type 'user-error)
    (setq gptelt-auq--pending nil)))

(ert-deftest gptelt-auq-answer-with-index-single-select ()
  "answer-with-index selects the correct option and calls callback."
  (let* ((callback-result nil)
         (gptelt-auq--active-p t)
         (gptelt-auq--queue nil)
         (gptelt-auq--in-recursive-edit nil)
         (gptelt-auq--pending
          (list :option-pairs '(("Alpha" . "first") ("Beta" . "second"))
                :question-text "Choose?"
                :callback (lambda (r) (setq callback-result r))
                :remaining-questions nil
                :results-so-far nil)))
    (gptelt-auq--answer-with-index 2)
    ;; Pending should be cleared
    (should (null gptelt-auq--pending))
    ;; Callback should have been called with "Beta"
    (should callback-result)
    (let ((parsed (json-read-from-string callback-result)))
      (should (string= "Beta" (alist-get (intern "Choose?") (alist-get 'answers parsed)))))
    (setq gptelt-auq--active-p nil)))

(ert-deftest gptelt-auq-toggle-multi-select ()
  "toggle-multi-select toggles option indices in buffer-local var."
  (let* ((gptelt-auq--pending
          (list :option-pairs '(("A" . "") ("B" . ""))
                :multi-select t
                :question-text "Pick?"
                :header "H"
                :callback (lambda (_) nil)
                :remaining-questions nil
                :results-so-far nil)))
    ;; Create the buffer with multi-select mode
    (gptelt-auq--show-help-buffer "Pick?" "H" '(("A" . "") ("B" . "")) t)
    (with-current-buffer gptelt-auq--help-buffer-name
      ;; Initially empty
      (should (null gptelt-auq--multi-selected))
      ;; Toggle option 1 on
      (gptelt-auq--toggle-multi-select 1)
      (should (memq 1 gptelt-auq--multi-selected))
      ;; Toggle option 2 on
      (gptelt-auq--toggle-multi-select 2)
      (should (memq 2 gptelt-auq--multi-selected))
      ;; Toggle option 1 off
      (gptelt-auq--toggle-multi-select 1)
      (should (null (memq 1 gptelt-auq--multi-selected)))
      (should (memq 2 gptelt-auq--multi-selected)))
    (gptelt-auq--cleanup-help-buffer)
    (setq gptelt-auq--pending nil)))

(ert-deftest gptelt-auq-confirm-selection-no-pending ()
  "confirm-selection errors when no question is pending."
  (let ((gptelt-auq--pending nil))
    (should-error (gptelt-auq--confirm-selection) :type 'user-error)))

(ert-deftest gptelt-auq-confirm-selection-single-select ()
  "confirm-selection errors in single-select mode."
  (let ((gptelt-auq--pending
         (list :multi-select nil
               :option-pairs '(("A" . "")))))
    (should-error (gptelt-auq--confirm-selection) :type 'user-error)
    (setq gptelt-auq--pending nil)))

(ert-deftest gptelt-auq-confirm-multi-select-submits ()
  "confirm-selection submits toggled options in multi-select mode."
  (let* ((callback-result nil)
         (gptelt-auq--active-p t)
         (gptelt-auq--queue nil)
         (gptelt-auq--in-recursive-edit nil)
         (gptelt-auq--pending
          (list :option-pairs '(("X" . "") ("Y" . "") ("Z" . ""))
                :multi-select t
                :question-text "Features?"
                :header "H"
                :callback (lambda (r) (setq callback-result r))
                :remaining-questions nil
                :results-so-far nil)))
    (gptelt-auq--show-help-buffer "Features?" "H"
                                  '(("X" . "") ("Y" . "") ("Z" . "")) t)
    (with-current-buffer gptelt-auq--help-buffer-name
      ;; Toggle X and Z
      (gptelt-auq--toggle-multi-select 1)
      (gptelt-auq--toggle-multi-select 3))
    (gptelt-auq--confirm-selection)
    ;; Should have result with "X, Z"
    (should callback-result)
    (let ((parsed (json-read-from-string callback-result)))
      (should (string= "X, Z"
                        (alist-get (intern "Features?") (alist-get 'answers parsed)))))
    (setq gptelt-auq--active-p nil
          gptelt-auq--pending nil)))

;;; ask-user-question-test.el ends here
