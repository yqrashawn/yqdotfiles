;;; .nixpkgs/.doom.d/gptel-tools/elisp-test.el -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest gptel-tools-evaluate-elisp-buffer-test ()
  "Test gptel-evaluate-elisp-buffer sets value in a test buffer."
  (let* ((test-buffer (generate-new-buffer "gptel-eval-buffer-test")))
    (unwind-protect
        (progn
          (with-current-buffer test-buffer
            (emacs-lisp-mode)
            (insert "(setq gptel-eval-buffer-test-value 42)"))
          ;; Simulate evaluation (skip prompt for test)
          (let ((old-yn (symbol-function 'y-or-n-p)))
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (gptel-evaluate-elisp-buffer (buffer-name test-buffer))))
          (should (and (boundp 'gptel-eval-buffer-test-value)
                       (= gptel-eval-buffer-test-value 42))))
      (when (boundp 'gptel-eval-buffer-test-value)
        (makunbound 'gptel-eval-buffer-test-value))
      (kill-buffer test-buffer))))

(ert-deftest gptel-tools-evaluate-elisp-buffer-error-test ()
  "Test gptel-evaluate-elisp-buffer returns error string on evaluation error."
  (let* ((test-buffer (generate-new-buffer "gptel-eval-buffer-err-test")))
    (unwind-protect
        (progn
          (with-current-buffer test-buffer
            (emacs-lisp-mode)
            (insert "(this-is-not-a-function-call)"))
          (let ((old-yn (symbol-function 'y-or-n-p))
                (result nil))
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (setq result (gptel-evaluate-elisp-buffer (buffer-name test-buffer))))
            (should (and (stringp result)
                         (string-match-p "Error evaluate buffer" result)))))
      (kill-buffer test-buffer))))

(ert-deftest gptel-tools-run-ert-test-basic ()
  "Test gptel-run-ert-test output includes all key event lines for a passing test."
  ;; Define a minimal test if not present
  (let ((test-name 'gptel-ert-smoke-pass))
    (unless (ert-test-boundp test-name)
      (eval '(ert-deftest gptel-ert-smoke-pass () (should (= 1 1)))))
    (let ((output (gptel-run-ert-test "gptel-ert-smoke-pass")))
      (should (string-match-p "Running \\([0-9]+\\) tests" output))
      (should (string-match-p "Started: gptel-ert-smoke-pass" output))
      (should (string-match-p "Ended:   gptel-ert-smoke-pass" output))
      (should (string-match-p "passed.*gptel-ert-smoke-pass" output))
      (should (string-match-p "Ran \\([0-9]+\\) tests" output)))))

(ert-deftest gptel-tools-evaluate-elisp-file-test ()
  "Test gptel-evaluate-elisp-file sets value in a temp file."
  (let* ((file-path (make-temp-file "gptel-eval-file-test" nil ".el"))
         (content "(setq gptel-eval-file-test-value 99)")
         (_ (with-temp-file file-path (insert content))))
    (unwind-protect
        (progn
          (let ((old-yn (symbol-function 'y-or-n-p)))
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (gptel-evaluate-elisp-file file-path)))
          (should (and (boundp 'gptel-eval-file-test-value)
                       (= gptel-eval-file-test-value 99))))
      (when (boundp 'gptel-eval-file-test-value)
        (makunbound 'gptel-eval-file-test-value))
      (when (file-exists-p file-path) (delete-file file-path)))))

(ert-deftest gptel-tools-evaluate-elisp-string-test ()
  "Test gptel-evaluate-elisp-string sets value using elisp string."
  (let* ((elisp-string "(setq gptel-eval-string-test-value 123)")
         (result nil))
    (unwind-protect
        (progn
          (let ((old-yn (symbol-function 'y-or-n-p)))
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (setq result (gptel-evaluate-elisp-string elisp-string))))
          (should (and (boundp 'gptel-eval-string-test-value)
                       (= gptel-eval-string-test-value 123)))
          (should (string-match-p "Result: " result)))
      (when (boundp 'gptel-eval-string-test-value)
        (makunbound 'gptel-eval-string-test-value)))))

(ert-deftest gptel-tools-evaluate-elisp-string-error-test ()
  "Test gptel-evaluate-elisp-string error result for bad code."
  (let* ((elisp-string "(this-is-not-a-symbol)")
         (result nil))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (setq result (gptel-evaluate-elisp-string elisp-string)))
    (should (and (stringp result)
                 (string-match-p "Error evaluating elisp string" result)))))
