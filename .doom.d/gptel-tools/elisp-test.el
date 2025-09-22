;;; elisp-test.el --- GPTEL Elisp test suite -*- lexical-binding: t; -*-
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))
;; Homepage: https://github.com/yqrashawn/gptel-tools
;; Keywords: tools, test

;;; Commentary:
;;
;; Test suite for elisp utilities in GPTEL.
;;
;;; Code:

(require 'ert)

(ert-deftest gptelt-evaluate-elisp-buffer-test ()
  "Test gptelt-evaluate-elisp-buffer sets value in a test buffer."
  (let* ((test-buffer (generate-new-buffer "gptelt-eval-buffer-test")))
    (unwind-protect
        (progn
          (with-current-buffer test-buffer
            (emacs-lisp-mode)
            (insert "(setq gptelt-eval-buffer-test-value 42)"))
          ;; Simulate evaluation (skip prompt for test)
          (let ((old-yn (symbol-function 'y-or-n-p)))
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (gptelt-evaluate-elisp-buffer (buffer-name test-buffer))))
          (should (and (boundp 'gptelt-eval-buffer-test-value)
                       (= gptelt-eval-buffer-test-value 42))))
      (when (boundp 'gptelt-eval-buffer-test-value)
        (makunbound 'gptelt-eval-buffer-test-value))
      (kill-buffer test-buffer))))

(ert-deftest gptelt-evaluate-elisp-buffer-error-test ()
  "Test gptelt-evaluate-elisp-buffer returns error string on evaluation error."
  (let* ((test-buffer (generate-new-buffer "gptelt-eval-buffer-err-test")))
    (unwind-protect
        (progn
          (with-current-buffer test-buffer
            (emacs-lisp-mode)
            (insert "(this-is-not-a-function-call)"))
          (let ((old-yn (symbol-function 'y-or-n-p))
                (result nil))
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (setq result (gptelt-evaluate-elisp-buffer (buffer-name test-buffer))))
            (should (and (stringp result)
                         (string-match-p "Error evaluate buffer" result)))))
      (kill-buffer test-buffer))))

(ert-deftest gptelt-run-ert-test-basic ()
  "Test gptelt-run-ert-test output includes all key event lines for a passing test."
  ;; Define a minimal test if not present
  (let ((test-name 'gptelt-ert-smoke-pass))
    (unless (ert-test-boundp test-name)
      (eval '(ert-deftest gptelt-ert-smoke-pass () (should (= 1 1)))))
    (let ((output (gptelt-run-ert-test "gptelt-ert-smoke-pass")))
      (should (string-match-p "Running \\([0-9]+\\) tests" output))
      (should (string-match-p "Started: gptelt-ert-smoke-pass" output))
      (should (string-match-p "Ended:   gptelt-ert-smoke-pass" output))
      (should (string-match-p "passed.*gptelt-ert-smoke-pass" output))
      (should (string-match-p "Ran \\([0-9]+\\) tests" output)))))

(ert-deftest gptelt-evaluate-elisp-file-test ()
  "Test gptelt-evaluate-elisp-file sets value in a temp file."
  (let* ((file-path (make-temp-file "gptelt-eval-file-test" nil ".el"))
         (content "(setq gptelt-eval-file-test-value 99)")
         (_ (with-temp-file file-path (insert content))))
    (unwind-protect
        (progn
          (let ((old-yn (symbol-function 'y-or-n-p)))
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (gptelt-evaluate-elisp-file file-path)))
          (should (and (boundp 'gptelt-eval-file-test-value)
                       (= gptelt-eval-file-test-value 99))))
      (when (boundp 'gptelt-eval-file-test-value)
        (makunbound 'gptelt-eval-file-test-value))
      (when (file-exists-p file-path) (delete-file file-path)))))

(ert-deftest gptelt-evaluate-elisp-string-test ()
  "Test gptelt-evaluate-elisp-string sets value using elisp string."
  (let* ((elisp-string "(setq gptelt-eval-string-test-value 123)")
         (result nil))
    (unwind-protect
        (progn
          (let ((old-yn (symbol-function 'y-or-n-p)))
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (setq result (gptelt-evaluate-elisp-string elisp-string))))
          (should (and (boundp 'gptelt-eval-string-test-value)
                       (= gptelt-eval-string-test-value 123)))
          (should (string-match-p "Result: " result)))
      (when (boundp 'gptelt-eval-string-test-value)
        (makunbound 'gptelt-eval-string-test-value)))))

(ert-deftest gptelt-evaluate-elisp-string-error-test ()
  "Test gptelt-evaluate-elisp-string error result for bad code."
  (let* ((elisp-string "(this-is-not-a-symbol)")
         (result nil))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (setq result (gptelt-evaluate-elisp-string elisp-string)))
    (should (and (stringp result)
                 (string-match-p "Error evaluating elisp string" result)))))
