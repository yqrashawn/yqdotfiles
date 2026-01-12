;;; .nixpkgs/.doom.d/gptel-tools/shell-test.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for gptel-tools/shell

;;; Code:

(require 'ert)
;; shell.el is loaded by evaluating it or via main gptel-tools config

;;; Test Helpers

(defvar gptelt-shell-test--done nil
  "Flag indicating async test completion.")

(defvar gptelt-shell-test--result nil
  "Result from async test.")

(defun gptelt-shell-test-wait (timeout)
  "Wait for async test to complete within TIMEOUT seconds."
  (let ((start-time (float-time)))
    (while (and (not gptelt-shell-test--done)
                (< (- (float-time) start-time) timeout))
      (accept-process-output nil 0.05)
      (sleep-for 0.05)))
  (unless gptelt-shell-test--done
    (error "Test timed out after %d seconds" timeout))
  gptelt-shell-test--result)

(defun gptelt-shell-test-run (callback-fn timeout)
  "Run async test with CALLBACK-FN and wait up to TIMEOUT seconds."
  (setq gptelt-shell-test--done nil
        gptelt-shell-test--result nil)
  (funcall callback-fn
           (lambda (result)
             (setq gptelt-shell-test--result result
                   gptelt-shell-test--done t)))
  (gptelt-shell-test-wait timeout))

;;; Tests

(ert-deftest gptelt-shell-test-basic-command ()
  "Test basic command execution."
  (let ((result (gptelt-shell-test-run
                 (lambda (cb) (gptelt-run-shell-command cb "echo 'hello world'"))
                 10)))
    (should (stringp result))
    (should (string-match-p "Exit code: 0" result))
    (should (string-match-p "hello world" result))))

(ert-deftest gptelt-shell-test-exit-code ()
  "Test that non-zero exit codes are captured."
  (let ((result (gptelt-shell-test-run
                 (lambda (cb) (gptelt-run-shell-command cb "exit 42"))
                 10)))
    (should (string-match-p "Exit code: 42" result))))

(ert-deftest gptelt-shell-test-stderr ()
  "Test that stderr is captured in output."
  (let ((result (gptelt-shell-test-run
                 (lambda (cb) (gptelt-run-shell-command 
                               cb 
                               "echo 'stdout'; echo 'stderr' >&2"))
                 10)))
    (should (string-match-p "stdout" result))
    (should (string-match-p "stderr" result))))

(ert-deftest gptelt-shell-test-working-directory ()
  "Test that working directory parameter works."
  (let ((result (gptelt-shell-test-run
                 (lambda (cb) (gptelt-run-shell-command cb "pwd" "/tmp"))
                 10)))
    (should (string-match-p "Working directory: /tmp" result))
    ;; The pwd output should also show /tmp (or /private/tmp on macOS)
    (should (string-match-p "/tmp" result))))

(ert-deftest gptelt-shell-test-timeout ()
  "Test that timeout kills long-running process."
  (let ((result (gptelt-shell-test-run
                 (lambda (cb) (gptelt-run-shell-command 
                               cb 
                               "sleep 30; echo 'should not see this'"
                               nil  ; default directory
                               2))  ; 2 second timeout
                 10)))
    (should (string-match-p "TIMEOUT" result))
    (should-not (string-match-p "should not see this" result))))

(ert-deftest gptelt-shell-test-custom-shell ()
  "Test that custom shell parameter works."
  (let ((result (gptelt-shell-test-run
                 (lambda (cb) (gptelt-run-shell-command 
                               cb 
                               "echo $0"  ; prints shell name
                               nil nil "/bin/sh"))
                 10)))
    (should (string-match-p "Exit code: 0" result))))

(ert-deftest gptelt-shell-test-multiline-output ()
  "Test that multiline output is captured correctly."
  (let ((result (gptelt-shell-test-run
                 (lambda (cb) (gptelt-run-shell-command 
                               cb 
                               "echo 'line1'; echo 'line2'; echo 'line3'"))
                 10)))
    (should (string-match-p "line1" result))
    (should (string-match-p "line2" result))
    (should (string-match-p "line3" result))))

(ert-deftest gptelt-shell-test-truncation ()
  "Test output truncation when exceeding limit."
  (let ((gptelt-shell-command-max-output-bytes 100))  ; Small limit for testing
    (let ((result (gptelt-shell-test-run
                   (lambda (cb) (gptelt-run-shell-command 
                                 cb 
                                 "for i in $(seq 1 50); do echo 'this is a test line number '$i; done"))
                   10)))
      (should (string-match-p "OUTPUT TRUNCATED" result)))))

(ert-deftest gptelt-shell-test-special-characters ()
  "Test handling of special characters in commands."
  (let ((result (gptelt-shell-test-run
                 (lambda (cb) (gptelt-run-shell-command 
                               cb 
                               "echo 'hello\"world' && echo \"it's working\""))
                 10)))
    (should (string-match-p "hello\"world" result))
    (should (string-match-p "it's working" result))))

(provide 'gptel-tools/shell-test)
;;; shell-test.el ends here
