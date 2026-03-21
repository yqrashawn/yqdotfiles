;;; gptel-extra-test.el -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest +gptel-idle-timeout-default ()
  "Default idle timeout should be 6 hours in seconds."
  (should (= +gptel-idle-timeout (* 6 60 60))))

(ert-deftest +gptel-idle-check-interval-default ()
  "Default check interval should be 1 hour in seconds."
  (should (= +gptel-idle-check-interval (* 60 60))))

(ert-deftest +gptel--kill-idle-buffers-kills-stale-file-buffer ()
  "Should kill a file-visiting gptel-mode buffer idle longer than timeout."
  (let* ((tmpfile (make-temp-file "gptel-test"))
         (buf (find-file-noselect tmpfile))
         (+gptel-idle-timeout 0))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local gptel-mode t)
            (setq buffer-display-time (time-subtract (current-time) (seconds-to-time 1))))
          (+gptel--kill-idle-buffers)
          (should-not (buffer-live-p buf)))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-file tmpfile))))

(ert-deftest +gptel--kill-idle-buffers-keeps-active-file-buffer ()
  "Should not kill a file-visiting gptel-mode buffer that was recently displayed."
  (let* ((tmpfile (make-temp-file "gptel-test"))
         (buf (find-file-noselect tmpfile))
         (+gptel-idle-timeout (* 6 60 60)))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local gptel-mode t)
            (setq buffer-display-time (current-time)))
          (+gptel--kill-idle-buffers)
          (should (buffer-live-p buf)))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-file tmpfile))))

(ert-deftest +gptel--kill-idle-buffers-ignores-non-gptel-buffer ()
  "Should not kill buffers without gptel-mode."
  (let* ((tmpfile (make-temp-file "gptel-test"))
         (buf (find-file-noselect tmpfile))
         (+gptel-idle-timeout 0))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local gptel-mode nil)
            (setq buffer-display-time (time-subtract (current-time) (seconds-to-time 100))))
          (+gptel--kill-idle-buffers)
          (should (buffer-live-p buf)))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-file tmpfile))))

(ert-deftest +gptel--kill-idle-buffers-ignores-non-file-buffer ()
  "Should not kill gptel-mode buffers that don't visit a file."
  (let ((buf (generate-new-buffer " *test-gptel-no-file*"))
        (+gptel-idle-timeout 0))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local gptel-mode t)
            (setq buffer-display-time (time-subtract (current-time) (seconds-to-time 100))))
          (+gptel--kill-idle-buffers)
          (should (buffer-live-p buf)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest +gptel--kill-idle-buffers-ignores-nil-display-time ()
  "Should not kill buffer if it was never displayed."
  (let* ((tmpfile (make-temp-file "gptel-test"))
         (buf (find-file-noselect tmpfile))
         (+gptel-idle-timeout 0))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local gptel-mode t)
            (setq buffer-display-time nil))
          (+gptel--kill-idle-buffers)
          (should (buffer-live-p buf)))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-file tmpfile))))

(ert-deftest +gptel-start-idle-timer-creates-timer ()
  "Should create a repeating timer."
  (let ((+gptel--idle-timer nil)
        (+gptel-idle-check-interval 3600))
    (unwind-protect
        (progn
          (+gptel-start-idle-timer)
          (should +gptel--idle-timer)
          (should (timerp +gptel--idle-timer)))
      (+gptel-stop-idle-timer))))

(ert-deftest +gptel-stop-idle-timer-cancels-timer ()
  "Should cancel and nil out the timer."
  (let ((+gptel--idle-timer nil)
        (+gptel-idle-check-interval 3600))
    (+gptel-start-idle-timer)
    (+gptel-stop-idle-timer)
    (should-not +gptel--idle-timer)))

;;; Inject message tests

(ert-deftest +gptel-inject-write-message-writes-file ()
  "Should write message content to the inject file."
  (let* ((+gptel-inject-message-dir (make-temp-file "inject-test" t))
         (session-id "test-session-abc")
         (expected-file (expand-file-name (concat session-id ".txt")
                                          +gptel-inject-message-dir)))
    (unwind-protect
        (progn
          (should (+gptel-inject--write-message "Please also fix the tests" session-id))
          (should (file-exists-p expected-file))
          (should (string= "Please also fix the tests"
                           (string-trim (with-temp-buffer
                                          (insert-file-contents expected-file)
                                          (buffer-string))))))
      (delete-directory +gptel-inject-message-dir t))))

(ert-deftest +gptel-inject-write-message-skips-empty ()
  "Should not write file for empty/whitespace message."
  (let* ((+gptel-inject-message-dir (make-temp-file "inject-test" t))
         (session-id "test-session-empty")
         (expected-file (expand-file-name (concat session-id ".txt")
                                          +gptel-inject-message-dir)))
    (unwind-protect
        (progn
          (should-not (+gptel-inject--write-message "   " session-id))
          (should-not (file-exists-p expected-file)))
      (delete-directory +gptel-inject-message-dir t))))

(ert-deftest +gptel-inject-write-message-creates-dir ()
  "Should create inject directory if it doesn't exist."
  (let* ((+gptel-inject-message-dir (expand-file-name "nonexistent-subdir"
                                                       (make-temp-file "inject-test" t))))
    (unwind-protect
        (progn
          (+gptel-inject--write-message "test msg" "dir-test")
          (should (file-directory-p +gptel-inject-message-dir)))
      (delete-directory (file-name-directory +gptel-inject-message-dir) t))))

;;; Session ID generation tests

(ert-deftest +gptel--ccl-backend-p-returns-nil-for-non-ccl ()
  "Should return nil when backend is not ccl or ccld."
  (let ((gptel-backend nil))
    (should-not (+gptel--ccl-backend-p))))

(ert-deftest +gptel--ccl-backend-p-returns-t-for-ccl ()
  "Should return t when backend is ccl."
  (let ((gptel-backend gptel--ccl))
    (should (+gptel--ccl-backend-p))))

(ert-deftest +gptel--new-session-id-returns-nil-for-non-ccl ()
  "Should return nil when not a ccl/ccld backend."
  (let ((gptel-backend nil))
    (should-not (+gptel--new-session-id))))

(ert-deftest +gptel--new-session-id-always-generates-fresh-uuid ()
  "Should always generate a new UUID, even when one exists."
  (let ((gptel-backend gptel--ccl)
        (gptel-claude-code--session-id "old-session-id"))
    (let ((id (+gptel--new-session-id)))
      (should id)
      (should (stringp id))
      (should-not (string= "old-session-id" id))
      ;; UUID format
      (should (string-match-p
               "^[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}$"
               id)))))

(ert-deftest +gptel--new-session-id-sets-buffer-local ()
  "Should set the buffer-local session-id var."
  (let ((gptel-backend gptel--ccl)
        (gptel-claude-code--session-id nil))
    (let ((id (+gptel--new-session-id)))
      (should (string= id gptel-claude-code--session-id)))))

(ert-deftest +gptel--current-heading-session-id-returns-nil-for-non-ccl ()
  "Should return nil when not a ccl/ccld backend."
  (let ((gptel-backend nil))
    (should-not (+gptel--current-heading-session-id))))

(provide 'gptel-extra-test)
;;; gptel-extra-test.el ends here
