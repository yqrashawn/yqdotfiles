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

(provide 'gptel-extra-test)
;;; gptel-extra-test.el ends here
