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

;;; Comment region tests

(ert-deftest +gptel-comment--insert-appends-quote-block ()
  "Should insert quote block + comment at end of source buffer."
  (let ((source (generate-new-buffer " *test-comment-source*")))
    (unwind-protect
        (progn
          (with-current-buffer source
            (insert "existing content\n"))
          (+gptel-comment--insert "quoted text" "my comment" source)
          (with-current-buffer source
            (should (string= "existing content\n\n#+begin_quote\nquoted text\n#+end_quote\nmy comment\n"
                             (buffer-string)))))
      (kill-buffer source))))

(ert-deftest +gptel-comment--insert-adds-newline-if-needed ()
  "Should add newline before quote block if buffer doesn't end with one."
  (let ((source (generate-new-buffer " *test-comment-source*")))
    (unwind-protect
        (progn
          (with-current-buffer source
            (insert "no trailing newline"))
          (+gptel-comment--insert "quoted" "comment" source)
          (with-current-buffer source
            (should (string= "no trailing newline\n\n#+begin_quote\nquoted\n#+end_quote\ncomment\n"
                             (buffer-string)))))
      (kill-buffer source))))

(ert-deftest +gptel-comment--insert-multiline-quote ()
  "Should handle multi-line quoted text correctly."
  (let ((source (generate-new-buffer " *test-comment-source*")))
    (unwind-protect
        (progn
          (with-current-buffer source
            (insert "start\n"))
          (+gptel-comment--insert "line 1\nline 2\nline 3" "looks good" source)
          (with-current-buffer source
            (should (string= "start\n\n#+begin_quote\nline 1\nline 2\nline 3\n#+end_quote\nlooks good\n"
                             (buffer-string)))))
      (kill-buffer source))))

(ert-deftest +gptel-comment--insert-empty-buffer ()
  "Should work on an empty source buffer."
  (let ((source (generate-new-buffer " *test-comment-empty*")))
    (unwind-protect
        (progn
          (+gptel-comment--insert "quoted" "comment" source)
          (with-current-buffer source
            (should (string= "#+begin_quote\nquoted\n#+end_quote\ncomment\n"
                             (buffer-string)))))
      (kill-buffer source))))

(ert-deftest +gptel-comment-send-errors-on-empty-comment ()
  "Should error when comment buffer is empty."
  (let ((comment-buf (generate-new-buffer " *test-comment*"))
        (source (generate-new-buffer " *test-source*")))
    (unwind-protect
        (with-current-buffer comment-buf
          (setq-local +gptel-comment--quoted-text "quoted")
          (setq-local +gptel-comment--source-buffer source)
          (should-error (+gptel-comment-send) :type 'user-error))
      (when (buffer-live-p comment-buf) (kill-buffer comment-buf))
      (when (buffer-live-p source) (kill-buffer source)))))

(ert-deftest +gptel-comment-send-errors-on-dead-source ()
  "Should error when source buffer has been killed."
  (let ((comment-buf (generate-new-buffer " *test-comment*"))
        (source (generate-new-buffer " *test-dead-source*")))
    (kill-buffer source)
    (unwind-protect
        (with-current-buffer comment-buf
          (insert "my comment")
          (setq-local +gptel-comment--quoted-text "quoted")
          (setq-local +gptel-comment--source-buffer source)
          (should-error (+gptel-comment-send) :type 'user-error))
      (when (buffer-live-p comment-buf) (kill-buffer comment-buf)))))

(ert-deftest +gptel-comment-send-inserts-and-kills-buffer ()
  "Should insert quote+comment into source and kill comment buffer."
  (let ((comment-buf (generate-new-buffer " *test-comment*"))
        (source (generate-new-buffer " *test-source*")))
    (unwind-protect
        (progn
          (with-current-buffer source
            (insert "existing\n"))
          (with-current-buffer comment-buf
            (insert "my reply")
            (setq-local +gptel-comment--quoted-text "agent said this")
            (setq-local +gptel-comment--source-buffer source))
          (cl-letf (((symbol-function 'window-parameter) (lambda (&rest _) nil)))
            (with-current-buffer comment-buf
              (+gptel-comment-send)))
          (with-current-buffer source
            (should (string-match-p "#\\+begin_quote" (buffer-string)))
            (should (string-match-p "agent said this" (buffer-string)))
            (should (string-match-p "my reply" (buffer-string))))
          (should-not (buffer-live-p comment-buf)))
      (when (buffer-live-p comment-buf) (kill-buffer comment-buf))
      (when (buffer-live-p source) (kill-buffer source)))))

(ert-deftest +gptel-comment--insert-adds-blank-line-separator ()
  "Should add blank line before quote block when appending after text."
  (let ((source (generate-new-buffer " *test-comment-blank*")))
    (unwind-protect
        (progn
          (with-current-buffer source
            (insert "some text\n"))
          (+gptel-comment--insert "quoted" "comment" source)
          (with-current-buffer source
            (should (string= "some text\n\n#+begin_quote\nquoted\n#+end_quote\ncomment\n"
                             (buffer-string)))))
      (kill-buffer source))))

(ert-deftest +gptel-comment-region-errors-without-gptel-mode ()
  "Should error when called outside gptel-mode."
  (let ((buf (generate-new-buffer " *test-no-gptel*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "some text")
          (should-error (+gptel-comment-region 1 10) :type 'user-error))
      (kill-buffer buf))))

(ert-deftest +gptel-comment-region-errors-on-empty-selection ()
  "Should error when selected region is empty/whitespace."
  (let ((buf (generate-new-buffer " *test-empty-sel*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local gptel-mode t)
          (insert "   ")
          (should-error (+gptel-comment-region 1 4) :type 'user-error))
      (kill-buffer buf))))

(provide 'gptel-extra-test)
;;; gptel-extra-test.el ends here
