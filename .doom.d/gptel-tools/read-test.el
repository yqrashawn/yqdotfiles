;;; .nixpkgs/.doom.d/gptel-tools/read-test.el -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest gptel-tools-test-read-file ()
  "Test gptel-tools-read-file returns file content as expected."
  (let* ((fname (make-temp-file "gptel-read-file-test" nil ".txt"))
         (content "foo\nbar\nbaz\nquux\n")
         (_ (with-temp-file fname (insert content))))
    (unwind-protect
        (should (string-match-p "foo" (gptel-tools-read-file fname 0 10)))
      (when (file-exists-p fname) (delete-file fname)))))

(ert-deftest gptel-tools-test-read-buffer ()
  "Test gptel-tools-read-buffer returns buffer content by buffer name."
  (let* ((buf (generate-new-buffer "gptel-read-buffer-test")))
    (unwind-protect
        (progn
          (with-current-buffer buf (insert "ReadBufferTest123\nfoobar\n"))
          (should (string-match-p "ReadBufferTest123" (gptel-tools-read-buffer "gptel-read-buffer-test" 0 10))))
      (kill-buffer buf))))
