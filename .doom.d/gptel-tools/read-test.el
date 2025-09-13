;;; .nixpkgs/.doom.d/gptel-tools/read-test.el -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest gptelt-test-read-file ()
  "Test gptelt-read-file returns file content as expected."
  (let* ((fname (make-temp-file "gptelt-read-file-test" nil ".txt"))
         (content "foo\nbar\nbaz\nquux\n")
         (_ (with-temp-file fname (insert content))))
    (unwind-protect
        (should (string-match-p "foo" (gptelt-read-file fname 0 10)))
      (when (file-exists-p fname) (delete-file fname)))))

(ert-deftest gptelt-test-read-buffer ()
  "Test gptelt-read-buffer returns buffer content by buffer name."
  (let* ((buf (generate-new-buffer "gptelt-read-buffer-test")))
    (unwind-protect
        (progn
          (with-current-buffer buf (insert "ReadBufferTest123\nfoobar\n"))
          (should (string-match-p "ReadBufferTest123" (gptelt-read-buffer "gptelt-read-buffer-test" 0 10))))
      (kill-buffer buf))))
