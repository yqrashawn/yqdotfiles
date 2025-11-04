;;; .nixpkgs/.doom.d/gptel-tools/read-test.el -*- lexical-binding: t; coding: utf-8 -*-

(require 'ert)

(comment
  "␂ and ␃")

(ert-deftest gptelt-test-read-file ()
  "Test gptelt-read-file returns file content as expected. ␂ and ␃"
  (let* ((fname (make-temp-file "gptelt-read-file-test" nil ".txt"))
         (content "foo\nbar\nbaz\nquux\n")
         (_ (with-temp-file fname (insert content)))
         (result (gptelt-read-file fname 0 10)))
    (unwind-protect
        (progn
          (should (string-match-p "\\[Total lines: 4\\]" result))
          (should (string-match-p "\\[Content lines: 1-4\\]" result))
          (should (string-match-p (format "\\[File path: %s\\]" fname) result))
          (should (string-match-p "␂foo" result))
          (should (string-match-p "quux\n␃" result)))
      (when (file-exists-p fname) (delete-file fname)))))

(ert-deftest gptelt-test-read-buffer ()
  "Test gptelt-read-buffer returns buffer content by buffer name."
  (let* ((fname (make-temp-file "gptelt-read-buffer-test" nil ".txt"))
         (buf (find-file-noselect fname))
         result)
    (unwind-protect
        (progn
          (with-current-buffer buf (insert "ReadBufferTest123\nfoobar\n"))
          (setq result (gptelt-read-buffer (buffer-name buf) 0 10))
          (should (string-match-p "\\[Total lines: 2\\]" result))
          (should (string-match-p "\\[Content lines: 1-2\\]" result))
          (should (string-match-p "\\[Buffer name: gptelt-read-buffer-test\\w+\\.txt\\]" result))
          (should (string-match-p
                   (concat "\\[File path: .*gptelt-read-buffer-test\\w+\\.txt\\]")
                   result))
          (should (string-match-p "␂ReadBufferTest123" result))
          (should (string-match-p "foobar\n␃" result)))
      (kill-buffer buf))))

;;; read-test.el ends here
