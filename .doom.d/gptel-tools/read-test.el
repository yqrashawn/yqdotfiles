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
          (should (string-match-p "␂ *1→foo" result))
          (should (string-match-p "4→quux\n␃" result)))
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
          (should (string-match-p "␂ *1→ReadBufferTest123" result))
          (should (string-match-p "2→foobar\n␃" result)))
      (kill-buffer buf))))

(ert-deftest gptelt-test-read-file-string-numbers ()
  "Test gptelt-read-file accepts string numbers for offset and limit."
  (let* ((fname (make-temp-file "gptelt-read-file-string-test" nil ".txt"))
         (content "line1\nline2\nline3\nline4\nline5\n")
         (_ (with-temp-file fname (insert content)))
         (result (gptelt-read-file fname "1" "400")))
    (unwind-protect
        (progn
          (should (string-match-p "\\[Total lines: 5\\]" result))
          (should (string-match-p "\\[Content lines: 2-5\\]" result))
          (should (string-match-p "␂ *2→line2" result))
          (should (string-match-p "5→line5\n␃" result))
          (should-not (string-match-p "1→line1" result)))
      (when (file-exists-p fname) (delete-file fname)))))

(ert-deftest gptelt-test-read-buffer-string-numbers ()
  "Test gptelt-read-buffer accepts string numbers for offset and limit."
  (let* ((fname (make-temp-file "gptelt-read-buffer-string-test" nil ".txt"))
         (buf (find-file-noselect fname))
         result)
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "line1\nline2\nline3\nline4\nline5\n"))
          (setq result (gptelt-read-buffer (buffer-name buf) "1" "400"))
          (should (string-match-p "\\[Total lines: 5\\]" result))
          (should (string-match-p "\\[Content lines: 2-5\\]" result))
          (should (string-match-p "␂ *2→line2" result))
          (should (string-match-p "5→line5\n␃" result))
          (should-not (string-match-p "1→line1" result)))
      (kill-buffer buf))))

;;; --- Line number tests ---

(ert-deftest gptelt-test-add-line-numbers ()
  "Test gptelt--add-line-numbers formats like Claude Code."
  ;; Basic case
  (let ((result (gptelt--add-line-numbers "foo\nbar\nbaz\n" 1 3)))
    (should (string-match-p "^ *1→foo$" result))
    (should (string-match-p "^ *2→bar$" result))
    (should (string-match-p "^ *3→baz$" result)))
  ;; Width adjusts for large line numbers
  (let ((result (gptelt--add-line-numbers "a\nb\n" 99 100)))
    (should (string-match-p "^ *99→a$" result))
    (should (string-match-p "^100→b$" result)))
  ;; Minimum width is 6 (matches Claude Code)
  (let ((result (gptelt--add-line-numbers "x\n" 1 1)))
    (should (string-match-p "^     1→x$" result))))

;;; --- Error wrapping tests ---

(ert-deftest gptelt-test-read-buffer-mcp-error-wrapped ()
  "Test that read-buffer-mcp wraps errors in <tool_use_error> tags."
  (let ((result (gptelt-read-buffer-mcp "nonexistent-buffer-xyz-999")))
    (should (stringp result))
    (should (string-prefix-p "<tool_use_error>" result))
    (should (string-suffix-p "</tool_use_error>" result))
    (should (string-match-p "not found" result))))

(ert-deftest gptelt-test-read-buffer-mcp-success-not-wrapped ()
  "Test that read-buffer-mcp does NOT wrap successful reads."
  (let* ((fname (make-temp-file "gptelt-read-mcp-test" nil ".txt"))
         (buf (find-file-noselect fname))
         result)
    (unwind-protect
        (progn
          (with-current-buffer buf (insert "hello\n"))
          (setq result (gptelt-read-buffer-mcp (buffer-name buf) 0 10))
          (should (stringp result))
          (should-not (string-match-p "<tool_use_error>" result))
          (should (string-match-p "hello" result)))
      (kill-buffer buf)
      (when (file-exists-p fname) (delete-file fname)))))

(ert-deftest gptelt-test-read-multiple-buffers-mcp-error-wrapped ()
  "Test that read-multiple-buffers-mcp wraps per-buffer errors."
  (let ((results (gptelt-read-multiple-buffers-mcp
                  (list (list :buffer-name "nonexistent-buf-abc-777")))))
    (should (listp results))
    (let ((err-msg (plist-get (car results) :error)))
      (should err-msg)
      (should (string-prefix-p "<tool_use_error>" err-msg))
      (should (string-suffix-p "</tool_use_error>" err-msg)))))

;;; read-test.el ends here
