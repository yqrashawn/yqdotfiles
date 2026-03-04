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

;;; --- read_multiple_files tests ---

(ert-deftest gptelt-test-read-multiple-files ()
  "Test gptelt-read-multiple-files returns line-numbered content."
  (let* ((f1 (make-temp-file "gptelt-multi-read-1" nil ".txt"))
         (f2 (make-temp-file "gptelt-multi-read-2" nil ".txt"))
         (f3 (make-temp-file "gptelt-multi-read-nonexist" nil ".txt")))
    (with-temp-file f1 (insert "alpha\nbeta\n"))
    (with-temp-file f2 (insert "gamma\ndelta\nepsilon\n"))
    (delete-file f3)  ; make it nonexistent
    (unwind-protect
        (let ((results (gptelt-read-multiple-files
                        (list (list :file-path f1)
                              (list :file-path f2 :offset 1 :limit 300)
                              (list :file-path f3)))))
          ;; First file: full content with line numbers
          (let ((r1 (nth 0 results)))
            (should (equal (plist-get r1 :file-path) f1))
            (should (plist-get r1 :content))
            (should (string-match-p "␂ *1→alpha" (plist-get r1 :content)))
            (should (string-match-p "2→beta" (plist-get r1 :content))))
          ;; Second file: offset=1, should start at line 2
          (let ((r2 (nth 1 results)))
            (should (equal (plist-get r2 :file-path) f2))
            (should (plist-get r2 :content))
            (should (string-match-p "␂ *2→delta" (plist-get r2 :content)))
            (should-not (string-match-p "1→gamma" (plist-get r2 :content))))
          ;; Third file: nonexistent, should have error
          (let ((r3 (nth 2 results)))
            (should (equal (plist-get r3 :file-path) f3))
            (should (plist-get r3 :error))))
      (when (file-exists-p f1) (delete-file f1))
      (when (file-exists-p f2) (delete-file f2)))))

(ert-deftest gptelt-test-read-multiple-buffers-line-numbers ()
  "Test gptelt-read-multiple-buffers returns line-numbered content."
  (let* ((f1 (make-temp-file "gptelt-multi-buf-1" nil ".txt"))
         (f2 (make-temp-file "gptelt-multi-buf-2" nil ".txt"))
         (b1 (find-file-noselect f1))
         (b2 (find-file-noselect f2)))
    (unwind-protect
        (progn
          (with-current-buffer b1 (insert "one\ntwo\nthree\n"))
          (with-current-buffer b2 (insert "four\nfive\n"))
          (let ((results (gptelt-read-multiple-buffers
                          (list (list :buffer-name (buffer-name b1))
                                (list :buffer-name (buffer-name b2))
                                (list :buffer-name "nonexistent-multi-buf-999")))))
            ;; First buffer
            (let ((r1 (nth 0 results)))
              (should (string-match-p "␂ *1→one" (plist-get r1 :content)))
              (should (string-match-p "3→three" (plist-get r1 :content))))
            ;; Second buffer
            (let ((r2 (nth 1 results)))
              (should (string-match-p "␂ *1→four" (plist-get r2 :content)))
              (should (string-match-p "2→five" (plist-get r2 :content))))
            ;; Third: error
            (let ((r3 (nth 2 results)))
              (should (plist-get r3 :error)))))
      (kill-buffer b1)
      (kill-buffer b2)
      (when (file-exists-p f1) (delete-file f1))
      (when (file-exists-p f2) (delete-file f2)))))

;;; read-test.el ends here
