;;; .nixpkgs/.doom.d/gptel-tools/edit-file-test.el -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest gptelt-edit-edit-file-buffer-temp-test ()
  "Test create-file-buffer, edit-file, and edit-buffer (with temp files), including replace_all."
  (let* ((temp-el-path (make-temp-file "gptelt-test-" nil ".el"))
         (temp-txt-path (make-temp-file "gptelt-test-" nil ".txt")))
    (unwind-protect
        (progn
          ;; Create initial files
          (with-temp-file temp-el-path
            (insert ";; TEMP TEST FILE\n(foo 1)\n(foo 1)\n(bar 2)\n"))
          (with-temp-file temp-txt-path
            (insert ";; new temp file\ndata: test123\ndata: test123"))
          
          ;; edit-file with replace_all=nil (only first)
          (gptelt-edit-edit-file 'ignore temp-el-path "(foo 1)" "(foo 42)")
          (with-temp-buffer
            (insert-file-contents temp-el-path)
            (let ((content (buffer-string)))
              (should (string-match-p "(foo 42)" content))
              (should (string-match-p "(foo 1)" content))))
          
          ;; edit-file with replace_all=t (replace all)
          (gptelt-edit-edit-file 'ignore temp-el-path "(foo 1)" "(foo 77)" t)
          (with-temp-buffer
            (insert-file-contents temp-el-path)
            (let ((content (buffer-string)))
              (should (not (string-match-p "(foo 1)" content)))
              (should (string-match-p "(foo 77)" content))))

          ;; Edit temp .txt file and verify replace_all=t
          (gptelt-edit-edit-file 'ignore temp-txt-path "test123" "xyz789" t)
          (with-temp-buffer
            (insert-file-contents temp-txt-path)
            (let ((content (buffer-string)))
              (should (not (string-match-p "test123" content)))
              (should (string-match-p "xyz789" content))))

          ;; Edit buffer for temp .el file and verify default (first only)
          (let* ((buf (find-file-noselect temp-el-path))
                 (old "(bar 2)")
                 (new "(bar 99)"))
            (gptelt-edit-edit-buffer 'ignore (buffer-name buf) old new)
            (let ((new-content (with-current-buffer buf (buffer-string))))
              (should (string-match-p "(bar 99)" new-content)))))
      (when (file-exists-p temp-el-path) (delete-file temp-el-path))
      (when (file-exists-p temp-txt-path) (delete-file temp-txt-path)))))


(ert-deftest gptelt-edit-multi-edit-buffer-test ()
  "Test multi_edit_buffer tool on two temp buffers with and without replace_all."
  (let* ((temp1-path (make-temp-file "gptelt-test-" nil ".el"))
         (temp2-path (make-temp-file "gptelt-test-" nil ".txt")))
    (unwind-protect
        (progn
          ;; Create initial files
          (with-temp-file temp1-path
            (insert ";; TEMP1\n(foo 1)\n(foo 1)\n(bar 2)\n"))
          (with-temp-file temp2-path
            (insert ";; TEMP2\ndata: alpha\ndata: beta\ndata: alpha"))
          
          (let* ((buf1 (find-file-noselect temp1-path))
                 (buf2 (find-file-noselect temp2-path))
                 (edits1 `[,(list :old_string "(foo 1)" :new_string "(foo 99)" :replace_all t)
                           ,(list :old_string "(bar 2)" :new_string "(bar 77)")])
                 (edits2 `[,(list :old_string "alpha" :new_string "omega" :replace_all t)
                           ,(list :old_string "beta" :new_string "theta")]))
            (should (>= (cl-count ?f (with-current-buffer buf1 (buffer-string))) 2))
            (should (>= (cl-count ?a (with-current-buffer buf2 (buffer-string))) 2))

            (gptelt-edit-multi-edit-buffer 'ignore (buffer-name buf1) edits1)
            (gptelt-edit-multi-edit-buffer 'ignore (buffer-name buf2) edits2)

            (let ((b1-content (with-current-buffer buf1 (buffer-string)))
                  (b2-content (with-current-buffer buf2 (buffer-string))))
              (should (not (string-match-p "(foo 1)" b1-content)))
              (should (string-match-p "(foo 99)" b1-content))
              (should (string-match-p "(bar 77)" b1-content))
              (should (not (string-match-p "alpha" b2-content)))
              (should (string-match-p "omega" b2-content))
              (should (string-match-p "theta" b2-content)))))
      (when (file-exists-p temp1-path) (delete-file temp1-path))
      (when (file-exists-p temp2-path) (delete-file temp2-path)))))

(ert-deftest gptelt-edit-multi-edit-buffer-error-handling-test ()
  "Test that multi_edit_buffer stops on first error and reports it correctly."
  (let* ((temp-path (make-temp-file "gptelt-test-" nil ".txt"))
         (result-msg nil))
    (unwind-protect
        (progn
          ;; Create initial file with content
          (with-temp-file temp-path
            (insert "line1\nline2\nline3\n"))
          
          (let* ((buf (find-file-noselect temp-path))
                 ;; First edit will succeed, second will fail (no "nonexistent" in buffer)
                 (edits `[,(list :old_string "line1" :new_string "changed1")
                          ,(list :old_string "nonexistent" :new_string "should-not-appear")
                          ,(list :old_string "line3" :new_string "should-not-appear-either")]))
            
            ;; Capture the callback result
            (gptelt-edit-multi-edit-buffer
             (lambda (result)
               (setq result-msg result))
             (buffer-name buf)
             edits)
            
            ;; Wait a bit for async callback to complete
            (sleep-for 0.1)
            
            ;; Verify error message indicates failure at edit 2
            (should result-msg)
            (should (string-match-p "Multi-edit failed" result-msg))
            (should (string-match-p "Edit 1/3: SUCCESS" result-msg))
            (should (string-match-p "Edit 2/3: FAILED" result-msg))
            (should (string-match-p "Edit 3/3: SKIPPED" result-msg))
            (should (string-match-p "old_string not found" result-msg))
            
            ;; Verify buffer state: first edit applied, others not
            (let ((content (with-current-buffer buf (buffer-string))))
              (should (string-match-p "changed1" content))
              (should (not (string-match-p "line1" content)))
              (should (string-match-p "line2" content))  ; unchanged
              (should (string-match-p "line3" content))  ; unchanged
              (should (not (string-match-p "should-not-appear" content))))))
      (when (file-exists-p temp-path) (delete-file temp-path)))))

;;; --- Flexible whitespace matching tests ---

(ert-deftest gptelt-edit-normalize-line-test ()
  "Test normalize-line strips leading/trailing whitespace and collapses internal runs."
  ;; Basic trimming and collapsing
  (should (equal (gptelt-edit--normalize-line "  foo  bar  ")
                 "foo bar"))
  ;; Tabs
  (should (equal (gptelt-edit--normalize-line "\tfoo\t\tbar\t")
                 "foo bar"))
  ;; Mixed whitespace
  (should (equal (gptelt-edit--normalize-line "  \t hello \t world  \t ")
                 "hello world"))
  ;; Already normalized
  (should (equal (gptelt-edit--normalize-line "foo bar")
                 "foo bar"))
  ;; Empty string
  (should (equal (gptelt-edit--normalize-line "")
                 ""))
  ;; Only whitespace
  (should (equal (gptelt-edit--normalize-line "   \t  ")
                 "")))

(ert-deftest gptelt-edit-flexible-match-indentation-test ()
  "Test flexible match finds text despite different indentation."
  (let* ((temp-path (make-temp-file "gptelt-test-" nil ".txt")))
    (unwind-protect
        (progn
          ;; Buffer has 4-space indentation
          (with-temp-file temp-path
            (insert "function foo() {\n    return 1;\n    console.log('done');\n}\n"))
          (let ((buf (find-file-noselect temp-path)))
            ;; Search with 2-space indentation -- should still match
            (let ((result (gptelt-edit--find-match
                           buf "function foo() {\n  return 1;\n  console.log('done');\n}")))
              (should result)
              (should (plist-get result :match-pos))
              ;; :corrected-string should be the actual buffer text
              (should (string-match-p "    return 1;" (plist-get result :corrected-string))))))
      (when (file-exists-p temp-path) (delete-file temp-path)))))

(ert-deftest gptelt-edit-flexible-match-tab-vs-space-test ()
  "Test flexible match handles tab vs space differences."
  (let* ((temp-path (make-temp-file "gptelt-test-" nil ".txt")))
    (unwind-protect
        (progn
          ;; Buffer uses tabs
          (with-temp-file temp-path
            (insert "if true:\n\tprint('hello')\n\tprint('world')\n"))
          (let ((buf (find-file-noselect temp-path)))
            ;; Search with spaces
            (let ((result (gptelt-edit--find-match
                           buf "if true:\n    print('hello')\n    print('world')")))
              (should result)
              (should (plist-get result :match-pos)))))
      (when (file-exists-p temp-path) (delete-file temp-path)))))

(ert-deftest gptelt-edit-flexible-match-extra-spaces-test ()
  "Test flexible match handles extra internal whitespace."
  (let* ((temp-path (make-temp-file "gptelt-test-" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file temp-path
            (insert "x  =  1\ny = 2\n"))
          (let ((buf (find-file-noselect temp-path)))
            ;; Search with single spaces -- should match double-spaced version
            (let ((result (gptelt-edit--find-match buf "x = 1")))
              (should result)
              ;; corrected-string should be actual buffer text
              (should (equal (plist-get result :corrected-string) "x  =  1")))))
      (when (file-exists-p temp-path) (delete-file temp-path)))))

(ert-deftest gptelt-edit-flexible-match-no-false-positive-test ()
  "Test flexible match does NOT match when content differs."
  (let* ((temp-path (make-temp-file "gptelt-test-" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file temp-path
            (insert "foo bar\nbaz qux\n"))
          (let ((buf (find-file-noselect temp-path)))
            ;; Different content should NOT match
            (should-not (gptelt-edit--find-match buf "foo baz"))
            (should-not (gptelt-edit--find-match buf "foo bar\nbaz quux"))))
      (when (file-exists-p temp-path) (delete-file temp-path)))))

(ert-deftest gptelt-edit-flexible-match-applies-replacement-test ()
  "Test that flexible match actually applies the replacement correctly."
  (let* ((temp-path (make-temp-file "gptelt-test-" nil ".txt"))
         (result-msg nil))
    (unwind-protect
        (progn
          (with-temp-file temp-path
            (insert "  def  foo():\n    return  1\n"))
          (let ((buf (find-file-noselect temp-path)))
            ;; Edit with loosely-spaced old_string
            (gptelt-edit-edit-file
             (lambda (r) (setq result-msg r))
             temp-path
             "def foo():\n  return 1"
             "def bar():\n  return 2")
            (sleep-for 0.1)
            (should (string-match-p "Successfully replaced" result-msg))
            ;; Verify the file was actually changed
            (with-temp-buffer
              (insert-file-contents temp-path)
              (let ((content (buffer-string)))
                (should (string-match-p "def bar" content))
                (should (not (string-match-p "def foo" content)))))))
      (when (file-exists-p temp-path) (delete-file temp-path)))))

;;; --- MCP validation wrapper tests ---

(ert-deftest gptelt-edit-mcp-validation-missing-file-path-test ()
  "Test MCP wrapper rejects nil/empty file_path."
  (let ((result nil))
    ;; nil file_path
    (gptelt-edit-edit-file-mcp (lambda (r) (setq result r))
                               nil "old" "new")
    (should (string-match-p "file_path.*required" result))
    ;; empty string
    (setq result nil)
    (gptelt-edit-edit-file-mcp (lambda (r) (setq result r))
                               "" "old" "new")
    (should (string-match-p "file_path.*required" result))))

(ert-deftest gptelt-edit-mcp-validation-missing-old-string-test ()
  "Test MCP wrapper rejects nil old_string."
  (let ((result nil))
    (gptelt-edit-edit-file-mcp (lambda (r) (setq result r))
                               "/tmp/foo.txt" nil "new")
    (should (string-match-p "old_string.*required" result))))

(ert-deftest gptelt-edit-mcp-validation-missing-new-string-test ()
  "Test MCP wrapper rejects nil new_string."
  (let ((result nil))
    (gptelt-edit-edit-file-mcp (lambda (r) (setq result r))
                               "/tmp/foo.txt" "old" nil)
    (should (string-match-p "new_string.*required" result))))

(ert-deftest gptelt-edit-mcp-validation-bad-replace-all-test ()
  "Test MCP wrapper rejects non-boolean replace_all."
  (let ((result nil))
    (gptelt-edit-edit-file-mcp (lambda (r) (setq result r))
                               "/tmp/foo.txt" "old" "new" "yes")
    (should (string-match-p "replace_all.*boolean" result))))

(ert-deftest gptelt-edit-mcp-validation-bad-instruction-test ()
  "Test MCP wrapper rejects non-string instruction."
  (let ((result nil))
    (gptelt-edit-edit-file-mcp (lambda (r) (setq result r))
                               "/tmp/foo.txt" "old" "new" nil 42)
    (should (string-match-p "instruction.*string" result))))

(ert-deftest gptelt-edit-mcp-buffer-validation-missing-buffer-name-test ()
  "Test edit-buffer MCP wrapper rejects nil/empty buffer_name."
  (let ((result nil))
    (gptelt-edit-edit-buffer-mcp (lambda (r) (setq result r))
                                 nil "old" "new")
    (should (string-match-p "buffer_name.*required" result))
    (setq result nil)
    (gptelt-edit-edit-buffer-mcp (lambda (r) (setq result r))
                                 "" "old" "new")
    (should (string-match-p "buffer_name.*required" result))))

(ert-deftest gptelt-edit-mcp-multi-edit-validation-test ()
  "Test multi-edit MCP wrappers reject invalid parameters."
  (let ((result nil))
    ;; nil edits
    (gptelt-edit-multi-edit-file-mcp (lambda (r) (setq result r))
                                     "/tmp/foo.txt" nil)
    (should (string-match-p "edits.*required" result))
    ;; empty vector
    (setq result nil)
    (gptelt-edit-multi-edit-file-mcp (lambda (r) (setq result r))
                                     "/tmp/foo.txt" [])
    (should (string-match-p "at least one edit" result))
    ;; wrong type
    (setq result nil)
    (gptelt-edit-multi-edit-file-mcp (lambda (r) (setq result r))
                                     "/tmp/foo.txt" "not-a-list")
    (should (string-match-p "edits.*list or array" result))
    ;; nil file_path
    (setq result nil)
    (gptelt-edit-multi-edit-file-mcp (lambda (r) (setq result r))
                                     nil '((:old_string "a" :new_string "b")))
    (should (string-match-p "file_path.*required" result))))

;;; --- Error path / edge case tests ---

(ert-deftest gptelt-edit-not-found-error-test ()
  "Test that editing with non-matching old_string returns error."
  (let* ((temp-path (make-temp-file "gptelt-test-" nil ".txt"))
         (result-msg nil))
    (unwind-protect
        (progn
          (with-temp-file temp-path
            (insert "hello world\n"))
          (let ((buf (find-file-noselect temp-path)))
            (gptelt-edit-edit-buffer
             (lambda (r) (setq result-msg r))
             (buffer-name buf) "nonexistent text" "replacement")
            (sleep-for 0.1)
            (should result-msg)
            (should (string-match-p "old_string not found" result-msg))))
      (when (file-exists-p temp-path) (delete-file temp-path)))))

(ert-deftest gptelt-edit-nonexistent-file-error-test ()
  "Test that editing a non-existent file signals an error."
  (should-error
   (gptelt-edit-edit-file 'ignore "/tmp/nonexistent-gptelt-test-12345.txt"
                          "old" "new")
   :type 'error))

(ert-deftest gptelt-edit-nonexistent-buffer-error-test ()
  "Test that editing a non-existent buffer signals an error."
  (should-error
   (gptelt-edit-edit-buffer 'ignore "nonexistent-buffer-12345"
                            "old" "new")
   :type 'error))

(ert-deftest gptelt-edit-find-similar-text-test ()
  "Test that error messages include similar text hints."
  (let* ((temp-path (make-temp-file "gptelt-test-" nil ".txt"))
         (result-msg nil))
    (unwind-protect
        (progn
          (with-temp-file temp-path
            (insert "function calculateTotal(items) {\n  return items.reduce((a, b) => a + b, 0);\n}\n"))
          (let ((buf (find-file-noselect temp-path)))
            (gptelt-edit-edit-buffer
             (lambda (r) (setq result-msg r))
             (buffer-name buf)
             ;; Slightly wrong old_string (typo: Totall)
             "function calculateTotall(items) {"
             "function computeSum(items) {")
            (sleep-for 0.1)
            ;; Should include the similar text hint since first line partially matches
            (should result-msg)
            (should (string-match-p "old_string not found" result-msg))
            ;; The "Found similar text" hint works because the first line's
            ;; normalized form is searchable
            (should (string-match-p "Found similar text\\|calculateTotal" result-msg))))
      (when (file-exists-p temp-path) (delete-file temp-path)))))

;;; --- Lisp balance checking tests ---

(ert-deftest gptelt-edit-lisp-rejects-unbalanced-test ()
  "Test that editing Lisp code rejects unbalanced result."
  (let* ((temp-path (make-temp-file "gptelt-test-" nil ".el"))
         (result-msg nil))
    (unwind-protect
        (progn
          (with-temp-file temp-path
            (insert "(defun foo ()\n  (message \"hello\"))\n"))
          (let ((buf (find-file-noselect temp-path)))
            (with-current-buffer buf (emacs-lisp-mode))
            ;; Mock LLM balance repair to fail immediately (we're testing
            ;; the rejection path, not the LLM repair)
            (cl-letf (((symbol-function 'gptelt--attempt-llm-balance)
                       (lambda (_buffer callback)
                         (funcall callback (list :rst nil :err "LLM repair disabled in test")))))
              (gptelt-edit-edit-buffer
               (lambda (r) (setq result-msg r))
               (buffer-name buf)
               "(message \"hello\")"
               "(message \"hello\"")  ;; missing closing paren
              ;; Balance check callback runs synchronously with our mock
              (sleep-for 0.5)
              (should result-msg)
              ;; Should reject the unbalanced edit
              (should (string-match-p "unbalanced\\|PARENTHESES\\|LLM repair" result-msg))
              ;; Buffer should be unchanged
              (should (string-match-p "(message \"hello\")"
                                      (with-current-buffer buf (buffer-string)))))))
      (when (file-exists-p temp-path) (delete-file temp-path)))))

(ert-deftest gptelt-edit-lisp-accepts-balanced-test ()
  "Test that editing Lisp code accepts balanced result."
  (let* ((temp-path (make-temp-file "gptelt-test-" nil ".el"))
         (result-msg nil))
    (unwind-protect
        (progn
          (with-temp-file temp-path
            (insert "(defun foo ()\n  (message \"hello\"))\n"))
          (let ((buf (find-file-noselect temp-path)))
            (with-current-buffer buf (emacs-lisp-mode))
            (gptelt-edit-edit-buffer
             (lambda (r) (setq result-msg r))
             (buffer-name buf)
             "(message \"hello\")"
             "(message \"goodbye\")")  ;; balanced replacement
            (sleep-for 1)
            (should result-msg)
            (should (string-match-p "Successfully\\|replaced" result-msg))
            ;; Buffer should be changed
            (should (string-match-p "goodbye"
                                    (with-current-buffer buf (buffer-string))))))
      (when (file-exists-p temp-path) (delete-file temp-path)))))

(ert-deftest gptelt-edit-skip-balance-check-test ()
  "Test that `gptelt-edit-skip-balance-check' skips Lisp balance checking."
  (let* ((temp-path (make-temp-file "gptelt-test-" nil ".el"))
         (result-msg nil))
    (unwind-protect
        (progn
          (with-temp-file temp-path
            (insert "(defun foo ()\n  (message \"hello\"))\n"))
          (let ((buf (find-file-noselect temp-path))
                (gptelt-edit-skip-balance-check t))
            (with-current-buffer buf (emacs-lisp-mode))
            (gptelt-edit-edit-buffer
             (lambda (r) (setq result-msg r))
             (buffer-name buf)
             "(message \"hello\")"
             "(message \"hello\"")  ;; missing closing paren — unbalanced
            (sleep-for 0.5)
            (should result-msg)
            ;; Should succeed because balance check is skipped
            (should (string-match-p "Successfully\\|replaced" result-msg))))
      (when (file-exists-p temp-path) (delete-file temp-path)))))

;;; --- multi-edit-file tests ---

(ert-deftest gptelt-edit-multi-edit-file-test ()
  "Test multi_edit_file applies multiple edits to a file."
  (let* ((temp-path (make-temp-file "gptelt-test-" nil ".txt"))
         (result-msg nil))
    (unwind-protect
        (progn
          (with-temp-file temp-path
            (insert "host: localhost\nport: 3000\nenv: dev\n"))
          (gptelt-edit-multi-edit-file
           (lambda (r) (setq result-msg r))
           temp-path
           (list (list :old_string "localhost" :new_string "0.0.0.0")
                 (list :old_string "port: 3000" :new_string "port: 8080")
                 (list :old_string "env: dev" :new_string "env: prod")))
          (sleep-for 0.1)
          (should result-msg)
          (should (string-match-p "Successfully applied 3 edits" result-msg))
          ;; Verify file content
          (with-temp-buffer
            (insert-file-contents temp-path)
            (let ((content (buffer-string)))
              (should (string-match-p "0.0.0.0" content))
              (should (string-match-p "port: 8080" content))
              (should (string-match-p "env: prod" content))
              (should (not (string-match-p "localhost" content)))
              (should (not (string-match-p "3000" content))))))
      (when (file-exists-p temp-path) (delete-file temp-path)))))

(ert-deftest gptelt-edit-multi-edit-file-with-replace-all-test ()
  "Test multi_edit_file with replace_all flag."
  (let* ((temp-path (make-temp-file "gptelt-test-" nil ".txt"))
         (result-msg nil))
    (unwind-protect
        (progn
          (with-temp-file temp-path
            (insert "TODO: fix this\nTODO: and this\nDONE: already fixed\n"))
          (gptelt-edit-multi-edit-file
           (lambda (r) (setq result-msg r))
           temp-path
           (list (list :old_string "TODO" :new_string "DONE" :replace_all t)))
          (sleep-for 0.1)
          (should result-msg)
          (should (string-match-p "Successfully applied 1 edit" result-msg))
          (with-temp-buffer
            (insert-file-contents temp-path)
            (let ((content (buffer-string)))
              (should (not (string-match-p "TODO" content)))
              ;; Should have 3 "DONE" now (2 replaced + 1 original)
              (should (= 3 (cl-count-if
                            (lambda (_) t)
                            (split-string content "DONE" t)))))))
      (when (file-exists-p temp-path) (delete-file temp-path)))))

(ert-deftest gptelt-edit-multi-edit-file-error-stops-test ()
  "Test multi_edit_file stops at first error."
  (let* ((temp-path (make-temp-file "gptelt-test-" nil ".txt"))
         (result-msg nil))
    (unwind-protect
        (progn
          (with-temp-file temp-path
            (insert "aaa\nbbb\nccc\n"))
          (gptelt-edit-multi-edit-file
           (lambda (r) (setq result-msg r))
           temp-path
           (list (list :old_string "aaa" :new_string "AAA")
                 (list :old_string "NONEXISTENT" :new_string "XXX")
                 (list :old_string "ccc" :new_string "CCC")))
          (sleep-for 0.1)
          (should result-msg)
          (should (string-match-p "Multi-edit failed" result-msg))
          (should (string-match-p "Edit 1/3: SUCCESS" result-msg))
          (should (string-match-p "Edit 2/3: FAILED" result-msg))
          (should (string-match-p "Edit 3/3: SKIPPED" result-msg))
          ;; First edit applied, third not
          (with-temp-buffer
            (insert-file-contents temp-path)
            (let ((content (buffer-string)))
              (should (string-match-p "AAA" content))
              (should (string-match-p "ccc" content)))))
      (when (file-exists-p temp-path) (delete-file temp-path)))))

(ert-deftest gptelt-edit-multi-edit-file-tilde-path-test ()
  "Test multi_edit_file with ~/relative path."
  (let* ((temp-path (make-temp-file "gptelt-test-" nil ".txt"))
         (home-relative (concat "~/" (file-relative-name temp-path "~")))
         (result-msg nil))
    (unwind-protect
        (progn
          (with-temp-file temp-path
            (insert "old-value\n"))
          (gptelt-edit-multi-edit-file
           (lambda (r) (setq result-msg r))
           home-relative
           (list (list :old_string "old-value" :new_string "new-value")))
          (sleep-for 0.1)
          (should result-msg)
          (should (string-match-p "Successfully applied 1 edit" result-msg))
          (with-temp-buffer
            (insert-file-contents temp-path)
            (should (string-match-p "new-value" (buffer-string)))))
      (when (file-exists-p temp-path) (delete-file temp-path)))))

;;; --- Error wrapping tests ---

(ert-deftest gptelt-edit-error-wrapping-callback-test ()
  "Test that error-wrapping-callback wraps errors and passes success through."
  (let ((results nil))
    (let ((cb (gptelt-edit--error-wrapping-callback
               (lambda (r) (push r results)))))
      ;; Success message passes through unchanged
      (funcall cb "Successfully replaced 1 occurrence(s) of text in foo.txt.")
      (should (equal (car results) "Successfully replaced 1 occurrence(s) of text in foo.txt."))
      ;; Error message gets wrapped
      (funcall cb "old_string not found. CHECK CAREFULLY")
      (should (string-prefix-p "<tool_use_error>" (car results)))
      (should (string-match-p "old_string not found" (car results)))
      (should (string-suffix-p "</tool_use_error>" (car results)))
      ;; Already-wrapped message not double-wrapped
      (funcall cb "<tool_use_error>already wrapped</tool_use_error>")
      (should (equal (car results) "<tool_use_error>already wrapped</tool_use_error>")))))

(ert-deftest gptelt-edit-mcp-validation-errors-wrapped-test ()
  "Test that MCP validation errors are wrapped in <tool_use_error> tags."
  (let ((result nil))
    ;; edit_file validation error
    (gptelt-edit-edit-file-mcp (lambda (r) (setq result r))
                               nil "old" "new")
    (should (string-prefix-p "<tool_use_error>" result))
    (should (string-match-p "file_path.*required" result))
    ;; edit_buffer validation error
    (setq result nil)
    (gptelt-edit-edit-buffer-mcp (lambda (r) (setq result r))
                                 nil "old" "new")
    (should (string-prefix-p "<tool_use_error>" result))
    (should (string-match-p "buffer_name.*required" result))
    ;; multi_edit_file validation error
    (setq result nil)
    (gptelt-edit-multi-edit-file-mcp (lambda (r) (setq result r))
                                     "/tmp/foo.txt" nil)
    (should (string-prefix-p "<tool_use_error>" result))
    (should (string-match-p "edits.*required" result))
    ;; multi_edit_buffer validation error
    (setq result nil)
    (gptelt-edit-multi-edit-buffer-mcp (lambda (r) (setq result r))
                                       nil '((:old_string "a" :new_string "b")))
    (should (string-prefix-p "<tool_use_error>" result))
    (should (string-match-p "buffer_name.*required" result))))

(ert-deftest gptelt-edit-not-found-error-wrapped-test ()
  "Test that not-found errors from edit operations are wrapped."
  (let* ((temp-path (make-temp-file "gptelt-test-" nil ".txt"))
         (result-msg nil))
    (unwind-protect
        (progn
          (with-temp-file temp-path
            (insert "hello world\n"))
          (gptelt-edit-edit-file-mcp
           (lambda (r) (setq result-msg r))
           temp-path "nonexistent text" "replacement")
          (sleep-for 0.1)
          (should result-msg)
          (should (string-prefix-p "<tool_use_error>" result-msg))
          (should (string-match-p "old_string not found" result-msg))
          (should (string-suffix-p "</tool_use_error>" result-msg)))
      (when (file-exists-p temp-path) (delete-file temp-path)))))

(ert-deftest gptelt-edit-success-not-wrapped-test ()
  "Test that successful edits are NOT wrapped in error tags."
  (let* ((temp-path (make-temp-file "gptelt-test-" nil ".txt"))
         (result-msg nil))
    (unwind-protect
        (progn
          (with-temp-file temp-path
            (insert "hello world\n"))
          (gptelt-edit-edit-file-mcp
           (lambda (r) (setq result-msg r))
           temp-path "hello" "goodbye")
          (sleep-for 0.1)
          (should result-msg)
          (should (string-prefix-p "Successfully" result-msg))
          (should (not (string-match-p "<tool_use_error>" result-msg))))
      (when (file-exists-p temp-path) (delete-file temp-path)))))

(ert-deftest gptelt-edit-multi-edit-error-wrapped-test ()
  "Test that multi-edit failure errors are wrapped."
  (let* ((temp-path (make-temp-file "gptelt-test-" nil ".txt"))
         (result-msg nil))
    (unwind-protect
        (progn
          (with-temp-file temp-path
            (insert "aaa\nbbb\n"))
          (gptelt-edit-multi-edit-file-mcp
           (lambda (r) (setq result-msg r))
           temp-path
           (list (list :old_string "aaa" :new_string "AAA")
                 (list :old_string "NONEXISTENT" :new_string "XXX")))
          (sleep-for 0.1)
          (should result-msg)
          (should (string-prefix-p "<tool_use_error>" result-msg))
          (should (string-match-p "Multi-edit failed" result-msg)))
      (when (file-exists-p temp-path) (delete-file temp-path)))))

;;; edit-file-test.el ends here
