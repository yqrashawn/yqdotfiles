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
          (gptelt-edit-edit-file temp-el-path "(foo 1)" "(foo 42)")
          (with-temp-buffer
            (insert-file-contents temp-el-path)
            (let ((content (buffer-string)))
              (should (string-match-p "(foo 42)" content))
              (should (string-match-p "(foo 1)" content))))
          
          ;; edit-file with replace_all=t (replace all)
          (gptelt-edit-edit-file temp-el-path "(foo 1)" "(foo 77)" t)
          (with-temp-buffer
            (insert-file-contents temp-el-path)
            (let ((content (buffer-string)))
              (should (not (string-match-p "(foo 1)" content)))
              (should (string-match-p "(foo 77)" content))))

          ;; Edit temp .txt file and verify replace_all=t
          (gptelt-edit-edit-file temp-txt-path "test123" "xyz789" t)
          (with-temp-buffer
            (insert-file-contents temp-txt-path)
            (let ((content (buffer-string)))
              (should (not (string-match-p "test123" content)))
              (should (string-match-p "xyz789" content))))

          ;; Edit buffer for temp .el file and verify default (first only)
          (let* ((buf (find-file-noselect temp-el-path))
                 (old "(bar 2)")
                 (new "(bar 99)"))
            (gptelt-edit-edit-buffer (buffer-name buf) old new)
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

            (gptelt-edit-multi-edit-buffer (buffer-name buf1) edits1)
            (gptelt-edit-multi-edit-buffer (buffer-name buf2) edits2)

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
