;;; .nixpkgs/.doom.d/gptel-tools/edit-file-test.el -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest gptelt-edit-edit-file-buffer-temp-test ()
  "Test create-file-buffer, edit-file, and edit-buffer (with temp files), including replace_all."
  (let* ((temp-el-path (gptelt--create-temp-file-buffer ";; TEMP TEST FILE\n(foo 1)\n(foo 1)\n(bar 2)\n" nil ".el"))
         (temp-txt-path (gptelt--create-temp-file-buffer ";; new temp file\ndata: test123\ndata: test123" nil ".txt")))
    (unwind-protect
        (progn
          ;; edit-file with replace_all=nil (only first)
          (gptelt-edit-edit-file temp-el-path "(foo 1)" "(foo 42)")
          (let ((el-content (gptelt-read-file temp-el-path 0 2000)))
            (should (and el-content (string-match-p "(foo 42)" el-content)))
            (should (string-match-p "(foo 1)" el-content)))
          ;; edit-file with replace_all=t (replace all)
          (gptelt-edit-edit-file temp-el-path "(foo 1)" "(foo 77)" t)
          (let ((el-content2 (gptelt-read-file temp-el-path 0 2000)))
            (should (not (string-match-p "(foo 1)" el-content2)))
            (should (string-match-p "(foo 77)" el-content2)))

          ;; Edit temp .txt file and verify replace_all=t
          (gptelt-edit-edit-file temp-txt-path "test123" "xyz789" t)
          (let ((txt-content (gptelt-read-file temp-txt-path 0 2000)))
            (should (and txt-content
                         (not (string-match-p "test123" txt-content))
                         (string-match-p "xyz789" txt-content))))

          ;; Edit buffer for temp .el file and verify default (first only)
          (let* ((buf (find-file-noselect temp-el-path))
                 (old "(bar 2)")
                 (new "(bar 99)"))
            (gptelt-edit-edit-buffer (buffer-name buf) old new)
            (let ((new-content (with-current-buffer buf (buffer-string))))
              (should (string-match-p "(bar 99)" new-content))))
          ;; Edit buffer with replace_all (append extra, then replace all)
          (with-current-buffer (find-file-noselect temp-txt-path)
            (goto-char (point-max))
            (insert "\ndata: xyz789")
            (save-buffer))
          (let* ((buf2 (find-file-noselect temp-txt-path)))
            (gptelt-edit-edit-buffer (buffer-name buf2) "xyz789" "abc333" t)
            (let ((new-content2 (with-current-buffer buf2 (buffer-string))))
              (should (not (string-match-p "xyz789" new-content2)))
              (should (>= (cl-count ?a new-content2) 2)))))
      (when (file-exists-p temp-el-path) (delete-file temp-el-path))
      (when (file-exists-p temp-txt-path) (delete-file temp-txt-path)))))


(ert-deftest gptelt-edit-multi-edit-buffer-test ()
  "Test multi_edit_buffer tool on two temp buffers with and without replace_all."
  (let* ((temp1-path
          (gptelt--create-temp-file-buffer ";; TEMP1\n(foo 1)\n(foo 1)\n(bar 2)\n" nil ".el"))
         (temp2-path
          (gptelt--create-temp-file-buffer ";; TEMP2\ndata: alpha\ndata: beta\ndata: alpha" nil ".txt")))
    (unwind-protect
        (progn
          (let*
              ((buf1 (find-file-noselect temp1-path))
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
