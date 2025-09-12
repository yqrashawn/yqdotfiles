;;; .nixpkgs/.doom.d/gptel-tools/test.el -*- lexical-binding: t; -*-


(setq-local debug-on-error t)
;;; create-file-buffer
(let* ((fname (expand-file-name (make-temp-name "gptel-create-file-buffer-") temporary-file-directory))
       (result (gptel-tools--create-file-buffer fname ";; create-file-buffer test\n(foo-bar 123)\n")))
  (unless (and (file-exists-p fname)
               (string-match-p "foo-bar" (gptel-tools-read-file fname)))
    (error "create-file-buffer did not create or write file: %s" fname)))

;;; read-buffer
(let* ((buf (generate-new-buffer "gptel-read-buffer-test")))
  (unwind-protect
      (progn
        (with-current-buffer buf (insert "ReadBufferTest123"))
        (unless (string-match-p "ReadBufferTest123" (gptel-tools-read-buffer "gptel-read-buffer-test"))
          (error "read-buffer did not match injected content")))
    (kill-buffer buf)))

;;; multi-edit-buffer
(let* ((fname (make-temp-file "gptel-multiedit-test"))
       (buf (find-file-noselect fname))
       (init-str "a\nb\nc\n")
       (_ (with-current-buffer buf (insert init-str)))
       (edits '(("a" . "x") ("b" . "y"))))
  (unwind-protect
      (let* ((result
              (gptel-edit-tool-multi-edit-buffer (buffer-name buf) edits))
             (content (with-current-buffer buf (buffer-string))))
        (unless (and (string-match-p "x" content)
                     (string-match-p "y" content)
                     (not (string-match-p "a" content)))
          (error "multi-edit-buffer failed: %s" content)))
    (kill-buffer buf)))

;;; multi-edit-file
(let* ((fpath (gptel-tools--create-temp-file-buffer "1-foo\n2-bar\n3-baz\n" nil ".txt"))
       (edits '(("foo" . "FOO") ("bar" . "BAR") ("baz" . "BAZ"))))
  (gptel-edit-tool-multi-edit-file fpath edits)
  (let ((content (gptel-tools-read-file fpath)))
    (unless (and (string-match-p "FOO" content)
                 (string-match-p "BAR" content)
                 (string-match-p "BAZ" content))
      (error "multi-edit-file failed: %s" content))))

;;; glob (should find at least one el file in project)
(let ((results (gptel-rg-tool-glob "*.el" "project" 10)))
  (unless (and (listp results) results (seq-some (lambda (f) (string-match-p "\\.el$" f)) results))
    (error "glob did not return any .el files: %s" results)))

;;; list-ripgrep-types (should return an alist with known types)
(let ((types (gptel-rg-tool-list-type-aliases)))
  (unless (and (listp types) (assoc "elisp" types))
    (error "list-ripgrep-types missing 'elisp': %S" types)))

;;; get-file-types (should recognize .el file as elisp)
(let* ((tmppath (gptel-tools--create-temp-file-buffer ";; hi" nil ".el"))
       (types (gptel-rg-tool-get-file-types tmppath)))
  (unless (and (listp types) (member "elisp" types))
    (error "get-file-types did not recognize elisp: %S" types)))

;;; search-content (should find a known string in this file)
(let* ((thisfile (buffer-file-name (current-buffer)))
       (needle "lexical-binding")
       (results (gptel-rg-tool-search-content needle (file-name-directory thisfile) "*.el" t t 0 10)))
  (unless (seq-some (lambda (r) (string-match-p needle (plist-get r :content))) results)
    (error "search-content did not find expected pattern: %S" results)))

;;; search-literal (should find "lexical-binding" in this file)
(let* ((thisfile (buffer-file-name (current-buffer)))
       (results (gptel-rg-tool-search-literal "lexical-binding" (file-name-directory thisfile) "*.el" 10)))
  (unless (seq-some (lambda (r) (string-match-p "lexical-binding" (plist-get r :content))) results)
    (error "search-literal did not find pattern")))

;;; search-regex (should find "lexical-binding" using regex)
(let* ((thisfile (buffer-file-name (current-buffer)))
       (results (gptel-rg-tool-search-regex "lexical-binding" (file-name-directory thisfile) "*.el" 10)))
  (unless (seq-some (lambda (r) (string-match-p "lexical-binding" (plist-get r :content))) results)
    (error "search-regex did not find pattern")))

;;; Test: list-buffers contains all the main tool files
(unless (member "test.el" (gptel-tools-list-buffers))
  (error "Buffer missing from list-buffers: %s" buf))

;;; Test filter-buffers-regex returns correct subset
(let ((filtered (gptel-tools-filter-buffers-regex "file")))
  (unless (and (member "edit-file.el" filtered)
               (member "create-file.el" filtered)
               (not (member "buffer.el" filtered)))
    (error "filter-buffers-regex failed: %S" filtered)))

;;; Test get-buffer-file-path returns correct paths
(let ((pairs '(("ripgrep.el" "/Users/yqrashawn/.nixpkgs/.doom.d/gptel-tools/ripgrep.el")
               ("buffer.el" "/Users/yqrashawn/.nixpkgs/.doom.d/gptel-tools/buffer.el"))))
  (dolist (pair pairs)
    (let ((buf (car pair))
          (path (cadr pair)))
      (unless (equal (gptel-tools--get-buffer-file-path buf) path)
        (error "get-buffer-file-path failed for %s" buf)))))

;;; Test visible-buffers returns all currently visible tool buffers
(let ((vis (gptel-tools-visible-buffers)))
  (unless (member (get-buffer "test.el") vis)
    (error "visible-buffers missing: test.el")))

;;; Test create-file-buffer and edit-file/edit-buffer tools with dynamically generated temp files.
(let* ((temp-el-path (gptel-tools--create-temp-file-buffer ";; TEMP TEST FILE\n(foo 1)\n(bar 2)\n" nil ".el"))
       (temp-txt-path (gptel-tools--create-temp-file-buffer ";; new temp file\ndata: test123" nil ".txt")))
  ;; Edit temp .el file and verify
  (progn
    (gptel-edit-tool-edit-file temp-el-path "(foo 1)" "(foo 42)")
    (let ((el-content (gptel-tools-read-file temp-el-path)))
      (unless (and el-content (string-match-p "(foo 42)" el-content))
        (error "edit-file failed on el temp file: %S" el-content))))
  ;; Edit temp .txt file and verify
  (progn
    (gptel-edit-tool-edit-file temp-txt-path "test123" "xyz789")
    (let ((txt-content (gptel-tools-read-file temp-txt-path)))
      (unless (and txt-content (string-match-p "xyz789" txt-content))
        (error "edit-file failed on txt temp file: %S" txt-content))))
  ;; Edit buffer for temp .el file and verify
  (let* ((buf (find-file-noselect temp-el-path))
         (old "(bar 2)")
         (new "(bar 99)")
         (edit-result (gptel-edit-tool-edit-buffer (buffer-name buf) old new))
         (new-content (with-current-buffer buf (buffer-string))))
    (unless (string-match-p "(bar 99)" new-content)
      (error "edit-buffer failed: %S" new-content))))
