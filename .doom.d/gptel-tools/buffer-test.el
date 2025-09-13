;;; .nixpkgs/.doom.d/gptel-tools/buffer-test.el -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest gptel-tools-list-buffers-test ()
  "Test: list-buffers contains all the main tool files."
  (should (member "*Messages*" (gptel-tools-list-buffers))))

(ert-deftest gptel-tools-filter-buffers-regex-test ()
  "Test filter-buffers-regex returns correct subset."
  (let ((filtered (gptel-tools-filter-buffers-regex "file")))
    (should (and (member "edit-file.el" filtered)
                 (member "create-file.el" filtered)
                 (not (member "buffer.el" filtered))))))

(ert-deftest gptel-tools-get-buffer-file-path-test ()
  "Test get-buffer-file-path returns correct paths."
  (let ((pairs `(("buffer-test.el"
                  ,(expand-file-name "~/.nixpkgs/.doom.d/gptel-tools/buffer-test.el")))))
    (dolist (pair pairs)
      (let ((buf (car pair))
            (path (cadr pair)))
        (should (equal (gptel-tools--get-buffer-file-path buf) path))))))

(ert-deftest gptel-tools-visible-buffers-test ()
  "Test visible-buffers returns all currently visible tool buffers."
  (should (not (seq-empty-p (gptel-tools-visible-buffers)))))
