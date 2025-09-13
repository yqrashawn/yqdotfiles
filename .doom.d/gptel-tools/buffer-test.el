;;; .nixpkgs/.doom.d/gptel-tools/buffer-test.el -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest gptel-tools-list-buffers-test ()
  "Test: list-buffers contains all the main tool files."
  (should (member "*Messages*" (gptel-tools-list-buffers))))

(ert-deftest gptel-tools-filter-buffers-regex-test ()
  "Test filter-buffers-regex returns correct subset."
  (let ((filtered (gptel-tools-filter-buffers-regex "ss")))
    (should (member "*Messages*" filtered))))

(ert-deftest gptel-tools-get-buffer-file-path-test ()
  "Test get-buffer-file-path returns correct paths."
  (let ((pairs `(("buffer-test.el"
                  ,(expand-file-name "~/.nixpkgs/.doom.d/gptel-tools/buffer-test.el")))))
    (dolist (pair pairs)
      (let ((buf (car pair))
            (path (cadr pair)))
        (should (equal (gptel-tools--get-buffer-file-path buf) path))))))

(ert-deftest gptel-tools-get-file-buffer-name-test ()
  "Test get-file-buffer-name returns correct buffer name for a file path."
  (let* ((file-path (expand-file-name "~/.nixpkgs/.doom.d/gptel-tools/buffer-test.el"))
         (expected-buffer "buffer-test.el"))
    (should (equal (gptel-tools--get-file-buffer-name file-path) expected-buffer))))

(ert-deftest gptel-tools-visible-buffers-test ()
  "Test visible-buffers returns all currently visible tool buffers."
  (should (not (seq-empty-p (gptel-tools-visible-buffers)))))
