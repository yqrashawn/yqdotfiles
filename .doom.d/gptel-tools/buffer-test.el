;;; .nixpkgs/.doom.d/gptel-tools/buffer-test.el -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest gptelt-list-buffers-test ()
  "Test: list-buffers contains all the main tool files."
  (should (member "*Messages*" (gptelt-list-buffers))))

(ert-deftest gptelt-filter-buffers-regex-test ()
  "Test filter-buffers-regex returns correct subset."
  (let ((filtered (gptelt-filter-buffers-regex "ss")))
    (should (member "*Messages*" filtered))))

(ert-deftest gptelt-get-buffer-file-path-test ()
  "Test get-buffer-file-path returns correct paths."
  (let ((pairs `(("buffer-test.el"
                  ,(expand-file-name "~/.nixpkgs/.doom.d/gptel-tools/buffer-test.el")))))
    (dolist (pair pairs)
      (let ((buf (car pair))
            (path (cadr pair)))
        (should (equal (gptelt--get-buffer-file-path buf) path))))))

(ert-deftest gptelt-get-file-buffer-name-test ()
  "Test get-file-buffer-name returns correct buffer name for a file path."
  (let* ((file-path (expand-file-name "~/.nixpkgs/.doom.d/gptel-tools/buffer-test.el"))
         (expected-buffer "buffer-test.el"))
    (should (equal (gptelt--get-file-buffer-name file-path) expected-buffer))))

(ert-deftest gptelt-visible-buffers-test ()
  "Test visible-buffers returns all currently visible tool buffers."
  (should (not (seq-empty-p (gptelt-visible-buffers)))))
