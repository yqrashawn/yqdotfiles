;;; .nixpkgs/.doom.d/gptel-tools/create-file-test.el -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest gptel-create-file-buffer-test ()
  "Test gptel-tools--create-file-buffer creates and writes file."
  (let* ((fname (expand-file-name (make-temp-name "gptel-create-file-buffer-") temporary-file-directory)))
    (unwind-protect
        (progn
          (gptel-tools--create-file-buffer fname ";; create-file-buffer test\n(foo-bar 123)\n")
          (should (and (file-exists-p fname)
                       (string-match-p "foo-bar" (gptel-tools-read-file fname 0 2000)))))
      (when (file-exists-p fname) (delete-file fname)))))
