;;; .nixpkgs/.doom.d/gptel-tools/create-file-test.el -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest gptelt-create-file-buffer-test ()
  "Test gptelt--create-file-buffer creates and writes file."
  (let* ((fname (expand-file-name (make-temp-name "gptelt-create-file-buffer-") temporary-file-directory)))
    (unwind-protect
        (progn
          (gptelt--create-file-buffer fname ";; create-file-buffer test\n(foo-bar 123)\n")
          (should (and (file-exists-p fname)
                       (string-match-p "foo-bar" (gptelt-read-file fname 0 2000)))))
      (when (file-exists-p fname) (delete-file fname)))))

;;; create-file-test.el ends here
