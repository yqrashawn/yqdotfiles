;;; .nixpkgs/.doom.d/gptel-tools/elisp-test.el -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest gptel-tools-evaluate-elisp-buffer-test ()
  "Test gptel-evaluate-elisp-buffer sets value in a test buffer."
  (let* ((test-buffer (generate-new-buffer "gptel-eval-buffer-test")))
    (unwind-protect
        (progn
          (with-current-buffer test-buffer (insert "(setq gptel-eval-buffer-test-value 42)"))
          ;; Simulate evaluation (skip prompt for test)
          (let ((old-yn (symbol-function 'y-or-n-p)))
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (gptel-evaluate-elisp-buffer (buffer-name test-buffer))))
          (should (and (boundp 'gptel-eval-buffer-test-value)
                       (= gptel-eval-buffer-test-value 42))))
      (when (boundp 'gptel-eval-buffer-test-value)
        (makunbound 'gptel-eval-buffer-test-value))
      (kill-buffer test-buffer))))

(ert-deftest gptel-tools-evaluate-elisp-file-test ()
  "Test gptel-evaluate-elisp-file sets value in a temp file."
  (let* ((file-path (make-temp-file "gptel-eval-file-test" nil ".el"))
         (content "(setq gptel-eval-file-test-value 99)")
         (_ (with-temp-file file-path (insert content))))
    (unwind-protect
        (progn
          (let ((old-yn (symbol-function 'y-or-n-p)))
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (gptel-evaluate-elisp-file file-path)))
          (should (and (boundp 'gptel-eval-file-test-value)
                       (= gptel-eval-file-test-value 99))))
      (when (boundp 'gptel-eval-file-test-value)
        (makunbound 'gptel-eval-file-test-value))
      (when (file-exists-p file-path) (delete-file file-path)))))
