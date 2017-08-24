;; packages.el --- Navagation Layer packages File for Spacemacs
;; Author: yqrashawn <namy.19@gmail.com>
(setq navigation-packages
      '(
        ace-jump-buffer
        ))

(defun navigation/init-ace-jump-buffer ()
  (use-package ace-jump-buffer
    :init
    (progn
      (setq bs-max-window-height 30
            bs-attributes-list (quote (("" 2 2 left " ")
                                       ("" 1 1 left bs--get-marked-string)
                                       ("" 1 1 left " ")
                                       ("Buffer" bs--get-name-length 10 left bs--get-name))))

      ;; filter buffers to current perspective
      (make-ace-jump-buffer-function "code"
        (with-current-buffer buffer
          (string-match "^*" (buffer-name buffer))))

      )))
