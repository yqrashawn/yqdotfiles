(setq undohist-packages '(undohist))

(defun undohist/init-undohist ()
  (use-package undohist
    :init
    (setq undohist-directory (expand-file-name "undo-history" spacemacs-cache-directory))
    :config
    (undohist-initialize)))
