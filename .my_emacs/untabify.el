
(defun untabify-all ()
  "Untabify the current buffer, unless `untabify-this-buffer' is nil."
  (interactive)
  (and untabify-this-buffer (untabify (point-min) (point-max))))

(define-minor-mode untabify-mode
  "Untabify buffer on save." nil " untab" nil
  (make-variable-buffer-local 'untabify-this-buffer)
  (setq untabify-this-buffer (not (derived-mode-p 'makefile-mode)))
  (add-hook 'before-save-hook #'untabify-all))
(add-hook 'prog-mode-hook 'untabify-mode)
