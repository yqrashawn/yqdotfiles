;;; completion/custom-capfs/config.el -*- lexical-binding: t; -*-

(defun +doom-buffer-files-capf ()
  "Cape capf for completing file paths of all buffers in doom-buffer-list.
Filters candidates by fuzzy matching input against truncated path (no root, root-1 folders).
Wraps candidates with =...= if current buffer is org-mode."
  (let ((buffers (doom-buffer-list)))
    (when buffers
      (let* ((input (thing-at-point 'filename t))
             (files (cl-remove-if-not #'identity (mapcar #'buffer-file-name buffers)))
             (bounds (if input
                         (bounds-of-thing-at-point 'filename)
                       (cons (point) (point))))
             (beg (car bounds))
             (end (cdr bounds))
             (filtered
              (if input
                  (cl-remove-if-not
                   (lambda (s)
                     (when s
                       (let* ((parts (split-string (expand-file-name s) "/" t))
                              (len (length parts))
                              (relpath
                               (if (> len 2)
                                   (mapconcat #'identity
                                              (nthcdr 2 parts) "/")
                                 (car (last parts)))))
                         (or (s-contains? input s)
                             (< (hotfuzz--cost input relpath) 10000)))))
                   files)
                files)))
        (list beg end
              (cond ((derived-mode-p 'org-mode)
                     (mapcar (lambda (s) (concat "=" s "=")) filtered))
                    ((eq major-mode 'agent-shell-mode)
                     (mapcar (lambda (s) (concat "`" s "`")) filtered))
                    filtered)
              :exclusive 'no
              :annotation-function (lambda (s) " BufferFile")
              :company-kind (lambda (_) 'file)
              :category 'doom-buffer-file)))))

(defun +doom-buffer-capf ()
  "Cape capf for completing buffer names of all buffers in doom-buffer-list.
Filters candidates by fuzzy matching input against buffer name.
Wraps candidates with =...= if current buffer is org-mode."
  (let ((buffers (doom-buffer-list)))
    (when buffers
      (let* ((input (thing-at-point 'symbol t))
             (names (mapcar #'buffer-name buffers))
             (bounds (if input
                         (bounds-of-thing-at-point 'symbol)
                       (cons (point) (point))))
             (beg (car bounds))
             (end (cdr bounds))
             (filtered (if input
                           (cl-remove-if-not
                            (lambda (s)
                              (and s (or (s-contains? input s)
                                         (< (hotfuzz--cost input s) 10000))))
                            names)
                         names)))
        (list beg end
              (cond ((derived-mode-p 'org-mode)
                     (mapcar (lambda (s) (concat "=" s "=")) filtered))
                    ((eq major-mode 'agent-shell-mode)
                     (mapcar (lambda (s) (concat "`" s "`")) filtered))
                    (t
                     filtered))
              :exclusive 'no
              :annotation-function (lambda (s) " BufferName")
              :company-kind (lambda (_) 'buffer)
              :category 'doom-buffer-name)))))

(add-hook!
 '(gptel-mode-hook agent-shell-mode-hook)
 (defun +custom-capf-setup-org-mode ()
   (add-hook 'completion-at-point-functions 
             #'+doom-buffer-capf 1000 t)
   (add-hook 'completion-at-point-functions #'+doom-buffer-files-capf 1000 t)))
