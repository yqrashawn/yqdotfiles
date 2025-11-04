;;; .nixpkgs/.doom.d/gptel-tools/imenu.el -*- lexical-binding: t; -*-

(defun get-buffer-imenu-symbols (buffer)
  "Get all imenu symbols from BUFFER.
Returns the index alist for the buffer's symbols.
Signals an error if the buffer doesn't support imenu."
  (with-current-buffer buffer
    (unless (or (and imenu-prev-index-position-function
                     imenu-extract-index-name-function)
                imenu-generic-expression
                (not (eq imenu-create-index-function
                         'imenu-default-create-index-function)))
      (error "Buffer %s does not support imenu" (buffer-name buffer)))
    (let ((index-alist (imenu--make-index-alist)))
      (if (and index-alist (equal (car index-alist) imenu--rescan-item))
          (cdr index-alist)
        index-alist))))

(comment
  (get-buffer-imenu-symbols (current-buffer)))

;;; imenu.el ends here
