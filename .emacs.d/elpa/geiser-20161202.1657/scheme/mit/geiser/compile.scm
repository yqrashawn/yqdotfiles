(declare (usual-integrations))

(load-option 'CREF)

(with-working-directory-pathname
    (directory-pathname (current-load-pathname))
  (lambda ()
    (cf "emacs")
    (cref/generate-constructors "geiser" 'ALL)))
