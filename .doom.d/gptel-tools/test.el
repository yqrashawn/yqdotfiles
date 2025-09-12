;;; .nixpkgs/.doom.d/gptel-tools/test.el -*- lexical-binding: t; -*-


;; tmp tests
(when (seq-empty-p
       (seq-filter
        (comp (partial 'string= "test.el"))
        (gptel-tools-list-buffers)))
  (error "gptel-tools-list-buffers"))
