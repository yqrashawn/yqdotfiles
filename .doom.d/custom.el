;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
    '((cider-clojure-cli-aliases . ":dev:test")
       (eval progn (pp-buffer) (indent-buffer))
       (+cider-project-reload-exec-cmd-clj . "(user/x)")
       (cider-clojure-cli-aliases . ":dev")
       (cider-preferred-build-tool . clojure-cli)
       (cider-shadow-watched-builds . ":app")
       (lsp-sqls-workspace-config-path . "root")
       (elisp-lint-indent-specs (if-let* . 2) (when-let* . 1) (let* . defun)
         (nrepl-dbind-response . 2) (cider-save-marker . 1)
         (cider-propertize-region . 1) (cider-map-repls . 1)
         (cider--jack-in . 1) (cider--make-result-overlay . 1)
         (insert-label . defun) (insert-align-label . defun)
         (insert-rect . defun) (cl-defun . 2) (with-parsed-tramp-file-name . 2)
         (thread-first . 0) (thread-last . 0)
         (transient-define-prefix . defmacro)
         (transient-define-suffix . defmacro))
       (checkdoc-package-keywords-flag) (cider-clojure-cli-aliases . ":cljs")
       (cider-preferred-build-tool . shadow-cljs)
       (+cider-project-reload-exec-cmd-clj . "(shadow.user/x)"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
