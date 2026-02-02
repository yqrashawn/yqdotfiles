;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
    '((elisp-lint-indent-specs (if-let* . 1) (if-let . 1)
        (mcp-server-lib-test--with-server . 0)
        (mcp-server-lib-test--with-tools . 1)
        (mcp-server-lib-test--register-tool . 1)
        (mcp-server-lib-test--with-resources . 1)
        (mcp-server-lib-test--register-resource . 2)
        (mcp-server-lib-test--register-resource-template . 2)
        (mcp-server-lib-test--with-resource-templates . 1)
        (mcp-server-lib-test--with-undefined-function . 1)
        (mcp-server-lib-test--with-request . defun)
        (mcp-server-lib-test--with-error-tracking . 1)
        (mcp-server-lib-test--check-resource-read-error . 0)
        (mcp-server-lib-ert-with-metrics-tracking . 1)
        (mcp-server-lib-ert-verify-req-success . defun) (cl-defstruct))
       (cider-clojure-cli-aliases . ":dev")
       (cider-preferred-build-tool . clojure-cli)
       (cider-shadow-watched-builds . ":app")
       (cider-jack-in-cmd . "clojure =DEPS= -M:duct --nrepl --cider")
       (cider-repl-init-code "(ns user)")
       (cider-ns-refresh-after-fn . "integrant.repl/resume")
       (cider-ns-refresh-before-fn . "integrant.repl/suspend")
       (cider-jack-in-cmd . "clojure -M:duct --nrepl --cider"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
