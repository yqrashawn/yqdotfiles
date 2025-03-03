;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
    '((cider-preferred-build-tool . shadow-cljs)
       (+cider-project-reload-exec-cmd-clj . "(shadow.user/x)")
       (cider-preferred-build-tool quote shadow-cljs)
       (cider-clojure-cli-aliases . ":cljs")
       (cider-jack-in-default quote shadow-cljs)
       (lsp-sqls-workspace-config-path . "root")
       (+cider-project-reload-exec-cmd-clj . "(user/x)")
       (cider-clojure-cli-aliases . ":dev")
       (cider-shadow-watched-builds . ":app"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
