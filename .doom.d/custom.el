;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
    '((cider-shadow-watched-builds . ":app")
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
