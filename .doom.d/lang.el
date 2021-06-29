;;; lang.el -*- lexical-binding: t; -*-

(use-package! jq-mode :mode (("\\.jq$" . jq-mode)))
(use-package! jenkinsfile-mode :mode ("\\Jenkinsfile\\'" . jenkinsfile-mode))
(use-package! tree-sitter-langs :defer t)
(use-package! tree-sitter
  :hook ((prog-mode text-mode) . +tree-sitter-manybe-enable)
  :init
  (setq! tree-sitter-hl-use-font-lock-keywords nil)
  :config
  (pushnew! tree-sitter-major-mode-language-alist
            '(clojure-mode . clojure)
            '(clojurescript-mode . clojure)
            '(clojurec-mode . clojure)))