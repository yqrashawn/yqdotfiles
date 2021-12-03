;;; lang.el -*- lexical-binding: t; -*-

(use-package! jq-mode :mode (("\\.jq$" . jq-mode)))
(use-package! jenkinsfile-mode :mode ("\\Jenkinsfile\\'" . jenkinsfile-mode))
(use-package! adoc-mode :mode ("\\\.adoc\\\'" . adoc-mode))
(use-package! nginx-mode
  :mode
  (("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)
   ("nginx.conf" . nginx-mode))
  :config
  (add-hook! nginx-mode (cmd! (set-company-backend! 'nginx-mode #'company-nginx))))
(use-package! dotenv-mode :mode (("\\\.env\\..*\\\'" . dotenv-mode)))
(use-package! company-nginx :defer t)
(pushnew! auto-mode-alist
          '("\\.aspell\\.en\\.pws\\'" . conf-mode)
          '("\\.meta\\'" . conf-mode)
          '("\\manifest.webapp\\'" . json-mode)
          '("\\.eslintrc\\'" . json-mode)
          '("\\.swcrc\\'" . json-mode)
          '("\\.babelrc\\'" . json-mode))
(use-package! crontab-mode :defer t)
(use-package! jsonnet-mode :mode "\.jsonnet\'")

(after! lsp
  (setq!
   ;; lsp-imenu-sort-methods '(position)
   ;; lsp-eldoc-enable-hover nil
   ;; lsp-disabled-clients '(javascript-typescript-langserver)
   lsp-bash-highlight-parsing-errors t
   lsp-eslint-package-manager "yarn"
   lsp-eslint-run "onSave"
   lsp-eslint-auto-fix-on-save t))
(after! lsp-ui
  (setq! lsp-ui-doc-show-with-cursor nil
         lsp-ui-doc-header t
         lsp-ui-doc-include-signature t))

;; (use-package! tree-sitter-langs
;;   :defer t
;;   ;; :config
;;   ;; (pushnew! tree-sitter-major-mode-language-alist
;;   ;;           '(clojure-mode . clojure)
;;   ;;           '(clojurescript-mode . clojure)
;;   ;;           '(clojurec-mode . clojure))
;;   )

;; (use-package! tree-sitter
;;   :defer t
;;   :hook ((prog-mode text-mode) . +tree-sitter-manybe-enable)
;;   ;; :init
;;   ;; (setq! tree-sitter-hl-use-font-lock-keywords nil)
;;   )

(use-package! adoc-mode
  :mode (("\\.adoc$" . adoc-mode)
         ("\\.asciidoc$" . adoc-mode)))