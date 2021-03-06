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

(after! lsp
  (setq!
   ;; lsp-imenu-sort-methods '(position)
   ;; lsp-eldoc-enable-hover nil
   ;; lsp-disabled-clients '(javascript-typescript-langserver)
   lsp-bash-highlight-parsing-errors t
   lsp-eslint-package-manager "yarn"
   lsp-eslint-run "onSave"
   lsp-eslint-auto-fix-on-save t)
  (add-hook! lsp-ui-mode (cmd! () (when (memq major-mode '(clojurescript-mode clojure-mode clojurec-mode)) (lsp-ui-mode -1)))))

(use-package! tree-sitter-langs
  :defer t
  :config
  (pushnew! tree-sitter-major-mode-language-alist
            '(clojure-mode . clojure)
            '(clojurescript-mode . clojure)
            '(clojurec-mode . clojure)))

(use-package! tree-sitter
  :hook ((prog-mode text-mode) . +tree-sitter-manybe-enable)
  :init
  (setq! tree-sitter-hl-use-font-lock-keywords nil))