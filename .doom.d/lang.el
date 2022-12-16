;;; lang.el -*- lexical-binding: t; -*-

(use-package! jq-mode :mode (("\\.jq$" . jq-mode)))
;(use-package! jenkinsfile-mode :mode ("\\Jenkinsfile\\'" . jenkinsfile-mode))
(use-package! adoc-mode :mode ("\\\.adoc\\\'" . adoc-mode))
(use-package! nginx-mode
  :mode
  (("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)
   ("nginx.conf" . nginx-mode))
  ;; :config
  ;; (set-company-backend! 'nginx-mode #'company-nginx)
  )
(use-package! dotenv-mode :mode (("\\\.env\\..*\\\'" . dotenv-mode)))
;; (use-package! company-nginx :defer t)
(pushnew! auto-mode-alist
          '("\\.aspell\\.en\\.pws\\'" . conf-mode)
          '("\\.meta\\'" . conf-mode)
          '("\\manifest.webapp\\'" . json-mode)
          '("\\.eslintrc\\'" . json-mode)
          '("\\.swcrc\\'" . json-mode)
          '("\\.babelrc\\'" . json-mode))
(use-package! crontab-mode :defer t)
(use-package! jsonnet-mode :mode "\.jsonnet\'")

(defun +disable-lsp-watcher-in-some-project ()
  (setq-local lsp-enable-file-watchers nil)
  ;; (when (string= (directory-file-name (doom-project-root)) (directory-file-name (getenv "HOME")))
  ;;   (setq-local lsp-enable-file-watchers nil))
  )

;; (setq! +lsp-company-backends
;;          (if (modulep! :editor snippets)
;;              '(:separate company-tabnine-capf company-files company-yasnippet)
;;            '(:separate company-tabnine-capf company-files)))
(setq! +lsp-company-backends nil)

;; (advice-add #'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))
(setq-default lsp-enable-file-watchers nil)
(setq! lsp-use-plists t)

(defun ++lsp-init-company-backends-h ()
  (when lsp-completion-mode
    (make-local-variable 'company-backends)
    (setq company-backends (remq nil company-backends))
    (setq company-backends (remq 'company-tabnine company-backends))
    (setq company-backends (remq 'company-capf company-backends))
    (cond
     ((memq major-mode +lispy-modes)
      (setq company-backends (pushnew! company-backends ;; 'company-tabnine
                                       'company-capf)))
     (t
      (setq company-backends (pushnew! company-backends 'company-capf 'company-tabnine))))))

(after! lsp-mode
  (setq-hook! '(go-mode-hook) lsp-headerline-breadcrumb-enable t)
  (pushnew! lsp-language-id-configuration '((nix-mode . "nix")))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                    :major-modes '(nix-mode)
                    :server-id 'nix))
  (when (modulep! :completion company)
    (add-hook! 'lsp-completion-mode-hook :append '++lsp-init-company-backends-h t))
  ;; (delq! 'lsp-ui-mode lsp-mode-hook)
  (setq!
   ;; lsp-imenu-sort-methods '(position)
   ;; lsp-eldoc-enable-hover nil
   ;; lsp-disabled-clients '(javascript-typescript-langserver)
   lsp-enable-file-watchers nil
   lsp-file-watch-threshold 200
   lsp-bash-highlight-parsing-errors t
   lsp-eslint-package-manager "yarn"
   lsp-eslint-run "onSave"
   lsp-eslint-auto-fix-on-save t
   lsp-completion-show-detail nil
   lsp-completion-show-kind nil
   lsp-completion-sort-initial-results nil)
  (pushnew! lsp-file-watch-ignored-directories "[/\\\\]coverage'" "[/\\\\]lcov-report'" "[/\\\\]\\.log\\'" "[/\\\\]\\.clj-kondo" "[/\\\\]\\storybook-static" "[/\\\\]\\.storybook" "[/\\\\]releases" "[/\\\\]\\.yarn" "[/\\\\]\\.vscode" "[/\\\\]build'" "[/\\\\]\\.shadow-cljs" "[/\\\\]cljs-runtime" "[/\\\\]dist" "[/\\\\]__snapshots__'" "[/\\\\]sp_")
  (after! flycheck
    (defadvice! +flycheck-get-next-checker-for-buffer (orig-fn checker)
      "support multiple flycheck checkers with lsp checker"
      :around #'flycheck-get-next-checker-for-buffer
      (if (eq checker 'lsp)
          (cond
           ((derived-mode-p 'go-mode) 'golangci-lint)
           (t (funcall orig-fn checker)))
        (funcall orig-fn checker)))))

(after! consult-lsp
  (defun +consult-lsp--diagnostics--transformer (file diag)
    (unless (and
             (string= (or (lsp-get diag :source) "") "typescript")
             (s-contains? "Could not find a declaration file for module" (or (lsp-get diag :message) "")))
      (consult-lsp--diagnostics--transformer file diag)))
  (setq! consult-lsp-diagnostics-transformer-function '+consult-lsp--diagnostics--transformer))

(after! lsp-ui
  (setq!
   lsp-ui-doc-show-with-cursor nil
   lsp-ui-imenu-auto-refresh t
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

(use-package! turbo-log
  :defer t
  :config
  (plist-put turbo-log--ecmascript-configs :include-semicolon nil)
  (setq turbo-log-msg-format-template "\"🚀: %s\"")
  (setq turbo-log-allow-insert-without-tree-sitter-p t))

(use-package! carp-mode
  :defer t
  :mode (("\\.carp$" . carp-mode)))

(use-package! inf-carp-mode
  :defer t
  :init
  (setq! inf-carp-program (expand-file-name "~/Downloads/carp-v0.5.4-x86_64-macos/bin/carp"))
  :config
  (setenv "CARP_DIR" (expand-file-name "~/Downloads/carp-v0.5.4-x86_64-macos/")))

(use-package! protobuf-mode
  :mode
  (("\\.proto" . protobuf-mode)))
