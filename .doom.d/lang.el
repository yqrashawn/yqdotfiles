;;; lang.el -*- lexical-binding: t; -*-

(use-package! jq-mode :mode (("\\.jq$" . jq-mode)))
;; (use-package! jenkinsfile-mode :mode ("\\Jenkinsfile\\'" . jenkinsfile-mode))
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

(when (modulep! :lang nim)
  (after! nim-mode
    (add-hook! 'nim-mode-hook #'lsp))
  (after! nim-suggest
    (defadvice! +nimsuggest--call-sync (orig-fn method callback)
      "nim-log in nimsuggest--call-sync takes to much cpu and mem"
      :around #'nimsuggest--call-sync
      (cl-letf (((symbol-function 'nim-log) (lambda (&rest _args))))
        (funcall orig-fn method callback)))))

(after! lsp-mode
  (setq! lsp-completion-provider :none)
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

;; (after! lsp-graphql
;;   (setf (lsp--client-priority (gethash 'graphql-lsp lsp-clients)) -3))

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

(use-package! adoc-mode
  :mode (("\\.adoc$" . adoc-mode)
         ("\\.asciidoc$" . adoc-mode)))

(use-package! turbo-log
  :defer t
  :config
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

(add-hook! '(go-ts-mode-hook typescript-ts-mode-hook tsx-ts-mode-hook) :append #'lsp)

(use-package! treesit
  :config
  (setq! treesit-language-source-alist '((bash "https://github.com/tree-sitter/tree-sitter-bash")
                                         (c "https://github.com/tree-sitter/tree-sitter-c")
                                         (clojure "https://github.com/sogaiu/tree-sitter-clojure")
                                         (cmake "https://github.com/uyha/tree-sitter-cmake")
                                         (common-lisp "https://github.com/theHamsta/tree-sitter-commonlisp")
                                         (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                                         (css "https://github.com/tree-sitter/tree-sitter-css")
                                         (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
                                         (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
                                         (elisp "https://github.com/Wilfred/tree-sitter-elisp")
                                         (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
                                         (fennel "https://github.com/TravonteD/tree-sitter-fennel")
                                         (fish "https://github.com/ram02z/tree-sitter-fish")
                                         (git-rebase "https://github.com/the-mikedavis/tree-sitter-git-rebase")
                                         (gitignore "https://github.com/shunsambongi/tree-sitter-gitignore")
                                         (git-commit "https://github.com/gbprod/tree-sitter-gitcommit")
                                         (gitattributes "https://github.com/ObserverOfTime/tree-sitter-gitattributes")
                                         (go "https://github.com/tree-sitter/tree-sitter-go")
                                         (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
                                         (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
                                         (html "https://github.com/tree-sitter/tree-sitter-html")
                                         (hlsl "https://github.com/theHamsta/tree-sitter-hlsl")
                                         (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
                                         (json "https://github.com/tree-sitter/tree-sitter-json")
                                         (julia "https://github.com/tree-sitter/tree-sitter-julia")
                                         (make "https://github.com/alemuller/tree-sitter-make")
                                         (lua "https://github.com/MunifTanjim/tree-sitter-lua")
                                         (nix "https://github.com/nix-community/tree-sitter-nix")
                                         (ocaml "https://github.com/tree-sitter/tree-sitter-ocaml")
                                         (perl "https://github.com/tree-sitter-perl/tree-sitter-perl")
                                         (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
                                         (rust "https://github.com/tree-sitter/tree-sitter-rust")
                                         (solidity "https://github.com/JoranHonig/tree-sitter-solidity")
                                         (sql "https://github.com/DerekStride/tree-sitter-sql")
                                         (scheme "https://github.com/6cdh/tree-sitter-scheme")
                                         (toml "https://github.com/ikatyang/tree-sitter-toml")
                                         (zig "https://github.com/maxxnino/tree-sitter-zig")
                                         (xml . ("https://github.com/ObserverOfTime/tree-sitter-xml" "master" "tree-sitter-xml/src"))
                                         (python "https://github.com/tree-sitter/tree-sitter-python")
                                         (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
                                         (yaml "https://github.com/ikatyang/tree-sitter-yaml"))))
