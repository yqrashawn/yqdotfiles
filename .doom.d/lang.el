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

(setq! lsp-eslint-enable nil)
(after! lsp
  (advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))
  (lsp-register-client
   (make-lsp-client
    :new-connection
    (lsp-stdio-connection
     (lambda () (lsp-eslint-server-command))
     (lambda () (lsp-eslint-server-exists? (lsp-eslint-server-command))))
    :activation-fn (lambda (filename &optional _)
                     (when lsp-eslint-enable
                       (or (string-match-p (rx (one-or-more anything) "."
                                               (or "ts" "js" "jsx" "tsx" "html" "vue" "svelte") eos)
                                           filename)
                           (and (derived-mode-p 'js-mode 'js2-mode 'typescript-mode 'html-mode 'svelte-mode)
                                (not (string-match-p "\\.json\\'" filename))))))
    :priority -1
    :completion-in-comments? t
    :add-on? t
    :multi-root nil
    :notification-handlers (ht ("eslint/status" #'lsp-eslint-status-handler))
    :request-handlers (ht ("workspace/configuration" #'lsp-eslint--configuration)
                          ("eslint/openDoc" #'lsp-eslint--open-doc)
                          ("eslint/probeFailed" #'lsp-eslint--probe-failed))
    :async-request-handlers (ht ("eslint/confirmESLintExecution" #'lsp-eslint--confirm-local))
    :server-id 'eslint
    :initialized-fn (lambda (workspace)
                      (with-lsp-workspace workspace
                        (lsp--server-register-capability
                         (lsp-make-registration
                          :id "random-id"
                          :method "workspace/didChangeWatchedFiles"
                          :register-options? (lsp-make-did-change-watched-files-registration-options
                                              :watchers
                                              `[,(lsp-make-file-system-watcher
                                                  :glob-pattern "**/.eslintr{c.js,c.yaml,c.yml,c,c.json}")
                                                ,(lsp-make-file-system-watcher
                                                  :glob-pattern "**/.eslintignore")
                                                ,(lsp-make-file-system-watcher
                                                  :glob-pattern "**/package.json")])))))
    :download-server-fn (lambda (_client callback error-callback _update?)
                          (let ((tmp-zip (make-temp-file "ext" nil ".zip")))
                            (delete-file tmp-zip)
                            (lsp-download-install
                             (lambda (&rest _)
                               (condition-case err
                                   (progn
                                     (lsp-unzip tmp-zip lsp-eslint-unzipped-path)
                                     (funcall callback))
                                 (error (funcall error-callback err))))
                             error-callback
                             :url lsp-eslint-download-url
                             :store-path tmp-zip)))))
  (setq!
   ;; lsp-imenu-sort-methods '(position)
   ;; lsp-eldoc-enable-hover nil
   ;; lsp-disabled-clients '(javascript-typescript-langserver)
   lsp-bash-highlight-parsing-errors t
   lsp-eslint-package-manager "yarn"
   lsp-eslint-run "onSave"
   lsp-eslint-auto-fix-on-save t
   lsp-completion-show-detail nil
   lsp-completion-show-kind nil
   lsp-completion-sort-initial-results nil
   lsp-eslint-enable nil)
  (add-hook! 'lsp-configure-hook
    (cmd! (if (memq major-mode '(clojure-mode clojurescript-mode clojurec-mode))
              (setq-local company-tabnine--disabled t)
            (setq-local company-tabnine--disabled nil)))))
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