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

(setq! lsp-use-plists t
       lsp-auto-guess-root nil
       lsp-copilot-enabled nil
       lsp-eslint-multi-root t)

(after! lsp-mode
  ;; https://github.com/emacs-lsp/lsp-mode/issues/3577#issuecomment-1709232622
  (delete 'lsp-terraform lsp-client-packages)
  ;; (cl-callf2 delq 'lsp-ui-mode lsp-mode-hook)
  (setq!
   lsp-completion-provider :none
   lsp-log-io nil
   lsp-copilot-enabled nil
   lsp-diagnostics-provider :flycheck
   lsp-disabled-clients '(copilot-ls graphql-lsp)
   lsp-bash-highlight-parsing-errors t
   lsp-eslint-package-manager "npm"
   lsp-eslint-run "onSave"
   lsp-eslint-auto-fix-on-save t
   lsp-semantic-tokens-enable nil
   lsp-completion-show-detail nil
   lsp-completion-show-kind nil
   lsp-completion-filter-on-incomplete nil
   lsp-completion-sort-initial-results nil
   lsp-keep-workspace-alive nil
   lsp-enable-folding nil
   lsp-enable-on-type-formatting nil
   lsp-enable-file-watchers nil
   lsp-before-save-edits t
   lsp-lens-enable nil)
  (pushnew! lsp-signature-auto-activate :after-completion)
  (pushnew! lsp-file-watch-ignored-directories
            "[/\\\\]coverage'"
            "[/\\\\]lcov-report'"
            "[/\\\\]\\.next\\'"
            "[/\\\\]\\.wrangler\\'"
            "[/\\\\]\\.log\\'"
            "[/\\\\]\\.logs\\'"
            "[/\\\\]\\storybook-static"
            "[/\\\\]\\.storybook"
            "[/\\\\]releases"
            "[/\\\\]build'"
            "[/\\\\]cljs-runtime"
            "[/\\\\]dist"
            "[/\\\\]__snapshots__'"
            "[/\\\\]sp_"))

(after! consult-lsp
  (defun +consult-lsp--diagnostics--transformer (file diag)
    (unless (and
             (string= (or (lsp-get diag :source) "") "typescript")
             (s-contains? "Could not find a declaration file for module" (or (lsp-get diag :message) "")))
      (consult-lsp--diagnostics--transformer file diag)))
  (setq! consult-lsp-diagnostics-transformer-function '+consult-lsp--diagnostics--transformer))

(after! lsp-ui
  (setq!
   lsp-ui-sideline-enable nil
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

(add-hook! '(go-ts-mode-hook
             typescript-ts-mode-hook
             tsx-ts-mode-hook
             jtsx-tsx-mode-hook
             jtsx-jsx-mode-hook
             nix-mode-hook)
           :append #'lsp)

(use-package! treesit-auto
  :hook (doom-first-file . global-treesit-auto-mode)
  :init
  (setq! treesit-auto-install t)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all))

(after! js-json-mode
  (set-formatter! 'prettier-json
    (alist-get 'prettier-json apheleia-formatters)
    :modes '(js-json-mode json-mode json-ts-mode jsonnet-mode)))

(after! sql-mode
  (set-formatter! 'pg-fluff
    '("sqlfluff" "fix" "--nocolor" "--dialect" "postgres" "--force" "-")
    :modes '(sql-mode)))

;;; lsp booster
(progn
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))

  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?) ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection)) ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (when-let ((command-from-exec-path (executable-find (car orig-result)))) ;; resolve command from exec-path (in case not found in $PATH)
              (setcar orig-result command-from-exec-path))
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))

  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

;; use lsp as formatter
(after! lsp-mode
  (cl-defun +apheleia-lsp-format-buffer
      (&key buffer scratch callback &allow-other-keys)
    "Copy BUFFER to SCRATCH, then format scratch, then call CALLBACK."
    (let* ((workspaces (with-current-buffer buffer (lsp-workspaces))))
      (with-current-buffer scratch
        (let ((buffer-file-name (buffer-local-value 'buffer-file-name buffer)))
          (with-lsp-workspaces workspaces (lsp-format-buffer)))
        (funcall callback)))))

(use-package treesit
  :init
  (setq! +pair-chars (seq-map 'string-to-char '("(" ")" "<" ">" "{" "}" "[" "]")))
  :config
  (after! expand-region
    (defadvice! +er/prepare-for-more-expansions-for-treesit-modes ()
      "fix the error that it requires 2 expand-region call to select things in some treesit modes, for example typescript-ts-mode"
      :after #'er/prepare-for-more-expansions
      (when (and
             (treesit-language-at (point))
             (region-active-p)
             (= (region-end) (+ (region-beginning) 1))
             (memq (char-after) +pair-chars))
        (call-interactively #'er/expand-region 1)))))

(use-package combobulate
  :init
  (setq! combobulate-key-prefix "C-c o")
  :hook ((python-ts-mode
          js-ts-mode
          html-ts-mode
          css-ts-mode
          yaml-ts-mode
          yaml-pro-ts-mode
          typescript-ts-mode
          json-ts-mode
          jtsx-tsx-mode
          jtsx-jsx-mode
          tsx-ts-mode) . combobulate-mode)
  :config
  (transient-define-prefix combobulate ()
    "Structured Editing and Navigation with Combobulate"
    ["Navigating and Searching"
     ["Linear / Explicit"
      ("b" "Logical prev" combobulate-navigate-logical-previous :transient t)
      ("e" "Logical next" combobulate-navigate-logical-next :transient t)
      ("j" "Forward sexp" forward-sexp :transient t)
      ("k" "Backward sexp" backward-sexp :transient t)
      ("M-p" "Previous sequent" combobulate-navigate-sequence-previous :transient t)
      ("M-n" "Next sequent" combobulate-navigate-sequence-next :transient t)]
     ["Hierarchical"
      ("h" "Up into list" combobulate-navigate-up :transient t)
      ("f" "Down into list" combobulate-navigate-down :transient t)
      ("C-M-p" "Backward sibling" combobulate-navigate-previous :transient t)
      ("C-M-n" "Forward sibling" combobulate-navigate-next :transient t)]
     ["Querying"
      ("B" "Query Builder …" combobulate-query)
      ("x" "Xref …" combobulate-xref)
      ("H" "Highlight …" combobulate-highlight)]
     [:description (lambda () (concat
                               (propertize "Defun " 'face 'transient-heading)
                               (format "(to: %s)"
                                       (propertize
                                        (symbol-name combobulate-beginning-of-defun-behavior)
                                        'face
                                        'font-lock-doc-face))))
                   ("C-M-a" "Beginning of defun" combobulate-navigate-beginning-of-defun :transient t)
                   ("C-M-e" "End of defun" combobulate-navigate-end-of-defun :transient t)]]
    ["Editing and Marking"
     ["Marking"
      ("C-M-h" "Mark defun" combobulate-mark-defun :transient t)
      ("M-h" "Expand region" combobulate-mark-node-dwim :transient t)]
     ["Editing"
      ("M-k" "Kill node DWIM" combobulate-kill-node-dwim :transient t)
      ("t" "Edit …" combobulate-edit)
      ("c" "Clone node DWIM" combobulate-clone-node-dwim)
      ("C-M-t" "Transpose sexp" combobulate-transpose-sexps)
      ("C-M-e" "Envelop …" combobulate-envelop)
      ("M-P" "Drag node up" combobulate-drag-up :transient t)
      ("M-N" "Drag node down" combobulate-drag-down :transient t)]
     ["Splicing"
      ("M-<up>" "Elevate before and out" combobulate-splice-up :transient t)
      ("M-<down>" "Elevate after and out" combobulate-splice-down :transient t)
      ("M-<left>" "Elevate self and out" combobulate-splice-self :transient t)
      ("M-<right>" "Elevate all and out" combobulate-splice-parent)]])
  ;; (assq-delete-all 'combobulate-mode minor-mode-map-alist)
  )

(use-package! yaml-pro
  :mode (("\\.yaml$" . yaml-pro-ts-mode)))

(after! treesit-fold
  (pushnew! treesit-fold-range-alist
            `(jtsx-tsx-mode . ,(treesit-fold-parsers-typescript))
            `(jtsx-typescript-mode . ,(treesit-fold-parsers-typescript))
            `(jtsx-javascript-mode . ,(treesit-fold-parsers-javascript))
            `(jtsx-jsx-mode . ,(treesit-fold-parsers-javascript))))
