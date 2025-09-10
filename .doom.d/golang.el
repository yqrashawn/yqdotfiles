;;; .nixpkgs/.doom.d/golang.el -*- lexical-binding: t; -*-

(add-hook! go-mode '(hl-line-mode which-func-try-to-enable))

(after! lsp-mode
  (setq-hook! '(go-mode-hook) lsp-headerline-breadcrumb-enable t)
  (after! flycheck
    (defadvice! +flycheck-get-next-checker-for-buffer (orig-fn checker)
      "support multiple flycheck checkers with lsp checker"
      :around #'flycheck-get-next-checker-for-buffer
      (if (eq checker 'lsp)
          (cond
           ((derived-mode-p 'go-mode) 'golangci-lint)
           (t (funcall orig-fn checker)))
        (funcall orig-fn checker)))))

(add-hook! go-mode
  (defun +go-mode-idle-highlight ()
    (idle-highlight-mode 1))
  (defadvice! ++go--spawn (_orig cmd)
    :around #'+go--spawn
    (detached-shell-command cmd)))
