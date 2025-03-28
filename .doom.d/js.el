;;; js.el -*- lexical-binding: t; -*-

(use-package! jest
  :commands (jest-popup)
  :init
  (setq! jest-executable "yarn test")
  (after! evil-collection
    (pushnew! evil-collection-mode-list 'jest-mode))
  (pushnew! evil-normal-state-modes 'jest-mode)
  :config
  (defadvice! +jest--project-root (orig-fn)
    :around #'jest--project-root
    (interactive)
    (doom-project-root)))

(use-package! rjsx-mode
  :defer t
  :mode (("\\.cjs\\'" . rjsx-mode)
         ("\\.mjs\\'" . rjsx-mode)
         ("\\.js\\'" . rjsx-mode)
         ;; ("\\.tsx\\'" . tsx-ts-mode)
         ("components\\/.*\\.js\\'" . rjsx-mode))
  :defer t
  :commands (rjsx-delete-creates-full-tag rjsx-electric-gt rjsx-electric-lt rjsx-rename-tag-at-point))

;; (add-hook! (rjsx-mode js2-mode tsx-ts-mode js-ts-mode) 'glasses-mode)

(after! npm-mode
  (defadvice! +npm-mode--exec-process (orig-fn cmd &optional comint)
    :around #'npm-mode--exec-process
    (apply orig-fn (s-replace-all '(("npm" . "yarn")) cmd comint))))

(when (boundp '+ligatures-extra-alist)
  (dolist (mode '(rjsx-mode
                  js2-mode
                  typescript-mode
                  typescript-ts-mode
                  tsx-ts-mode
                  web-mode))
    (with-eval-after-load mode
      (cl-callf2 delq (assq mode +ligatures-extra-alist) +ligatures-extra-alist)
      (set-ligatures! mode
        ;; Functional
        :def "function"
        :lambda "() =>"
        :composition "compose"
        ;; Types
        :null "null"
        :true "true" :false "false"
        ;; Flow
        :not "!"
        :and "&&" :or "||"
        ;; :for "for"
        ;; :return "return"
        )
      ;; Other
      ;; :yield "import"
      ;; :alist
      ;; '(("async " . ?⊳)
      ;;   ("await " . ?⊲)
      ;;   ("throw " . ?Ƭ)
      ;;   ("import " . ?ⅈ)
      ;;   ("export " . ?ⅇ)
      ;;   ("export default" . ?ⅆ)
      ;;   ("const " . ?ℂ)
      ;;   ("Promise" . ?⁋)
      ;;   (".then" . ?⇛)
      ;;   (".catch" . ?⇏)
      ;;   ("if " . ?␦)
      ;;   ("let " . ?ℿ))

      )))

(after! lsp-mode
  (setq-hook! '(rjsx-mode-hook js2-mode-hook js-mode-hook typescript-mode-hook)
    +format-with-lsp nil)

  (setq! lsp-clients-typescript-log-verbosity "off"
         lsp-clients-typescript-server-args '("--stdio"
                                              "--log-level"
                                              "1")
         lsp-clients-deno-enable-unstable t)
  (let ((vsintel (car (seq-filter
                       (lambda (s) (string-match-p "visualstudioexptteam\.vscodeintellicode-" s))
                       (directory-files (expand-file-name "~/.vscode/extensions/")))))
        (snapshot-tool (car (seq-filter
                             (lambda (s) (string-match-p "asvetliakov\.snapshot-tools-" s))
                             (directory-files (expand-file-name "~/.vscode/extensions/"))))))
    (when vsintel
      (setq! lsp-clients-typescript-plugins
             (vector
              (list
               :name "@vsintellicode/typescript-intellicode-plugin"
               :location vsintel)
              (list
               :name "@snapshot-tools/typescript-snapshots-plugin"
               :location vsintel)))))

  ;; (defadvice! +lsp--get-buffer-diagnostics (orig-fn)
  ;;   :around #'lsp--get-buffer-diagnostics
  ;;   (seq-filter
  ;;    (lambda (i)
  ;;      (if (hash-table-p i)
  ;;          (and
  ;;           ;; ts-ls
  ;;           (not (string-match-p "Could not find a declaration file for module .* implicitly has an .*any.* type" (gethash "message" i)))
  ;;           ;; deno
  ;;           (not (string-match-p "Relative import path .* not prefixed with .*file:.*" (gethash "message" i))))
  ;;        t))
  ;;    (funcall orig-fn)))
  )

(use-package! typescript-ts-mode
  :mode (("\\.cts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . typescript-ts-mode)
         ("\\.ts\\'" . typescript-ts-mode))
  :init
  (after! apheleia
    (set-formatter!
      'prettier-typescript
      (alist-get 'prettier-typescript apheleia-formatters)
      :modes '(typescript-ts-mode tsx-ts-mode typescript-mode))))

(after! apheleia
  (set-formatter!
    'prettier-javascript
    (alist-get 'prettier-javascript apheleia-formatters)
    :modes '(rjsx-mode js-mode js2-mode js-ts-mode)))

(add-hook! '+format-with-lsp-mode-hook
  (defun +turn-off-format-with-lsp-mode-for-modes ()
    (when +format-with-lsp-mode
      (when (memq major-mode '(typescript-mode
                               typescript-ts-mode
                               tsx-ts-mode
                               rjsx-mode
                               js-mode
                               js2-mode
                               js-ts-mode))
        (+format-with-lsp-mode -1)))))

(use-package! ts-refactor
  :hook (typescript-ts-mode tsx-ts-mode)
  :config
  (when (modulep! :editor evil +everywhere)
    (add-hook 'ts-refactor-mode-hook #'evil-normalize-keymaps)
    (let ((ts-refactor-mode-map (evil-get-auxiliary-keymap ts-refactor-mode-map 'normal t t)))
      (ts-refactor-add-keybindings-with-prefix (format "%s r" doom-localleader-key)))))

(def-project-mode! ++javascript-npm-mode
  :modes '(html-mode
           css-mode
           web-mode
           markdown-mode
           js-mode                     ; includes js2-mode and rjsx-mode
           json-mode
           typescript-mode
           typescript-ts-mode
           typescript-tsx-mode
           tsx-ts-mode
           solidity-mode)
  :when (locate-dominating-file default-directory "package.json")
  :add-hooks '(+javascript-add-npm-path-h npm-mode))

(use-package! lsp-tailwindcss
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  :config
  (dolist (tw-major-mode
           '(css-mode
             css-ts-mode
             typescript-mode
             typescript-ts-mode
             tsx-ts-mode
             rjsx-mode
             js2-mode
             js-ts-mode
             clojure-mode
             clojurescript-mode))
    (add-to-list 'lsp-tailwindcss-major-modes tw-major-mode)))
