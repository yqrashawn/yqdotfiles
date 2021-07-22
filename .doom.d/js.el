;;; js.el -*- lexical-binding: t; -*-

(use-package! jest
  :commands (jest-popup)
  :init
  (setq! jest-executable "yarn test")
  (pushnew! evil-collection-mode-list 'jest-mode)
  (pushnew! evil-normal-state-modes 'jest-mode)
  (set-popup-rule! "^\\*jest\\*" :side 'right :width 0.4 :vslot 2 :quit 'current :select nil))

(use-package! rjsx-mode
  :defer t
  :mode (("\\.cjs\\'" . rjsx-mode)
         ("\\.mjs\\'" . rjsx-mode)
         ("\\.js\\'" . rjsx-mode)
         ("\\.tsx\\'" . rjsx-mode)
         ("components\\/.*\\.js\\'" . rjsx-mode))
  :defer t
  :commands (rjsx-delete-creates-full-tag rjsx-electric-gt rjsx-electric-lt rjsx-rename-tag-at-point))

(after! npm-mode
  (defadvice! +npm-mode--exec-process (orig-fn cmd &optional comint)
    :around #'npm-mode--exec-process
    (apply orig-fn (s-replace-all '(("npm" . "yarn")) cmd comint))))

(when (boundp '+ligatures-extra-alist)
  (dolist (mode '(rjsx-mode
                  js2-mode
                  typescript-mode
                  web-mode))
    (with-eval-after-load mode
      (delq! (assq mode +ligatures-extra-alist) +ligatures-extra-alist)
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
                      :for "for"
                      :return "return"
                      ;; Other
                      ;; :yield "import"
                      :alist
                      '(("async" . ?⊳)
                        ("await" . ?⊲)
                        ("throw" . ?Ƭ)
                        ("this" . ?ƭ)
                        ("import" . ?⇟)
                        ("export" . ?⇞)
                        ("const" . ?Ċ)
                        ("Promise" . ?Ṗ)
                        ("if" . ?␦)
                        ("let" . ?ḷ))))))
