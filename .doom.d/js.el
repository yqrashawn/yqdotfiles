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

(defun +js-test-file-p ()
  (let ((path (buffer-file-name)))
    (and path (string-match-p "\.\\(test\\|spec\\)\.\\(j\\|t\\)s$" path))))

(setq +js-test-file-outline-regexp "\\(^\s*//\\(\\(//\\)+\\| ?\\(#\\|;\\|*\\)+\\) [^\n]\\)\\|\\(^\s*\\(describe\\|test\\|beforeEach\\|beforeAll\\)(\\)")

(defun +js-test-outline-level ()
  "Get the outline level of jest test file"
  (let ((s (match-string 0)))
    (when (string-match "^\s*" s)
      (+ (length (match-string 0 s)) 1))))

(defun +setup-js-test-file-outline ()
  (interactive)
  (when (+js-test-file-p)
      (setq-local outline-regexp +js-test-file-outline-regexp
                  outline-level '+js-test-outline-level
                  ;; outline-level 'outline-level
                  )
      (and (functionp 'outline-minor-faces-add-font-lock-keywords)
           (outline-minor-faces-add-font-lock-keywords))))

(add-hook! rjsx-mode '+setup-js-test-file-outline)

(after! lsp-mode
  (setq! lsp-clients-typescript-log-verbosity "off"
         lsp-clients-typescript-server-args '("--stdio"
                                              "--log-level"
                                              "1"
                                              "--tsserver-log-verbosity"
                                              "off"))
  (let ((vsintel (car (seq-filter
                       (lambda (s) (string-match-p "visualstudioexptteam\.vscodeintellicode-" s))
                       (directory-files (expand-file-name "~/.vscode/extensions/"))))))
    (when vsintel
      (setq! lsp-clients-typescript-plugins (vector (list :name "@vsintellicode/typescript-intellicode-plugin" :location vsintel)))))

  (defadvice! +lsp--get-buffer-diagnostics (orig-fn)
    :around #'lsp--get-buffer-diagnostics
    (seq-filter
     (lambda (i)
       (if (hash-table-p i)
           (not (string-match-p
                 "Could not find a declaration file for module .* implicitly has an .*any.* type"
                 (gethash "message" i)))
         t))
     (funcall orig-fn))))