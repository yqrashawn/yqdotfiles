(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-safe-remote-resources
    '("\\`https://raw\\.githubusercontent\\.com/yqrashawn/org-html-themes/master/org/theme-bigblow\\.setup\\'"))
 '(safe-local-variable-values
    '((eval add-hook 'before-save-hook
        (lambda nil
          (if
            (fboundp 'org-make-toc)
            (org-make-toc)
            (message-box "Please install org-make-toc.")))
        nil t)
       (org-edit-src-content-indentation 0)
       (eval cl-flet
         ((enhance-imenu-lisp
            (&rest keywords)
            (dolist
              (keyword keywords)
              (let
                ((prefix
                   (when
                     (listp keyword)
                     (cl-second keyword)))
                  (keyword
                    (if
                      (listp keyword)
                      (cl-first keyword)
                      keyword)))
                (add-to-list 'lisp-imenu-generic-expression
                  (list
                    (purecopy
                      (concat
                        (capitalize keyword)
                        (if
                          (string=
                            (substring-no-properties keyword -1)
                            "s")
                          "es" "s")))
                    (purecopy
                      (concat "^\\s-*("
                        (regexp-opt
                          (list
                            (if prefix
                              (concat prefix "-" keyword)
                              keyword)
                            (concat prefix "-" keyword))
                          t)
                        "\\s-+\\(" lisp-mode-symbol-regexp "\\)"))
                    2))))))
         (enhance-imenu-lisp
           '("bookmarklet-command" "define")
           '("class" "define")
           '("command" "define")
           '("ffi-method" "define")
           '("ffi-generic" "define")
           '("function" "define")
           '("internal-page-command" "define")
           '("internal-page-command-global" "define")
           '("mode" "define")
           '("parenscript" "define")
           "defpsmacro"))
       (vc-prepare-patches-separately)
       (diff-add-log-use-relative-names . t)
       (vc-git-annotate-switches . "-w")
       (+cljr--log-spy-with-error . t)
       (+clojure-use-zprint-formatter . t)
       (cider-ns-refresh-after-fn . "integrant.repl/resume")
       (cider-ns-refresh-before-fn . "integrant.repl/suspend")
       (mangle-whitespace . t)
       (elisp-lint-indent-specs
         (describe . 1)
         (it . 1)
         (thread-first . 0)
         (cl-flet . 1)
         (cl-flet* . 1)
         (org-element-map . defun)
         (org-roam-dolist-with-progress . 2)
         (org-roam-with-temp-buffer . 1)
         (org-with-point-at . 1)
         (magit-insert-section . defun)
         (magit-section-case . 0)
         (org-roam-with-file . 2))
       (elisp-lint-ignored-validators "byte-compile" "package-lint")
       (elisp-lint-indent-specs
         (if-let* . 2)
         (when-let* . 1)
         (let* . defun)
         (nrepl-dbind-response . 2)
         (cider-save-marker . 1)
         (cider-propertize-region . 1)
         (cider-map-repls . 1)
         (cider--jack-in . 1)
         (cider--make-result-overlay . 1)
         (insert-label . defun)
         (insert-align-label . defun)
         (insert-rect . defun)
         (cl-defun . 2)
         (with-parsed-tramp-file-name . 2)
         (thread-first . 0)
         (thread-last . 0))
       (eval progn
         (pp-buffer)
         (indent-buffer))
       (eval let
         ((tools-file
            (concat
              (pwd)
              "scripts/tools.el")))
         (when
           (file-exists-p tools-file)
           (load-file tools-file)))
       (js2-mode-show-strict-warnings)
       (js2-mode-show-parse-errors)
       (eval define-clojure-indent
         (l/matcha
           '(1
              (:defn)))
         (l/matche
           '(1
              (:defn)))
         (p.types/def-abstract-type
           '(1
              (:defn)))
         (p.types/defprotocol+
           '(1
              (:defn)))
         (p.types/defrecord+
           '(2 nil nil
              (:defn)))
         (p.types/deftype+
           '(2 nil nil
              (:defn)))
         (p/def-map-type
           '(2 nil nil
              (:defn)))
         (p/defprotocol+
           '(1
              (:defn)))
         (p/defrecord+
           '(2 nil nil
              (:defn)))
         (p/deftype+
           '(2 nil nil
              (:defn)))
         (tools.macro/macrolet
           '(1
              ((:defn))
              :form)))
       (eval put 'p.types/defprotocol+ 'clojure-doc-string-elt 2)
       (eval put 's/defn 'clojure-doc-string-elt 2)
       (eval put 'setting/defsetting 'clojure-doc-string-elt 2)
       (eval put 'defsetting 'clojure-doc-string-elt 2)
       (eval put 'api/defendpoint-async 'clojure-doc-string-elt 3)
       (eval put 'api/defendpoint 'clojure-doc-string-elt 3)
       (eval put 'define-premium-feature 'clojure-doc-string-elt 2)
       (eval put 'defendpoint-async 'clojure-doc-string-elt 3)
       (eval put 'defendpoint 'clojure-doc-string-elt 3)
       (ftf-project-finders ftf-get-top-git-dir)
       (eval defun cider--running-nrepl-paths nil
         '("~/Dropbox/sync/Alfred.alfredpreferences/workflows/user.workflow.B178FD26-4DD8-4281-922B-925CD4AC61CE/"))
       (eval defun cider--running-nrepl-paths nil
         '("~/.config/bblib/"))
       (projectile-project-root . "~/Dropbox/ORG")
       (counsel-rg-base-command "rg" "--max-columns" "240" "--with-filename" "--no-heading" "--line-number" "--color" "never" "-uu" "%s")
       (projectile-project-root . "~/Dropbox/ORG")
       (org-export-with-title . t)
       (org-export-with-properties)
       (eval require 'org-make-toc)
       (elisp-lint-indent-specs
         (if-let* . 2)
         (when-let* . 1)
         (let* . defun)
         (nrepl-dbind-response . 2)
         (cider-save-marker . 1)
         (cider-propertize-region . 1)
         (cider-map-repls . 1)
         (cider--jack-in . 1)
         (cider--make-result-overlay . 1)
         (insert-label . defun)
         (insert-align-label . defun)
         (insert-rect . defun)
         (cl-defun . 2)
         (with-parsed-tramp-file-name . 2)
         (thread-first . 1)
         (thread-last . 1))
       (cider-shadow-cljs-default-options . "app")
       (checkdoc-package-keywords-flag)
       (buffer-save-without-query . t)
       (cider-default-cljs-repl . shadow)
       (cider-shadow-cljs-default-options . "fr")
       (eval let
         ((tools-file
            (concat
              (car
                (dir-locals-find-file "."))
              "scripts/tools.el")))
         (when
           (file-exists-p tools-file)
           (load-file tools-file)))
       (eval load-file
         (concat
           (car
             (dir-locals-find-file "."))
           "scripts/tools.el"))
       (eval add-hook 'find-file-hook '+jest-setup-integration-test nil t)
       (eval defun +jest-setup-integration-test nil
         (if
           (+jest-integration-test-file-p)
           (with-eval-after-load 'jest
             (setq-local jest-executable "yarn run test:integration"))
           (with-eval-after-load 'jest
             (setq-local jest-executable "yarn run test:unit"))))
       (eval defun +jest-integration-test-file-p nil
         (and
           (buffer-file-name)
           (string-match-p "\\.integration.test\\.js"
             (buffer-file-name))))
       (eval add-hook 'find-file-hook '+ensure-jest-expect nil t)
       (eval defun +ensure-jest-expect nil
         (when
           (+jest-test-file-p)
           (+insert-jest-expect)))
       (eval defun +jest-test-file-p nil
         (and
           (buffer-file-name)
           (string-match-p "\\.\\(integration.test\\|spec\\|test\\)\\.js"
             (buffer-file-name))))
       (eval defun +insert-jest-expect nil
         (save-excursion
           (goto-char
             (point-min))
           (when
             (not
               (re-search-forward "from.*@jest/globals"
                 (point-max)
                 t))
             (insert "// eslint-disable-next-line no-unused-vars\12import {expect, describe, test, it, jest, afterAll, afterEach, beforeAll, beforeEach} from '@jest/globals' // prettier-ignore\12")
             (insert "// eslint-disable-next-line no-unused-vars\12import waitForExpect from 'wait-for-expect'\12"))))
       (projectile-project-root . "~/.doom.d/")))
 '(warning-suppress-types '((before-save-hook))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ts-fold-replacement-face ((t (:foreground nil :box nil :inherit font-lock-comment-face :weight light)))))
