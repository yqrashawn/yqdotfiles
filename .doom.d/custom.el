(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
    '((eval let
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
         (p\.types/def-abstract-type
           '(1
              (:defn)))
         (p\.types/defprotocol+
           '(1
              (:defn)))
         (p\.types/defrecord+
           '(2 nil nil
              (:defn)))
         (p\.types/deftype+
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
         (tools\.macro/macrolet
           '(1
              ((:defn))
              :form)))
       (eval put 'p\.types/defprotocol+ 'clojure-doc-string-elt 2)
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
             (insert "// eslint-disable-next-line no-unused-vars
import {expect, describe, test, it, jest, afterAll, afterEach, beforeAll, beforeEach} from '@jest/globals' // prettier-ignore
")
             (insert "// eslint-disable-next-line no-unused-vars
import waitForExpect from 'wait-for-expect'
"))))
       (projectile-project-root . "~/.doom.d/")))
 '(warning-suppress-types '((before-save-hook))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ts-fold-replacement-face ((t (:foreground nil :box nil :inherit font-lock-comment-face :weight light)))))
