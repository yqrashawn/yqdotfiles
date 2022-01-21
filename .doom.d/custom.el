(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
    '((Syntax . ANSI-Common-Lisp)
       (Base . 10)
       (eval defun cider--running-nrepl-paths nil
         '("~/.config/bblib/"))
       (eval defun cider--running-nrepl-paths nil
         '("~/local/bin/"))
       (eval defun cider--running-nrepl-paths nil
         '("~/Dropbox/sync/Alfred.alfredpreferences/workflows/user.workflow.B178FD26-4DD8-4281-922B-925CD4AC61CE/"))
       (eval defun cider--running-nrepl-paths nil
         '("~/.config/yabai/"))
       (eval defun cider--running-nrepl-paths nil
         '((expand-file-name "~/.config/yabai/")))
       (eval defun cider--running-nrepl-paths nil
         (expand-file-name "~/.config/yabai/"))
       (eval defun cider--running-nrepl-paths nil "~/.config/yabai/")
       (elisp-lint-indent-specs
         (describe . 1)
         (it . 1))
       (projectile-project-root . "~/.doom.d/")
       (cider-shadow-cljs-command . "yarn run shadow-cljs")
       (cider-shadow-watched-builds quote
         ("server" "front"))
       (projectile-project-root . "~/.doom.d")
       (projectile-project-root . "~/Dropbox/ORG")
       (counsel-rg-base-command "rg" "--max-columns" "240" "--with-filename" "--no-heading" "--line-number" "--color" "never" "-uu" "%s")
       (vulpea-id-auto-targets nil)
       (elisp-lint-indent-specs
         (vulpea-utils-with-file . 1)
         (vulpea-utils-with-note . 1)
         (org-with-point-at . 1))
       (cider-shadow-cljs-default-options . "server")
       (eval add-hook 'before-save-hook 'time-stamp)
       (cider-default-cljs-repl . shadow)
       (cider-shadow-cljs-default-options . "app")
       (elisp-lint-indent-specs
         (describe . 1)
         (it . 1)
         (org-element-map . defun)
         (org-roam-with-temp-buffer . 1)
         (org-with-point-at . 1)
         (magit-insert-section . defun)
         (magit-section-case . 0)
         (->> . 1)
         (org-roam-with-file . 2))
       (elisp-lint-ignored-validators "byte-compile" "package-lint")
       (mangle-whitespace . t)
       (eval require 'org-roam-dev)
       (comment-fill-column . 80)
       (eval add-to-list 'projectile-globally-ignored-directories "[/\\\\]conflux-portal[/\\]dist$")
       (eval add-to-list 'projectile-globally-ignored-directories "[/\\\\]conflux-portal[/\\]builds$")
       (eval progn
         (pp-buffer)
         (indent-buffer))
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
       (checkdoc-package-keywords-flag)
       (ffip-project-root . "/Users/yqrashawn/.emacs.d/")
       (projectile-project-name . ".emacs.d")
       (projectile-project-root . "/Users/yqrashawn/.emacs.d/")
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
       (eval projectile-register-project-type 'yarn
         '("package.json")
         :project-file "package.json" :compile "yarn install" :test "yarn test" :test-suffix ".test")))
 '(warning-suppress-log-types '((iedit) (iedit) (lsp-on-idle-hook) (use-package)))
 '(warning-suppress-types '((lsp-mode) (iedit) (lsp-on-idle-hook) (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ts-fold-replacement-face ((t (:foreground nil :box nil :inherit font-lock-comment-face :weight light)))))
