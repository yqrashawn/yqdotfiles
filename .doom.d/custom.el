(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((eval progn
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
 '(warning-suppress-log-types '((use-package) (use-package)))
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
