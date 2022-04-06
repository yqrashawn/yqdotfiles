(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
    '((cider-shadow-cljs-default-options . "app")
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
       (projectile-project-root . "~/.doom.d/"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ts-fold-replacement-face ((t (:foreground nil :box nil :inherit font-lock-comment-face :weight light)))))
