;;; prog.el -*- lexical-binding: t; -*-

(setq +company-backend-alist
      '((text-mode (:separate company-dabbrev company-yasnippet company-files company-ispell))
        (prog-mode company-capf company-yasnippet company-files company-keywords company-dabbrev-code company-dabbrev)
        (conf-mode company-capf company-dabbrev-code company-yasnippet)))

(setq! projectile-project-search-path '("~/workspace/office" "~/workspace/home" "~/workspace/third"))

(use-package! company-flx
  :defer t
  :init (add-hook! emacs-lisp-mode #'company-flx-mode))

(after! dash-docs
  (setq! dash-docs-docsets-path
         (let ((original-dash-path (expand-file-name "~/Library/Application Support/Dash/DocSets")))
           (if (and (string-equal system-type 'darwin)
                    (file-directory-p original-dash-path))
               original-dash-path
             dash-docs))))