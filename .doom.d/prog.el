;;; prog.el -*- lexical-binding: t; -*-

(setq +company-backend-alist
      '((text-mode (:separate company-dabbrev company-yasnippet company-files company-ispell))
        (prog-mode company-tabnine company-capf company-yasnippet company-files company-keywords company-dabbrev-code company-dabbrev)
        (conf-mode company-tabnine company-capf company-dabbrev-code company-yasnippet)))

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

(defvar yq//company-numbers '(59 ?a ?s ?d ?f ?g ?h ?j ?k ?l))

(defun yq//company-format-numbers (numbered)
  (format " %s" (char-to-string (nth (mod numbered 10) yq//company-numbers))))

(after! company
  (setq! company-selection-wrap-around t
         company-show-numbers t
         company-require-match nil
         company-dabbrev-minimum-length 2
         company-search-regexp-function #'company-search-flex-regexp
         company-show-numbers-function 'yq//company-format-numbers)

  (dotimes (i 10)
    (define-key! company-active-map
      (read-kbd-macro (format "M-%d" i)) #'company-complete-number
      (read-kbd-macro (format "C-x C-6 %d" i)) #'company-complete-number)))

(use-package! company-tabnine
  :defer t
  :init
  (setq! company-tabnine-binaries-folder "~/.TabNine/binaries/"
         company-tabnine-context-radius 6000
         company-tabnine-context-radius-after 6000
         company-tabnine-log-file-path "~/Downloads/tabnine.log")
  :config
  (setq! company-tabnine--disabled t) )
(use-package! copy-as-format :defer t)
(use-package! separedit :defer t)