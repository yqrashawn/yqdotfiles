(define-key evil-normal-state-map "su" 'spacemacs/select-custom-layout)
(spacemacs|define-custom-layout "@LMV"
  :binding "l"
  :body
  (progn
    (defconst lmv-spacemacs-layout-name      "LMV" "LMV")
    (defun spacemacs-layouts/add-lmv-buffer-to-persp ()
      (if (or
           (string-match "gltflmvviewer" buffer-file-truename))
          (persp-add-buffer (current-buffer)
                            (persp-get-by-name
                             lmv-spacemacs-layout-name))))
    (add-hook 'prog-mode-hook #'spacemacs-layouts/add-lmv-buffer-to-persp)
    (find-file "~/workspace/OFFICE/gltflmvviewer/")
    (prodigy-start-service (prodigy-find-service "lmv front dev"))))

(spacemacs|define-custom-layout "@MAIL"
  :binding "m"
  :body
  (progn
    (defconst mail-spacemacs-layout-name      "Mail" "Mail")
    (defun spacemacs-layouts/add-mail-buffer-to-persp ()
      (persp-add-buffer (current-buffer)
                        (persp-get-by-name
                         mail-spacemacs-layout-name)))
    (mu4e)))

(spacemacs|define-custom-layout "@ELISP"
  :binding "e"
  :body
  (progn
    (defconst mail-spacemacs-layout-name      "Elisp" "Elisp")
    (defun spacemacs-layouts/add-elsip-buffer-to-persp ()
      (if (or (string-match (find-buffer-visiting (current-buffer)))
              (eq major-mode 'emacs-lisp-mode))
          (persp-add-buffer (current-buffer)
                            (persp-get-by-name
                             lmv-spacemacs-layout-name))))
    (add-hook 'emacs-lisp-mode #'spacemacs-layouts/add-lmv-buffer-to-persp)
    (spacemacs/find-dotfile)))
