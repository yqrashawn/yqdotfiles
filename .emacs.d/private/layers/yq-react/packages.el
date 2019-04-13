(setq yq-react-packages '(rjsx-mode
                          prettier-js))


(defun yq-react/init-rjsx-mode ()
  (use-package rjsx-mode
    :defer t
    :init (progn
            (add-to-list 'auto-mode-alist '("components\/.*\.js\'" . rjsx-mode))
            (add-to-list 'auto-mode-alist '("containers\/.*\.js\'" . rjsx-mode)))
    :mode (("\\.jsx\\'" . rjsx-mode))
    :config
    (evil-define-key 'insert rjsx-mode-map (kbd "C-d") 'rjsx-delete-creates-full-tag)
    (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode "=" 'prettier-js)
    (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode "m" 'js2-mode)
    :commands (rjsx-mode)))


(defun yq-react/init-prettier-js ()
  (use-package prettier-js
    :commands (prettier-js-mode prettier-js)
    :defer t))