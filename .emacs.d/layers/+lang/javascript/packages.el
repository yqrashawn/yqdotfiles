;;; packages.el --- Javascript Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq javascript-packages
  '(
    company
    (company-tern :toggle (configuration-layer/package-usedp 'company))
    ;; evil-matchit
    flycheck
    ggtags
    js-doc
    js2-mode
    json-mode
    (tern :toggle (spacemacs//tern-detect))
    ;; skewer-mode
    ))

(defun javascript/post-init-company ()
  (spacemacs|add-company-hook js2-mode))

(defun javascript/init-company-tern ()
  (use-package company-tern
    :if (and (configuration-layer/package-usedp 'company)
             (configuration-layer/package-usedp 'tern))
    :defer t
    :init
    (push 'company-tern company-backends-js2-mode)))

(defun javascript/post-init-flycheck ()
  (dolist (mode '(js2-mode json-mode))
    (spacemacs/add-flycheck-hook mode)))

(defun javascript/post-init-ggtags ()
  (add-hook 'js2-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun javascript/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'js2-mode))

(defun javascript/init-js-doc ()
  (use-package js-doc
    :defer t
    :init
    (progn
      (defun spacemacs/js-doc-require ()
        "Lazy load js-doc"
        (require 'js-doc))
      (add-hook 'js2-mode-hook 'spacemacs/js-doc-require)

      (defun spacemacs/js-doc-set-key-bindings (mode)
        "Setup the key bindings for `js2-doc' for the given MODE."
        (spacemacs/declare-prefix-for-mode mode "mrd" "documentation")
        (spacemacs/set-leader-keys-for-major-mode mode "rdb" 'js-doc-insert-file-doc)
        (spacemacs/set-leader-keys-for-major-mode mode "rdf" 'js-doc-insert-function-doc)
        (spacemacs/set-leader-keys-for-major-mode mode "rdt" 'js-doc-insert-tag)
        (spacemacs/set-leader-keys-for-major-mode mode "rdh" 'js-doc-describe-tag))
      (spacemacs/js-doc-set-key-bindings 'js2-mode))))

(defun javascript/init-js2-mode ()
  (use-package js2-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
      ;; Required to make imenu functions work correctly
      (add-hook 'js2-mode-hook 'js2-imenu-extras-mode))
    :config
    (progn
      (spacemacs/declare-prefix-for-mode 'js2-mode "mh" "documentation")
      (spacemacs/declare-prefix-for-mode 'js2-mode "mg" "goto")
      (spacemacs/declare-prefix-for-mode 'js2-mode "mr" "refactor")
      (spacemacs/declare-prefix-for-mode 'js2-mode "mz" "folding")
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode
        "w" 'js2-mode-toggle-warnings-and-errors
        "zc" 'js2-mode-hide-element
        "zo" 'js2-mode-show-element
        "zr" 'js2-mode-show-all
        "ze" 'js2-mode-toggle-element
        "zF" 'js2-mode-toggle-hide-functions
        "zC" 'js2-mode-toggle-hide-comments))))

(defun javascript/post-init-evil-matchit ()
  (add-hook `js2-mode `turn-on-evil-matchit-mode))

(defun javascript/init-json-mode ()
  (use-package json-mode
    :defer t))

(defun javascript/init-tern ()
  (use-package tern
    :defer t
    :diminish tern-mode
    :init (add-hook 'js2-mode-hook 'tern-mode)
    :config
    (progn
      (when javascript-disable-tern-port-files
        (add-to-list 'tern-command "--no-port-file" 'append))
      (spacemacs//set-tern-key-bindings 'js2-mode))))

(defun javascript/init-skewer-mode ()
  (use-package skewer-mode
    :defer t
    :diminish skewer-mode
    :init
    (progn
      (spacemacs/register-repl 'skewer-mode 'spacemacs/skewer-start-repl "skewer")
      (add-hook 'js2-mode-hook 'skewer-mode))
    :config
    (progn
      (defun spacemacs/skewer-start-repl ()
        "Attach a browser to Emacs and start a skewer REPL."
        (interactive)
        (run-skewer)
        (skewer-repl))

      (defun spacemacs/skewer-load-buffer-and-focus ()
        "Execute whole buffer in browser and switch to REPL in insert state."
        (interactive)
        (skewer-load-buffer)
        (skewer-repl)
        (evil-insert-state))

      (defun spacemacs/skewer-eval-defun-and-focus ()
       "Execute function at point in browser and switch to REPL in insert state."
       (interactive)
       (skewer-eval-defun)
       (skewer-repl)
       (evil-insert-state))

      (defun spacemacs/skewer-eval-region (beg end)
        "Execute the region as JavaScript code in the attached browser."
        (interactive "r")
        (skewer-eval (buffer-substring beg end) #'skewer-post-minibuffer))

      (defun spacemacs/skewer-eval-region-and-focus (beg end)
        "Execute the region in browser and swith to REPL in insert state."
        (interactive "r")
        (spacemacs/skewer-eval-region beg end)
        (skewer-repl)
        (evil-insert-state))

      (spacemacs/declare-prefix-for-mode 'js2-mode "ms" "skewer")
      (spacemacs/declare-prefix-for-mode 'js2-mode "me" "eval")
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode
        "'" 'spacemacs/skewer-start-repl
        "ee" 'skewer-eval-last-expression
        "eE" 'skewer-eval-print-last-expression
        "sb" 'skewer-load-buffer
        "sB" 'spacemacs/skewer-load-buffer-and-focus
        "si" 'spacemacs/skewer-start-repl
        "sf" 'skewer-eval-defun
        "sF" 'spacemacs/skewer-eval-defun-and-focus
        "sr" 'spacemacs/skewer-eval-region
        "sR" 'spacemacs/skewer-eval-region-and-focus
        "ss" 'skewer-repl))))
