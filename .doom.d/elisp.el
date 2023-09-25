;;; lang/elisp.el -*- lexical-binding: t; -*-

(after! elisp-mode
  (setq! enable-local-variables t)
  (require 'pprint-to-buffer))

(after! dash
  (setq! dash-enable-fontlock t))

(use-package! eval-sexp-fu :hook (emacs-lisp-mode . eval-sexp-fu-flash-mode))
(use-package! lisp-extra-font-lock :hook (emacs-lisp-mode . lisp-extra-font-lock-mode))
(use-package! highlight-function-calls :hook (emacs-lisp-mode . highlight-function-calls-mode))
(use-package! easy-escape :hook (emacs-lisp-mode . easy-escape-minor-mode)) ; elisp regexp

(add-hook! 'emacs-lisp-mode-hook (require 'cider))

;; (use-package! elisp-slime-nav
;;   :hook ((emacs-lisp-mode ielm-mode) . turn-on-elisp-slime-nav-mode))

;;; from corgi-emacs-lisp
;; Show emacs-lisp eval results in an overlay, CIDER style.
;; https://endlessparentheses.com/eval-result-overlays-in-emacs-lisp.html
;; We rely on CIDER to do the heavy lifting, can't seem to find a general library
;; for doing this style of overlays.
(defun corgi/eval-overlay (value point)
  (cider--make-result-overlay (format "%S" value)
    :where point
    :duration 'command)
  ;; Preserve the return value.
  value)

(after! elisp-mode
  (advice-add 'eval-region :around
              (lambda (f beg end &rest r)
                (corgi/eval-overlay
                 (apply f beg end r)
                 end)))

  (advice-add 'eval-last-sexp :filter-return
              (lambda (r)
                (corgi/eval-overlay r (point))))

  (advice-add 'eval-defun :filter-return
              (lambda (r)
                (corgi/eval-overlay
                 r
                 (save-excursion
                   (end-of-defun)
                   (point))))))
