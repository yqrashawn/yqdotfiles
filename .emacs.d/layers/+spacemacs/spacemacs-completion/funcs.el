;;; funcs.el --- Spacemacs Completion Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Ivy

(defun spacemacs//ivy-hjkl-navigation (style)
  "Set navigation on 'hjkl' for the given editing STYLE."
  (cond
   ((or (eq 'vim style)
        (and (eq 'hybrid style)
             hybrid-mode-enable-hjkl-bindings))
    (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
    (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
    (define-key ivy-minibuffer-map (kbd "C-h") (kbd "DEL"))
    ;; Move C-h to C-S-h
    (define-key ivy-minibuffer-map (kbd "C-S-h") help-map)
    (define-key ivy-minibuffer-map (kbd "C-l") 'ivy-alt-done)
    (define-key ivy-minibuffer-map (kbd "<escape>")
      'minibuffer-keyboard-quit))
   (t
    (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-alt-done)
    (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-kill-line)
    (define-key ivy-minibuffer-map (kbd "C-h") nil)
    (define-key ivy-minibuffer-map (kbd "C-l") nil))))
