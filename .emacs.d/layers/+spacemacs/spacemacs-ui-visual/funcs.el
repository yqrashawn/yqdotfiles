;;; funcs.el --- Spacemacs Visual UI Layer functions File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; golden ratio

(defun spacemacs/no-golden-ratio-for-buffers (bufname)
  "Disable golden-ratio if BUFNAME is the name of a visible buffer."
  (and (get-buffer bufname) (get-buffer-window bufname 'visible)))

(defun spacemacs/no-golden-ratio-guide-key ()
  "Disable golden-ratio for guide-key popwin buffer."
  (or (spacemacs/no-golden-ratio-for-buffers " *guide-key*")
      (spacemacs/no-golden-ratio-for-buffers " *popwin-dummy*")))


;; popwin

(defun spacemacs/remove-popwin-display-config (str)
  "Removes the popwin display configurations that matches the passed STR"
  (setq popwin:special-display-config
        (-remove (lambda (x) (if (and (listp x) (stringp (car x)))
                                 (string-match str (car x))))
                 popwin:special-display-config)))


;; spaceline

(defun spacemacs/customize-powerline-faces ()
  "Alter powerline face to make them work with more themes."
  (when (boundp 'powerline-inactive2)
    (set-face-attribute 'powerline-inactive2 nil
                        :inherit 'font-lock-comment-face)))

(defun spacemacs//evil-state-face ()
  (let ((state (if (eq 'operator evil-state) evil-previous-state evil-state)))
    (intern (format "spacemacs-%S-face" state))))

(defun spacemacs//restore-powerline (buffer)
  "Restore the powerline in buffer"
  (with-current-buffer buffer
    (setq-local mode-line-format (default-value 'mode-line-format))
    (powerline-set-selected-window)
    (powerline-reset)))

(defun spacemacs//set-powerline-for-startup-buffers ()
  "Set the powerline for buffers created when Emacs starts."
  (dolist (buffer '("*Messages*" "*spacemacs*" "*Compile-Log*"))
    (when (and (get-buffer buffer)
               (configuration-layer/package-usedp 'spaceline))
      (spacemacs//restore-powerline buffer))))

(defun spacemacs//prepare-diminish ()
  (when spaceline-minor-modes-p
    (let ((unicodep (dotspacemacs|symbol-value
                     dotspacemacs-mode-line-unicode-symbols)))
      (setq spaceline-minor-modes-separator
            (if unicodep (if (display-graphic-p) "" " ") "|"))
      (dolist (mm spacemacs--diminished-minor-modes)
        (let ((mode (car mm)))
          (when (and (boundp mode) (symbol-value mode))
            (let* ((unicode (cadr mm))
                   (ascii (caddr mm))
                   (dim (if unicodep
                            unicode
                          (if ascii ascii unicode))))
              (diminish mode dim))))))))


;; ansi-colors

(defun spacemacs-ui-visual//compilation-buffer-apply-ansi-colors ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point-max))))
