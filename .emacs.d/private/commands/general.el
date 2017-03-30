;; -*- mode: emacs-lisp -*-
(defun space-after-comma ()
  "Insert space before each comma of current line."
  (interactive)
  (evil-ex-execute ":'<,'>s/, /"))
