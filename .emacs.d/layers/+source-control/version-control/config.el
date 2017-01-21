;;; config.el --- Version Control configuration File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar version-control-global-margin t
  "If non-nil, will show diff margins globally.")

(defvar version-control-diff-tool 'git-gutter+
  "Options are `git-gutter', `git-gutter+', and `diff-hl' to show
version-control markers.")

;; unchanged face
(defface git-gutter+-unchanged
  '((t (:background "yellow")))
  "face for unchanged lines"
  :group 'git-gutter+)

;; change face
(defface git-gutter+-modified
  '((t (:foreground "magenta" :weight bold)))
  "face for modified lines"
  :group 'git-gutter+)

;; added face
(defface git-gutter+-added
  '((t (:foreground "green" :weight bold)))
  "face for added lines"
  :group 'git-gutter+)

;; deleted face
(defface git-gutter+-deleted
  '((t (:foreground "red" :weight bold)))
  "face for deleted lines"
  :group 'git-gutter+)
