;;; packages.el --- Source Control Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq version-control-packages
      '(
        diff-mode
        git-gutter+
        ))

(defun version-control/init-diff-mode ()
  (use-package diff-mode
    :defer t
    :config
    (evilified-state-evilify diff-mode diff-mode-map
      "j" 'diff-hunk-next
      "k" 'diff-hunk-prev)))

(defun version-control/init-diff-hl ()
  (use-package diff-hl
    :init
    (progn
      (setq diff-hl-side 'left)
      (when (eq version-control-diff-tool 'diff-hl)
        (when (configuration-layer/package-usedp 'magit)
          (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
        (when version-control-global-margin
          (global-diff-hl-mode))
        (diff-hl-margin-mode)
        (spacemacs|do-after-display-system-init
         (setq diff-hl-side 'right)
         (diff-hl-margin-mode -1))))))

(defun version-control/init-git-gutter+ ()
  (use-package git-gutter+
    :commands (global-git-gutter+-mode git-gutter+-mode)
    :init
    (progn
      ;; If you enable global minor mode
      (when (and (eq version-control-diff-tool 'git-gutter+)
                 version-control-global-margin)
        (add-hook 'magit-pre-refresh-hook 'git-gutter+-refresh)
        (global-git-gutter+-mode t))
      (setq
       git-gutter+-modified-sign " "
       git-gutter+-added-sign "+"
       git-gutter+-deleted-sign "-"
       git-gutter+-diff-option "-w"
       git-gutter+-hide-gutter t))
    ;; identify magit changes
    :config
    (spacemacs|hide-lighter git-gutter+-mode)
    ;; (set-face-foreground 'git-gutter+-modified "black")
    ;; (set-face-foreground 'git-gutter+-added    "black")
    ;; (set-face-foreground 'git-gutter+-deleted  "black")
    ;; (set-face-background 'git-gutter+-modified "orange1")
    ;; (set-face-background 'git-gutter+-added    "green4")
    ;; (set-face-background 'git-gutter+-deleted  "red3")
    ))
