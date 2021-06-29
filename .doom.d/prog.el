;;; prog.el -*- lexical-binding: t; -*-

(setq +company-backend-alist
      '((text-mode (:separate company-dabbrev company-yasnippet company-files company-ispell))
        (prog-mode company-capf company-yasnippet company-files company-keywords company-dabbrev-code company-dabbrev)
        (conf-mode company-capf company-dabbrev-code company-yasnippet)))

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
(add-hook! 'doom-first-file-hook #'magit-wip-mode #'magit-auto-revert-mode)

(el-patch-feature magit-apply)
(after! magit
  (setq! magit-fetch-modules-jobs 10
         magit-repository-directories '(("~/.emacs.d" . 0)
                                        ;; ("~/.emacs.d/straight/repos/" . 1)
                                        ("~/workspace/home/" . 1)
                                        ("~/workspace/office/" . 1)
                                        ("~/workspace/third/" . 1)))
  (pushnew! magit-no-confirm 'stage-all-changes)
  (transient-define-argument magit-merge:--strategy-option ()
    :description "Strategy Option"
    :class 'transient-option
    ;; key for merge and rebase: "-s"
    ;; key for cherry-pick and revert: "=s"
    ;; shortarg for merge and rebase: "-s"
    ;; shortarg for cherry-pick and revert: none
    :key "-X"
    :argument "--strategy-option="
    :choices '("ours"
               "theirs"
               "patience"
               "subtree"
               "renormalize"
               "no-renormalize"
               "no-renames"
               "diff-algorithim=patience"
               "diff-algorithim=minimal"
               "diff-algorithim=histogram"
               "diff-algorithim=myers"))
  (transient-append-suffix 'magit-rebase "-s" '("-X" magit-merge:--strategy-option))
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-overview
                          'magit-insert-unpulled-from-upstream)
  (defun my-magit-command (&rest _)
    (interactive)
    (setq this-command #'my-magit-command))
  (with-eval-after-load 'ivy
    (setf (alist-get 'my-magit-command ivy-re-builders-alist) #'ivy--regex-fuzzy))
  (add-function :before magit-completing-read-function #'my-magit-command)
  (pushnew! ivy-re-builders-alist '(magit-log-other . ivy--regex-fuzzy))

  ;; (after! eldoc
  ;;   (add-hook! 'magit-status-mode-hook #'th/magit-eldoc-setup)
  ;;   (add-hook! 'magit-log-mode-hook #'th/magit-eldoc-setup)
  ;;   (eldoc-add-command 'magit-next-line)
  ;;   (eldoc-add-command 'magit-previous-line))

  (el-patch-defun magit-stage-file (file)
    "Stage all changes to FILE.
With a prefix argument or when there is no file at point ask for
the file to be staged.  Otherwise stage the file at point without
requiring confirmation."
    (interactive
     (let* ((atpoint (magit-section-value-if 'file))
            (current (magit-file-relative-name))
            (choices (nconc (magit-unstaged-files)
                            (magit-untracked-files)))
            (default (car (member (or atpoint current) choices))))
       (list (if (or current-prefix-arg (not default))
                 (el-patch-swap
                   (magit-completing-read "Stage file" choices
                                          nil t nil nil default)
                   (progn (set-transient-map yq-s-map) nil))
               default))))
    (el-patch-swap
      (magit-with-toplevel
        (magit-stage-1 nil (list file)))
      (if (and (eq (length (list file)) 1) (eq (car (list file)) nil))
          nil
        (magit-with-toplevel
          (magit-stage-1 nil (list file)))))))

(use-package! abridge-diff :hook (magit-status-mode . abridge-diff-mode))