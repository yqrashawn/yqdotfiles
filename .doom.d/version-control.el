;;; version-control.el -*- lexical-binding: t; -*-

(setq! vc-handled-backends '(Git))
(add-hook! 'doom-first-file-hook #'magit-wip-mode #'magit-auto-revert-mode)

(el-patch-feature magit-apply)
(after! magit
  (setq! magit-fetch-modules-jobs 10
         transient-default-level 7
         magit-log-margin '(t age-abbreviated magit-log-margin-width t 18)
         magit-section-initial-visibility-alist '((stashes . hide)
                                                  (untracked . show)
                                                  (staged . show))
         ;; magit-blame-echo-style 'margin
         magit-repository-directories '(("~/.emacs.d" . 0)
                                        ;; ("~/.emacs.d/straight/repos/" . 1)
                                        ("~/workspace/home/" . 1)
                                        ("~/workspace/office/" . 1)
                                        ;; ("~/workspace/third/" . 1)
                                        ))
  (pushnew! magit-no-confirm 'stage-all-changes)
  (add-hook! 'magit-process-mode-hook #'++doom-apply-ansi-color-to-compilation-buffer-h)
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
  (after! ivy
    (setf (alist-get 'my-magit-command ivy-re-builders-alist) #'ivy--regex-fuzzy)
    (pushnew! ivy-re-builders-alist '(magit-log-other . ivy--regex-fuzzy)))
  (add-function :before magit-completing-read-function #'my-magit-command)

  ;; https://emacs-pe.github.io/2015/06/30/magit-github-pr/
  (defun marsam/add-pull-request-refs (&optional remote local-ns)
    "Set pull requests refs from a REMOTE with LOCAL-NS namespace into Git config."
    (interactive (let* ((remote (magit-read-remote "Fetch remote"))
                        (local-ns (read-string "local namespace: " (format "%s/pr" remote))))
                   (list remote local-ns)))
    (and (not (magit-get-boolean "core" "disableprref"))
         (let* ((remote (or remote "origin"))
                (local-ns (if (or (null local-ns) (string= "" local-ns)) (format "%s/pr" remote) local-ns))
                (pr-refs (format "+refs/pull/*/head:refs/remotes/%s/*" local-ns))
                (remote-fetch-refs (magit-get-all "remote" remote "fetch")))
           (and remote-fetch-refs
                (not (magit-get-boolean "remote" remote "disableprref"))
                (not (member pr-refs remote-fetch-refs))
                (string-match "github.com" (magit-get "remote" remote "url"))
                (magit-git-string "config" "--add" (format "remote.%s.fetch" remote) pr-refs)))))

  (defun +marsam/add-pull-request-refs ()
    (dolist (remote (magit-list-remotes))
      (marsam/add-pull-request-refs remote)))
  (defadvice! +forge-pull (&optional repo until) :after #'forge-pull
    (+marsam/add-pull-request-refs))
  ;; (add-hook 'magit-mode-hook '+marsam/add-pull-request-refs)

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
(use-package! magit-cz :after magit)
(after! with-editor (add-hook! 'with-editor-mode-hook #'evil-insert-state))
(use-package smeargle :commands (smeargle))