;;; version-control.el -*- lexical-binding: t; -*-

(setq! vc-handled-backends '(Git))
(add-hook! 'doom-first-file-hook #'magit-wip-mode #'magit-auto-revert-mode)

(el-patch-feature magit-apply)

(after! git-commit
  (setq! git-commit-major-mode 'markdown-mode
         git-commit-summary-max-length 75
         git-commit-fill-column 75))

(after! magit
  (setq!
    magit-openpgp-default-signing-key "B198FB15EC5C13012E940B37E394C5D9A8E535A6"
    magit-fetch-modules-jobs 10
    magit-diff-expansion-threshold 20
    magit-diff-refine-hunk t
    magit-revision-insert-related-refs 'mixed
    magit-revision-show-gravatars nil
    magit-revision-fill-summary-line 80
    magit-prefer-remote-upstream t
    magit-process-popup-time 60
    magit-process-log-max 10
    magit-refs-show-commit-count 'all
    ;; magit-status-goto-file-position t
    magit-log-show-refname-after-summary t
    magit-status-show-hashes-in-headers nil
    transient-default-level 7
    magit-log-margin '(t age-abbreviated magit-log-margin-width t 18)
    magit-section-initial-visibility-alist '((stashes . hide)
                                              (untracked . show)
                                              (staged . show))
    ;; magit-blame-echo-style 'margin
    magit-repository-directories '(("~/.emacs.d" . 0)
                                   ;; ("~/.emacs.d/straight/repos/" . 1)
                                   ;; ("~/workspace/third/" . 1)
                                   ("~/workspace/home/" . 1)
                                   ("~/workspace/office/" . 1))
    magit-branch-prefer-remote-upstream '("master" "main" "dev" "develop" "next")
    magit-branch-adjust-remote-upstream-alist '(("origin/devlope" . "\\/"))
    magit-published-branches '("origin/master" "origin/main" "origin/dev" "origin/develop")
    magit-clone-set-remote.pushDefault t)
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
(use-package! smeargle :commands (smeargle))
(use-package! magit-delta :hook (magit-mode . magit-delta-mode))

(after! forge
  (defadvice! +forge-topic-setup-buffer (topic)
    "Refetch topic on open"
    :after #'forge-topic-setup-buffer
    (forge-pull-topic topic))

  ;; https://github.com/magit/forge/issues/300
  (setq! forge-database-connector (if (> emacs-major-version 28) 'sqlite-builtin 'sqlite-module)
         forge-topic-list-limit '(20 . 5))

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
    (+marsam/add-pull-request-refs)))

(after! code-review
  (setq! code-review-lgtm-message "Thanks for your contribution. LGTM! :thumbsup:")
  ;; https://github.com/wandersoncferreira/code-review/pull/228/files
  (defun code-review-section--magit-diff-insert-file-section
    (file orig status modes rename header binary long-status)
  "Overwrite the original Magit function on `magit-diff.el' FILE.
ORIG, STATUS, MODES, RENAME, HEADER, BINARY and LONG-STATUS are arguments of the original fn."

  ;;; --- beg -- code-review specific code.
  ;;; I need to set a reference point for the first hunk header
  ;;; so the positioning of comments is done correctly.
  (let* ((raw-path-name (substring-no-properties file))
         (clean-path (if (string-prefix-p "b/" raw-path-name)
                         (replace-regexp-in-string "^b\\/" "" raw-path-name)
                       raw-path-name)))
    (code-review-db--curr-path-update clean-path))
    ;;; --- end -- code-review specific code.
  (insert ?\n)
  (magit-insert-section section
    (file file (or (equal status "deleted")
                   (derived-mode-p 'magit-status-mode)))
    (insert (propertize (format "%-10s %s" status
                                (if (or (not orig) (equal orig file))
                                    file
                                  (format "%s -> %s" orig file)))
                        'font-lock-face 'magit-diff-file-heading))
    (when long-status
      (insert (format " (%s)" long-status)))
    (magit-insert-heading)
    (unless (equal orig file)
      (oset section source orig))
    (oset section header header)
    (when modes
      (magit-insert-section (hunk '(chmod))
        (insert modes)
        (magit-insert-heading)))
    (when rename
      (magit-insert-section (hunk '(rename))
        (insert rename)
        (magit-insert-heading)))
    (when (string-match-p "Binary files.*" header)
      (magit-insert-section (code-review-binary-file-section file)
        (insert (propertize "Visit file"
                            'face 'code-review-request-review-face
                            'mouse-face 'highlight
                            'help-echo "Visit the file in Dired buffer"
                            'keymap 'code-review-binary-file-section-map))
        (magit-insert-heading)))
    (magit-wash-sequence #'magit-diff-wash-hunk))))
