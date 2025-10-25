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
   magit-diff-visit-prefer-worktree t
   magit-revision-insert-related-refs t
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
   magit-branch-prefer-remote-upstream '("master" "main" "mainnet" "dev" "develop" "sepolia" "next")
   magit-branch-adjust-remote-upstream-alist '(("origin/develop" . ("/" "feat" "fix" "ci" "chore"))
                                               ("origin/main" . ("/" "feat" "fix" "ci" "chore"))
                                               ("origin/master" . ("/" "feat" "fix" "ci" "chore"))
                                               ("origin/main" . ("/" "feat" "fix" "ci" "chore"))
                                               ("origin/mainnet" . ("/" "feat" "fix" "ci" "chore"))
                                               ("origin/sepolia" . ("/" "feat" "fix" "ci" "chore")))
   magit-published-branches '("origin/master" "origin/main" "origin/dev" "origin/develop" "origin/mainnet" "origin/sepolia")
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

  (el-patch-defun magit-stage-files (files &optional force)
    "Read one or more files and stage all changes in those files.
With prefix argument FORCE, offer ignored files for completion."
    (interactive
     (let* ((choices (if current-prefix-arg
                         (magit-ignored-files)
                       (nconc (magit-unstaged-files)
                              (magit-untracked-files))))
            (default (or (magit-section-value-if 'file)
                         (magit-file-relative-name)))
            (default (car (member default choices))))
       (el-patch-swap
         (list (magit-completing-read-multiple
                (if current-prefix-arg "Stage ignored file,s: " "Stage file,s: ")
                choices nil t nil nil default)
               current-prefix-arg)
         (if current-prefix-arg
             (list (magit-completing-read-multiple
                    "Stage ignored file,s: "
                    choices nil t nil nil default)
                   current-prefix-arg)
           (progn (set-transient-map yq-s-map) (list nil nil))))))
    (el-patch-swap
      (magit-with-toplevel
        (magit-stage-1 (and force "--force") files))
      (if (and (eq (length files) 1) (eq (car files) nil))
          nil
        (magit-with-toplevel
          (magit-stage-1 (and force "--force") files))))))

(use-package! abridge-diff :hook (magit-status-mode . abridge-diff-mode))
(use-package! magit-cz :after magit)
(after! with-editor (add-hook! 'with-editor-mode-hook #'evil-insert-state))
(use-package! smeargle :commands (smeargle))
(use-package! magit-delta :hook (magit-mode . magit-delta-mode))

(after! forge
  (setq! forge-pull-notifications t)

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

  (defadvice! +forge-browse (orig-fn)
    :around #'forge-browse
    (let ((browse-url-browser-function #'browse-url-default-browser))
      (funcall orig-fn))))

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

(setq +forge-current-topic-store (expand-file-name "~/Dropbox/sync/doom/store/forge-current-topic.el"))

(defun +forge-current-topic-get ()
  (doom-store-get
   (project-root (project-current t))
   +forge-current-topic-store))

(defun +forge-current-topic-set (topic-id)
  (doom-store-put
   (project-root (project-current t))
   topic-id
   nil
   +forge-current-topic-store))

(after! magit
  (require 'forge))

(after! forge
  (magit-add-section-hook
   'magit-status-sections-hook
   #'+forge-insert-current-topic #'forge-insert-pullreqs)

  (defun +forge-insert-current-topic ()
    (when (forge-get-repository :tracked?)
      (magit-insert-section (current-topic)
        (when-let (topic-id (+forge-current-topic-get))
          (let* ((topic-id (+forge-current-topic-get))
                 (issue-topic (forge-get-issue topic-id))
                 (pr-topic (forge-get-pullreq topic-id))
                 (discussion-topic (forge-get-discussion topic-id))
                 (topic-type (cond (issue-topic 'issue)
                                   (pr-topic 'pullreq)
                                   (discussion-topic 'discussion)))
                 (topic (or issue-topic
                            pr-topic
                            discussion-topic)))
            (when topic
              (forge--insert-topics topic-type "Current Topic" (list topic))))))))

  (defun +forge-current-topic-refresh-buffer ()
    (+forge-insert-current-topic))

  (defun +forge-set-current-topic (topic)
    (interactive (list (forge-read-topic "Set current topic")))
    (with-current-buffer (current-buffer)
      (+forge-current-topic-set topic)
      (magit-refresh)))

  (defun +forge-clear-current-topic ()
    (interactive)
    (doom-store-rem (project-root (project-current t)) +forge-current-topic-store))

  (defun +forge-log-current-topic (&optional show-topic-buffer)
    (interactive "P")
    (if-let ((topic-id (+forge-current-topic-get)))
        (progn
          (forge-visit-topic (forge-get-topic topic-id))
          (let ((topic-buf (current-buffer)))
            (unless show-topic-buffer (bury-buffer))
            (with-current-buffer topic-buf
              (forge-create-post)
              (let ((post-buf (current-buffer)))
                (bury-buffer post-buf)
                (unless show-topic-buffer
                  (+popup-buffer post-buf '((select . t)
                                            (side . bottom))))))))
      (message "No current topic for this project"))))

(defun +write-magit-wip-diffs-on-save ()
  "If magit-wip-mode is on, generate wip diff files for 1, 3, 5, and 10 minutes in the repo."
  (require 'magit)
  (when (bound-and-true-p magit-wip-mode)
    (let ((repo-root (magit-toplevel)))
      ;; Generate diffs for 1, 3, 5, and 10 minutes
      (dolist (minutes '(1 3 5 10))
        (let* ((diff-buf (+magit-wip-diff-n-min-buffer minutes))
               (out-file (expand-file-name (format ".magit-wip-%dm.diff" minutes) repo-root)))
          (with-current-buffer diff-buf
            (write-region (point-min) (point-max) out-file nil 'silent)))))))

(after! magit
  (defadvice! +magit-wip-commit-buffer-file (&optional msg)
    :after #'magit-wip-commit-buffer-file
    (+write-magit-wip-diffs-on-save)))
