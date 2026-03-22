;;; completion/custom-capfs/config.el -*- lexical-binding: t; -*-

;; --- Caches for disk-based capfs ---

(defvar +claude-code--skill-cache nil
  "Cached list of skill names (without \"sk\" prefix).")

(defvar +claude-code--agent-cache nil
  "Cached list of agent names (without \"ag\" prefix).")

(defvar +claude-code--cache-refresh-interval 30
  "Seconds between cache refreshes for skill/agent completions.")

(defvar +claude-code--cache-timer nil
  "Timer for periodic cache refresh.")

(defun +claude-code--refresh-skill-cache ()
  "Refresh the skill name cache from disk."
  (let* ((skills-dirs (cl-remove-if-not
                       #'file-directory-p
                       (list (expand-file-name "~/.claude/skills")
                             (when-let ((root (ignore-errors (++workspace-current-project-root))))
                               (expand-file-name ".claude/skills" root)))))
         (dirs (cl-remove-duplicates
                (cl-loop for sd in skills-dirs
                         append
                         (cl-remove-if-not
                          (lambda (f)
                            (file-directory-p (expand-file-name f sd)))
                          (directory-files sd nil "^[^.]"))
                         append
                         (mapcar #'file-name-sans-extension
                                 (directory-files sd nil "\\.md$")))
                :test #'string=)))
    (setq +claude-code--skill-cache dirs)))

(defun +claude-code--refresh-agent-cache ()
  "Refresh the agent name cache from disk."
  (let* ((agents-dirs (cl-remove-if-not
                       #'file-directory-p
                       (list (expand-file-name "~/.claude/agents")
                             (when-let ((root (ignore-errors (++workspace-current-project-root))))
                               (expand-file-name ".claude/agents" root)))))
         (names (cl-remove-duplicates
                 (cl-loop for ad in agents-dirs
                          append (mapcar #'file-name-sans-extension
                                         (directory-files ad nil "\\.md$")))
                 :test #'string=)))
    (setq +claude-code--agent-cache names)))

(defun +claude-code--refresh-all-caches ()
  "Refresh both skill and agent caches."
  (+claude-code--refresh-skill-cache)
  (+claude-code--refresh-agent-cache))

(defun +claude-code--ensure-cache-timer ()
  "Start the periodic cache refresh timer if not already running."
  (unless (and +claude-code--cache-timer
               (memq +claude-code--cache-timer timer-list))
    (+claude-code--refresh-all-caches)
    (setq +claude-code--cache-timer
          (run-with-timer +claude-code--cache-refresh-interval
                          +claude-code--cache-refresh-interval
                          #'+claude-code--refresh-all-caches))))

(defun +doom-buffer-files-capf ()
  "Cape capf for completing file paths of all buffers in doom-buffer-list.
Filters candidates by fuzzy matching input against truncated path (no root, root-1 folders).
Wraps candidates with =...= if current buffer is org-mode."
  (let ((buffers (doom-buffer-list)))
    (when buffers
      (let* ((input (thing-at-point 'filename t))
             (files (cl-remove-if-not #'identity (mapcar #'buffer-file-name buffers)))
             (bounds (if input
                         (bounds-of-thing-at-point 'filename)
                       (cons (point) (point))))
             (beg (car bounds))
             (end (cdr bounds))
             (filtered
              (if input
                  (cl-remove-if-not
                   (lambda (s)
                     (when s
                       (let* ((parts (split-string (expand-file-name s) "/" t))
                              (len (length parts))
                              (relpath
                               (if (> len 2)
                                   (mapconcat #'identity
                                              (nthcdr 2 parts) "/")
                                 (car (last parts)))))
                         (or (s-contains? input s)
                             (< (hotfuzz--cost input relpath) 10000)))))
                   files)
                files)))
        (list beg end
              (cond ((derived-mode-p 'org-mode)
                     (mapcar (lambda (s) (concat "=" s "=")) filtered))
                    ((eq major-mode 'agent-shell-mode)
                     (mapcar (lambda (s) (concat "`" s "`")) filtered))
                    filtered)
              :exclusive 'no
              :annotation-function (lambda (s) " BufferFile")
              :company-kind (lambda (_) 'file)
              :category 'doom-buffer-file)))))

(defun +doom-buffer-capf ()
  "Cape capf for completing buffer names of all buffers in doom-buffer-list.
Filters candidates by fuzzy matching input against buffer name.
Wraps candidates with =...= if current buffer is org-mode."
  (let ((buffers (doom-buffer-list)))
    (when buffers
      (let* ((input (thing-at-point 'symbol t))
             (names (mapcar #'buffer-name buffers))
             (bounds (if input
                         (bounds-of-thing-at-point 'symbol)
                       (cons (point) (point))))
             (beg (car bounds))
             (end (cdr bounds))
             (filtered (if input
                           (cl-remove-if-not
                            (lambda (s)
                              (and s (or (s-contains? input s)
                                         (< (hotfuzz--cost input s) 10000))))
                            names)
                         names)))
        (list beg end
              (cond ((derived-mode-p 'org-mode)
                     (mapcar (lambda (s) (concat "=" s "=")) filtered))
                    ((eq major-mode 'agent-shell-mode)
                     (mapcar (lambda (s) (concat "`" s "`")) filtered))
                    (t
                     filtered))
              :exclusive 'no
              :annotation-function (lambda (s) " BufferName")
              :company-kind (lambda (_) 'buffer)
              :category 'doom-buffer-name)))))

(defun +claude-code-skill-completion ()
  "Cape capf for completing Claude Code skill names.
Triggers when the word at point starts with \"sk\".  Lists all
folder names in ~/.claude/skills/ as candidates.  The completion
result is =skill-name= skill.
Uses a periodically refreshed cache instead of hitting disk on each invocation."
  (let* ((word (thing-at-point 'symbol t))
         (bounds (when word (bounds-of-thing-at-point 'symbol))))
    (when (and word bounds (string-prefix-p "sk" word t))
      (let* ((beg (car bounds))
             (end (cdr bounds))
             (candidates (mapcar (lambda (d) (concat "sk" d))
                                 (or +claude-code--skill-cache '()))))
        (list beg end candidates
              :exclusive 'no
              :exit-function (lambda (str status)
                               (when (eq status 'finished)
                                 (let* ((name (substring str (length "sk")))
                                        (end (point))
                                        (start (- end (length str))))
                                   (delete-region start end)
                                   (goto-char start)
                                   (insert "use the " "=" name "= skill"))))
              :annotation-function (lambda (s)
                                     (concat " " (substring s (length "sk"))))
              :company-kind (lambda (_) 'text)
              :category 'claude-code-skill)))))

(defun +claude-code-agent-completion ()
  "Cape capf for completing Claude Code agent names.
Triggers when the word at point starts with \"ag\".  Lists all
markdown file names (without .md) in ~/.claude/agents/ as candidates.
The completion result is =agent-name= agent.
Uses a periodically refreshed cache instead of hitting disk on each invocation."
  (let* ((word (thing-at-point 'symbol t))
         (bounds (when word (bounds-of-thing-at-point 'symbol))))
    (when (and word bounds (string-prefix-p "ag" word t))
      (let* ((beg (car bounds))
             (end (cdr bounds))
             (candidates (mapcar (lambda (n) (concat "ag" n))
                                 (or +claude-code--agent-cache '()))))
        (list beg end candidates
              :exclusive 'no
              :exit-function (lambda (str status)
                               (when (eq status 'finished)
                                 (let* ((name (substring str (length "ag")))
                                        (end (point))
                                        (start (- end (length str))))
                                   (delete-region start end)
                                   (goto-char start)
                                   (insert "use the " "=" name "= agent"))))
              :annotation-function (lambda (s)
                                     (concat " " (substring s (length "ag"))))
              :company-kind (lambda (_) 'text)
              :category 'claude-code-agent)))))

(add-hook!
 '(gptel-mode-hook agent-shell-mode-hook eat-mode-hook)
 (defun +custom-capf-setup-org-mode ()
   (+claude-code--ensure-cache-timer)
   (add-hook 'completion-at-point-functions
             #'+claude-code-skill-completion 900 t)
   (add-hook 'completion-at-point-functions
             #'+claude-code-agent-completion 900 t)
   (add-hook 'completion-at-point-functions
             #'+doom-buffer-capf 1000 t)
   (add-hook 'completion-at-point-functions #'+doom-buffer-files-capf 1000 t)))
