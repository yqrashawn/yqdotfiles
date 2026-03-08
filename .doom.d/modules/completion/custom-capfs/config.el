;;; completion/custom-capfs/config.el -*- lexical-binding: t; -*-

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
Triggers when the word at point starts with \"skill\".  Lists all
folder names in ~/.claude/skills/ as candidates.  The completion
result is =skill-name= skill."
  (let* ((word (thing-at-point 'symbol t))
         (bounds (when word (bounds-of-thing-at-point 'symbol))))
    (when (and word bounds (string-prefix-p "skill" word t))
      (let* ((beg (car bounds))
             (end (cdr bounds))
             (skills-dir (expand-file-name "~/.claude/skills"))
             (entries (when (file-directory-p skills-dir)
                        (cl-remove-if
                         (lambda (f) (member f '("." "..")))
                         (directory-files skills-dir nil "^[^.]"))))
             (dirs (cl-remove-if-not
                    (lambda (f)
                      (file-directory-p (expand-file-name f skills-dir)))
                    entries))
             ;; Prefix candidates with "skill" so they match the typed text
             (candidates (mapcar (lambda (d) (concat "skill" d)) dirs)))
        (list beg end candidates
              :exclusive 'no
              :exit-function (lambda (str status)
                               (when (eq status 'finished)
                                 ;; str is "skill<name>", replace with "=<name>= skill"
                                 (let* ((name (substring str (length "skill")))
                                        (end (point))
                                        (start (- end (length str))))
                                   (delete-region start end)
                                   (goto-char start)
                                   (insert "=" name "= skill"))))
              :annotation-function (lambda (s)
                                     (concat " " (substring s (length "skill"))))
              :company-kind (lambda (_) 'text)
              :category 'claude-code-skill)))))

(defun +claude-code-agent-completion ()
  "Cape capf for completing Claude Code agent names.
Triggers when the word at point starts with \"agent\".  Lists all
markdown file names (without .md) in ~/.claude/agents/ as candidates.
The completion result is =agent-name= agent."
  (let* ((word (thing-at-point 'symbol t))
         (bounds (when word (bounds-of-thing-at-point 'symbol))))
    (when (and word bounds (string-prefix-p "agent" word t))
      (let* ((beg (car bounds))
             (end (cdr bounds))
             (agents-dir (expand-file-name "~/.claude/agents"))
             (files (when (file-directory-p agents-dir)
                      (directory-files agents-dir nil "\\.md$")))
             (names (mapcar (lambda (f) (file-name-sans-extension f)) files))
             ;; Prefix candidates with "agent" so they match the typed text
             (candidates (mapcar (lambda (n) (concat "agent" n)) names)))
        (list beg end candidates
              :exclusive 'no
              :exit-function (lambda (str status)
                               (when (eq status 'finished)
                                 ;; str is "agent<name>", replace with "=<name>= agent"
                                 (let* ((name (substring str (length "agent")))
                                        (end (point))
                                        (start (- end (length str))))
                                   (delete-region start end)
                                   (goto-char start)
                                   (insert "=" name "= agent"))))
              :annotation-function (lambda (s)
                                     (concat " " (substring s (length "agent"))))
              :company-kind (lambda (_) 'text)
              :category 'claude-code-agent)))))

(add-hook!
 '(gptel-mode-hook agent-shell-mode-hook eat-mode-hook)
 (defun +custom-capf-setup-org-mode ()
   (add-hook 'completion-at-point-functions
             #'+claude-code-skill-completion 900 t)
   (add-hook 'completion-at-point-functions
             #'+claude-code-agent-completion 900 t)
   (add-hook 'completion-at-point-functions
             #'+doom-buffer-capf 1000 t)
   (add-hook 'completion-at-point-functions #'+doom-buffer-files-capf 1000 t)))
