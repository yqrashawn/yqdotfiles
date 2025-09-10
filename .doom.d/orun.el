;;; .nixpkgs/.doom.d/orun.el -*- lexical-binding: t; -*-

(require 'a)
(require 'seq)

(defun o/run-valid-cmd? (cmd)
  (and (hash-table-p cmd)
       (a-has-key? cmd :name)
       (a-has-key? cmd :cmd)))

(defun o/run--get-command-list ()
  (when-let ((root (projectile-project-root)))
    (when (f-exists? (concat (projectile-project-root) ".orun"))
      (let ((l))
        (setq l (with-temp-buffer
                  (insert-file-contents (concat (projectile-project-root) ".orun"))
                  (read (buffer-string))))
        (setq l (seq-map
                 (lambda (x)
                   (cond ((stringp x) (a-hash-table :name x :cmd x))
                         ((and (listp x) (o/keyword? (car x)))
                          (let ((ht (apply #'a-hash-table x)))
                            (if (and (a-has-key? ht :name) (not (a-has-key? ht :cmd)))
                                (a-assoc-1 ht :cmd (a-get ht :name))
                              ht)))
                         (t nil)))
                 l))
        (setq l (seq-filter 'o/run-valid-cmd? l))))))

(defun o/run-get-command-name (cmd)
  (when (o/run-valid-cmd? cmd)
    (a-get cmd :name)))

(defun o/run-get-command-cmd (cmd)
  (when (o/run-valid-cmd? cmd)
    (a-get cmd :cmd)))

(defun o/run-command-quiet? (cmd)
  (when (o/run-valid-cmd? cmd)
    (a-get cmd :quiet?)))

(defun o/run-get-command-by-name (n)
  (seq-find
   (lambda (cmd) (string= n (o/run-get-command-name cmd)))
   (o/run--get-command-list)))

(defun o/run-git-exclude? ()
  (seq-find
   (lambda (cmd) (and (hash-table-p cmd) (a-get cmd :git-exclude?)))
   (o/run--get-command-list)))

(defun o/run-git-exclude ()
  (interactive)
  (when (and (f-exists? (concat (projectile-project-root) ".git")) (o/run-git-exclude?))
    (let ((git-exclude-file (concat (projectile-project-root) ".git/info/exclude")))
      (o/append-to-file-unless
       git-exclude-file
       "/.orun"
       (lambda () (search-forward "/.orun" nil t))
       t))))

(defvar o/run-history nil)

;;;###autoload
(defun o/run (cmds)
  (interactive
   (list (completing-read-multiple "Commands: " (seq-map 'o/run-get-command-name (o/run--get-command-list)) nil nil nil 'o/run-history)))
  (seq-doseq (cmd cmds)
    (let* ((cmd (o/run-get-command-by-name cmd))
           (detached-session-mode (if (o/run-command-quiet? cmd) 'detached 'attached)))
      ;; (detached-create-session (o/run-get-command-cmd cmd))
      (detached-shell-command (o/run-get-command-cmd cmd)))))
