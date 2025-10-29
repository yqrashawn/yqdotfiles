;;; helper.el -*- lexical-binding: t; -*-


(require 's)
(require 'a)
(require 'seq)
(require 'f)
(load! "clj-elisp.el")

(defalias 'prn #'print)

(setq! +lispy-modes '(cider-repl-mode
                      clojure-mode
                      clojurec-mode
                      clojurescript-mode
                      common-lisp-mode
                      emacs-lisp-mode
                      eshell-mode
                      fennel-mode
                      fennel-repl-mode
                      geiser-repl-mode
                      gerbil-mode
                      inf-clojure-mode
                      inferior-emacs-lisp-mode
                      inferior-lisp-mode
                      inferior-scheme-mode
                      lisp-interaction-mode
                      lisp-mode
                      monroe-mode
                      racket-mode
                      racket-repl-mode
                      scheme-interaction-mode
                      scheme-mode
                      slime-repl-mode
                      sly-mrepl-mode
                      stumpwm-mode
                      ielm-mode
                      hy-mode
                      fennel-mode
                      dune-mode
                      lfe-mode))

(setq! +clojure-modes '(clojure-mode
                        clojurec-mode
                        clojurescript-mode))

(setq! +clojure-modes-hooks '(clojure-mode-hook
                              clojurec-mode-hook
                              clojurescript-mode-hook))

(defun +xcrun-devices ()
  (apply #'vconcat (a-vals (a-get (json-parse-string (shell-command-to-string "xcrun simctl list devices -j")) "devices"))))

(defun +xcrun-device (device-name)
  (seq-find (lambda (v) (string= device-name (a-get v "name"))) (+xcrun-devices)))

(defun o/keyword? (x)
  (and
   (eq (type-of x) 'symbol)
   (s-starts-with? ":" (symbol-name x))))

(defun o/append-to-file-unless (filename content pred &optional newline prepend)
  (when (f-exists? filename)
    (with-temp-file filename
      (insert-file-contents filename)
      (goto-char (point-min))
      (with-current-buffer (current-buffer)
        (unless (funcall pred)
          (unless prepend (goto-char (point-max)))
          (when newline (unless (bolp) (newline)))
          (insert content))))))

(defun +magit-wip-diff-n-min-buffer (n)
  (let ((buf-name (format! " *changes-with-%d-minutes*" n)))
    ;; (when (get-buffer buf-name) (kill-buffer buf-name))
    (let ((b (get-buffer-create buf-name t)))
      (with-current-buffer b
        (erase-buffer)
        (insert!
         ("%s"
          (concat
           (format "# These are diffs of changes I made within %d minutes\n\n" n)
           (shell-command-to-string
            (format!
             "git reflog --since=\"%d minutes ago\" --oneline -p refs/wip/wtree/refs/heads/%s"
             n
             (magit-get-current-branch)))))))
      b)))

(comment
  (insert!
   "\n"
   ("%s" (with-current-buffer (+magit-wip-diff-n-min-buffer 5)
           (buffer-string)))))

(defun +project-files-buffers (file-list)
  "Return a list of buffers for FILE-LIST, where FILE-LIST is a list of relative file paths.
Each file is opened (if not already) with `find-file-noselect` relative to
 the current project root."
  (let ((project-root (doom-project-root))
        buffers)
    (setq buffers (mapcar (lambda (file)
                            (find-file-noselect (expand-file-name file project-root)))
                          file-list))
    buffers))

(defun +magit-wip-buffer-changed-within-n-min (n)
  (when (magit-git-repo-p (doom-project-root))
    (thread-last
      (shell-command-to-string

       (format!
        "git reflog --since=\"%d minutes ago\" --name-only --pretty=format: refs/wip/wtree/refs/heads/%s | grep -v '^$' | sort -u"
        n
        (magit-get-current-branch)))
      s-lines
      (seq-filter (lambda (s) (not (string-empty-p s))))
      +project-files-buffers)))

(defun +visible-buffers ()
  (seq-filter
   (lambda (b)
     (with-current-buffer b (not gptel-mode)))
   (mapcar 'window-buffer (window-list))))

(defun +visible-buffers-list-buffer ()
  (let ((buf-list (+visible-buffers))
        (b (get-buffer-create " *visible-buffers-list*" t)))
    (with-current-buffer b
      (erase-buffer)
      (insert! "# These are the buffers currently visible to the user.\n")
      (seq-doseq
          (b (+visible-buffers))
        (insert! ("Buffer name: `%s`" (buffer-name b)))
        (when-let ((buf-file (buffer-file-name b)))
          (insert! (",Buffer's File name: `%s`" buf-file))
          (if-let ((buf-proj-root (doom-project-root buf-file)))
              (insert! (",File project root: `%s`" buf-proj-root))
            (insert! ",File is not in any project")))
        (insert! "\n"))
      (insert! "\n"))
    b))

(comment
  (insert!
   "\n"
   ("%s" (with-current-buffer
             (+visible-buffers-list-buffer)
           (buffer-string)))))

;;; clojure workspace
(defun ++workspace-cljs? ()
  (-> (expand-file-name
       "shadow-cljs.edn"
       (++workspace-current-project-root))
      (file-exists-p)))

(defun ++workspace-clj? ()
  (-> (expand-file-name
       "deps.edn"
       (++workspace-current-project-root))
      (file-exists-p)))

(defun ++workspace-clojure? ()
  (or (++workspace-clj?)
      (++workspace-cljs?)))

(defun ++workspace-cljs-repl-connected? ()
  (when (and (++workspace-cljs?)
             (fboundp 'cider-connected-p))
    (let ((cljs-buf
           (-> (clj/filter
                (clj/comp
                 (clj/partial 'string-suffix-p ".cljs")
                 'buffer-file-name)
                (+workspace-buffer-list))
               (clj/first))))
      (when (buffer-live-p cljs-buf)
        (with-current-buffer cljs-buf
          (cider-connected-p))))))

(defun ++workspace-clj-repl-connected? ()
  (when (and (++workspace-clj?)
             (fboundp 'cider-connected-p))
    (with-current-buffer
        (-> (expand-file-name
             "deps.edn"
             (++workspace-current-project-root))
            (find-file-noselect))
      (cider-connected-p))))

(defun ++workspace-cider-connected-p ()
  (and (++workspace-clojure?)
       (or (++workspace-cljs-repl-connected?)
           (++workspace-clj-repl-connected?))))

;;; js workspace
(defun ++workspace-npm? ()
  (-> (expand-file-name
       "package.json"
       (++workspace-current-project-root))
      (file-exists-p)))

(defun ++workspace-nextjs? ()
  (-> (expand-file-name
       ".next"
       (++workspace-current-project-root))
      (file-exists-p)))

(defun ++workspace-nextjs-live? ()
  (-> (expand-file-name
       ".next/dev/lock"
       (++workspace-current-project-root))
      (file-exists-p)))

;;; workspace info buffer
(defun +current-workspace-info-buffer ()
  (let ((b (get-buffer-create " *user-current-workspace-info*" t)))
    (with-current-buffer b
      (erase-buffer)
      (insert!
       "# User Current Workspace Info\n"
       "<env>\n"
       ("Today's date: `%s`\n" (format-time-string "%Y-%m-%d"))
       ("workspace name: `%s`\n" (+workspace-current-name))
       ("workspace root: `%s`\n" (++workspace-current-project-root))
       ("workspace project name: `%s`\n"
        (doom-project-name (++workspace-current-project-root)))
       ("workspace project root: `%s`\n" (++workspace-current-project-root))
       ("Is project a git repo: %s\n"
        (if (magit-git-repo-p (++workspace-current-project-root))
            "Yes" "No"))

       ((and (++workspace-clojure?)
             "workspace project type: %s project\n")
        (cond
         ((and (++workspace-cljs?) (++workspace-clj?)) "clj and cljs")
         ((++workspace-cljs?) "cljs")
         (t "clj")))

       ((and (++workspace-clj?)
             "clj nREPL is %s.\n")
        (if (++workspace-clj-repl-connected?)
            "connected, you can evaluate clj file/buffer/code"
          "not connected, you can't evaluate clj"))

       ((and (++workspace-clj?)
             "shadow-cljs nREPL is %s.\n")
        (if (++workspace-cljs-repl-connected?)
            "connected, you can evaluate cljs file/buffer/code"
          "not connected, you can't evaluate cljs"))

       ((and (++workspace-npm?) (++workspace-nextjs?)
             "This project is using Next.js, the Next.js dev server is %s.\n")
        (if (++workspace-nextjs-live?)
            "running"
          "not running"))
       "</env>"))
    b))

(defvar +llm-project-default-files '())
(make-variable-buffer-local '+llm-project-default-files)
(put '+llm-project-default-files 'safe-local-variable #'listp)

(defun ++workspace-current-project-root ()
  (or (persp-parameter '+workspace-project (+workspace-current))
      (when (string= (+workspace-current-name) "main")
        (expand-file-name "~/.nixpkgs"))))

;;;###autoload
(defun ++workspace-set-project-to-current-buffer-project ()
  "Set persp workspace project to current buffer project"
  (interactive)
  (unless (+workspace-current)
    (user-error "No current workspace"))
  (unless (doom-project-root)
    (user-error "No project root"))
  (set-persp-parameter
   '+workspace-project (doom-project-root) (+workspace-current)))

(defun +tmp-file-p (file-path)
  (or
   (string-prefix-p "/private/var/folders" file-path)
   (string-prefix-p "/tmp" file-path)))

;; Define a specific timeout error (safe if re-evaluated)
(unless (get 'await-timeout 'error-conditions)
  (define-error 'await-timeout "await-callback: timed out"))

(defun await-callback (starter &optional timeout)
  "STARTER is (lambda (resolve reject) ...). Block until it calls one.

TIMEOUT is seconds (float or integer). Defaults to 1.0s.
On timeout, signals `await-timeout'. C-g aborts and signals `quit'.

Example:
  (await-callback
   (lambda (resolve _reject)
     (run-at-time 0.2 nil (lambda () (funcall resolve :ok)))))  ;; => :ok"
  (let* ((timeout (or timeout 1.0))
         (deadline (+ (float-time) timeout))
         (done nil) (value nil) (err nil))
    (funcall starter
             (lambda (v) (setq value v done t))
             (lambda (e) (setq err e done t)))
    (with-local-quit
      (while (not done)
        (when (> (float-time) deadline)
          (signal 'await-timeout (list timeout)))
        (accept-process-output nil 0.1)))
    (when quit-flag
      (setq quit-flag nil)
      (signal 'quit nil))
    (if err (signal (car err) (cdr err)) value)))

(defun +kitty-get-project-windows ()
  (let* ((project-root
          (string-remove-suffix "/"
                                (++workspace-current-project-root)))
         (json (shell-command-to-string
                (format
                 (expand-file-name
                  "~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/local-bins/kitty-ls-project-windows '%s'")
                 project-root))))
    (ignore-errors (json-parse-string json))))

(defun +kitty-get-window-last-cmd-output (window-id)
  (+kitty
   (format! "@ get-text --match id:%d --extent last_cmd_output" window-id)))

(defun +force-save-buffer ()
  "Save current buffer, answering yes to supersession/lock prompts; skip hooks."
  (interactive)
  (cl-letf (((symbol-function 'ask-user-about-supersession-threat) (lambda (&rest _) t))
            ((symbol-function 'ask-user-about-lock) (lambda (&rest _) t))
            ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
            ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
    (let ((before-save-hook nil)
          (after-save-hook nil)
          (write-contents-functions nil)
          (write-file-functions nil)
          (create-lockfiles nil))
      (basic-save-buffer))))
