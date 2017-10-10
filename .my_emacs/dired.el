(setq dired-listing-switches
      (if (eq system-type 'windows-nt)
          "-alh"
        "-laGh1v --group-directories-first"))
(setq directory-free-space-args "-Pmh")
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq dired-omit-files
      (format "\\(?:\\.%s\\'\\)\\|%s\\|\\`\\.[^.]\\|\\`_minted"
              (regexp-opt
               '("aux" "pickle" "synctex.gz" "bcf" "am" "in" "blx.bib"
                 "vrb" "opt" "nav" "snm" "out"))
              (regexp-opt
               '("compile_commands.json"
                 "__pycache__"))))
(setq dired-omit-verbose nil)

;;* advice
(defadvice dired-advertised-find-file (around ora-dired-subst-directory activate)
  "Replace current buffer if file is a directory."
  (interactive)
  (let* ((orig (current-buffer))
         (filename (dired-get-filename t t))
         (bye-p (file-directory-p filename)))
    ad-do-it
    (when (and bye-p (not (string-match "[/\\\\]\\.$" filename)))
      (kill-buffer orig))))

(defadvice dired-delete-entry (before ora-force-clean-up-buffers (file) activate)
  (let ((buffer (get-file-buffer file)))
    (when buffer
      (kill-buffer buffer))))

;;* rest
(defun ora-dired-get-size ()
  (interactive)
  (let* ((cmd (concat "du -sch "
                      (mapconcat (lambda (x) (shell-quote-argument (file-name-nondirectory x)))
                                 (dired-get-marked-files) " ")))
         (res (shell-command-to-string cmd)))
    (if (string-match "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$" res)
        (message (match-string 1 res))
      (error "unexpected output %s" res))))

(defvar ora-dired-filelist-cmd
  '(("vlc" "-L")))

(require 'hydra)
(defhydra hydra-marked-items (dired-mode-map "")
  "
Number of marked items: %(length (dired-get-marked-files))
"
  ("m" dired-mark "mark")
  ("e" ora-ediff-files "ediff")
  ("C" dired-do-copy "copy")
  ("D" dired-do-delete "delete")
  )

(defun ora-ediff-files ()
  (interactive)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name "file: "
                                       (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (set-window-configuration wnd))))
      (error "no more than 2 files should be marked"))))

(defun my-dired-imenu-prev-index-position (&optional arg)
  "Go to the header line of previous directory."
  (interactive "p")
  (unless (= (line-number-at-pos) 1)
    (call-interactively 'dired-prev-subdir)
    t))

(defun my-dired-extract-index-name ()
  "Extract name of the current item for imenu."
  (save-excursion
    (back-to-indentation)
    (buffer-substring-no-properties
     (point)
     (1- (re-search-forward ":$")))))

(defun my-dired-imenu-create-index ()
  "Create `imenu' index for dired."
  (let* ((alist (imenu-default-create-index-function))
         (uniquified (f-uniquify-alist (-map 'car alist))))
    (--remove
     (= 0 (length (car it)))
     (--map (cons (cdr (assoc (car it) uniquified)) (cdr it))
            alist))))

(defun my-dired-imenu-init ()
  "Initialize `imenu' variables in current buffer."
  (setq-local imenu-prev-index-position-function
              'my-dired-imenu-prev-index-position)
  (setq-local imenu-extract-index-name-function
              'my-dired-extract-index-name)
  (setq-local imenu-create-index-function
              'my-dired-imenu-create-index))

(add-hook 'dired-mode-hook 'my-dired-imenu-init)

(with-eval-after-load 'dired
  (require 'ivy-dired-history)
  (require 'dired-quick-sort)
  (dired-quick-sort-setup)
  (define-key dired-mode-map "S" 'dired-do-symlink)
  (define-key dired-mode-map "s" 'hydra-dired-quick-sort/body)
  (define-key dired-mode-map "," 'dired))

(use-package dired-subtree
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";;" . dired-subtree-remove)))
