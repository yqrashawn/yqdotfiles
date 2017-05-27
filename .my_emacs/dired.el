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
