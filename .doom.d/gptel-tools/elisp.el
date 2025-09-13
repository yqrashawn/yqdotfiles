;;; .nixpkgs/.doom.d/gptel-tools/elisp.el -*- lexical-binding: t; -*-

(require 'find-func)

;;; Tool: Evaluate Elisp Buffer (ask for confirmation, show buffer if not visible)
(defun gptel-evaluate-elisp-buffer (buffer-name)
  "Evaluate elisp BUFFER-NAME after confirming with user. Show buffer if not visible. Bury if not shown before."
  (interactive)
  (let* ((buf (get-buffer buffer-name))
         (shown-before (get-buffer-window buf))
         win)
    (unless buf (error "Buffer not found: %s" buffer-name))
    (setq win (unless shown-before (display-buffer buf)))
    (when (y-or-n-p (format "Evaluate buffer %s? " buffer-name))
      (with-current-buffer buf
        (eval-buffer)
        (message "Evaluated buffer: %s" buffer-name)))
    (when (and win (window-live-p win))
      (quit-restore-window win 'bury))))

;;; Tool: Evaluate Elisp File (load file into buffer, then evaluate, with confirmation)
(defun gptel-evaluate-elisp-file (file-path)
  "Load FILE-PATH into buffer and evaluate after confirming with user. Show buffer if not visible. Bury if not shown before."
  (interactive)
  (unless (file-name-absolute-p file-path) (error "file-path must be absolute"))
  (gptel-evaluate-elisp-buffer (find-file-noselect file-path)))

;;; gptel tool registration
(when (fboundp 'gptel-make-tool)
  (gptel-make-tool
   :name "evaluate_elisp_buffer"
   :function #'gptel-evaluate-elisp-buffer
   :description "Evaluate a buffer by buffer name (after user confirmation). Shows buffer if not visible, buries if not shown before."
   :args (list '(:name "buffer_name" :type string :description "The name of the buffer to evaluate"))
   :category "elisp"
   :confirm t
   :include t)

  (gptel-make-tool
   :name "evaluate_elisp_file"
   :function #'gptel-evaluate-elisp-file
   :description "Evaluate a file by loading it as a buffer and evaluating (after user confirmation). Shows buffer if not visible, buries if not shown before."
   :args (list '(:name "file_path" :type string :description "Absolute path to the elisp file to evaluate"))
   :category "elisp"
   :confirm t
   :include t))
