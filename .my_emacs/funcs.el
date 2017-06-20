(defun duplicate-line ()
  "Duplicate current line"
  (interactive)
  (kill-whole-line)
  (yank)
  (yank))

(defun ora-ediff-files ()
  (interactive)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "file: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (set-window-configuration wnd))))
      (error "no more than 2 files should be marked"))))

(define-key global-map (kbd "s-i") 'open-terminal-here)
(defun open-terminal-here ()
  (interactive)
  (shell-command "open -a iTerm ."))

(require 'butler)
(add-to-list 'butler-server-list
             '(jenkins "Jenkins"
                       (server-address . "http://114.215.220.91:8080/")
                       (auth-file . "~/.authinfo.gpg")))
