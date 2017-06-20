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

(defvar snwob-starting-window-or-buffer nil)

(defun snwob-single-buffer-p ()
  "Return non-nil if the current frame has one buffer only."
  (null (cdr (buffer-list (selected-frame)))))

(defun snwob-single-frame-p ()
  "Return non-nil if the selected frame is the only one."
  (null (cdr (frame-list))))

(defun snwob--current-window-or-buffer ()
  "Return the current buffer if there is a single window.
Otherwise, return the selected window."
  (if (one-window-p)
    (current-buffer)
  (selected-window)))

(defun snwob--other-frame-or-window-or-buffer ()
  "Switch to the other frame if there is more than one.
Otherwise, call `snwob--other-window-or-buffer'."
  (if (snwob-single-frame-p)
    (snwob--other-window-or-buffer)
  (other-frame 1)
  (setq this-command #'other-frame)))

(defun snwob--other-window-or-buffer ()
  "Switch to another window if there is one.
Otherwise, switch to the other buffer."
  (cond ((one-window-p)
   (switch-to-buffer (other-buffer (current-buffer) t (selected-frame))))
  (t
   (other-window 1))))

(defun smart-next-window-or-buffer ()
  "Switch to the other buffer if there is one window only.
Otherwise, switch to another window.  After a full cycle of two
buffers (or as many windows as there are in the selected frame)
switch to another frame."
  (interactive)
  (cond ((eq last-command #'smart-next-window-or-buffer)
   (if (eq snwob-starting-window-or-buffer (snwob--current-window-or-buffer))
     (snwob--other-frame-or-window-or-buffer)
     (snwob--other-window-or-buffer)))
  (t
   (setq snwob-starting-window-or-buffer
       (snwob--current-window-or-buffer))
   (snwob--other-window-or-buffer))))

(define-key evil-normal-state-map "sk" #'smart-next-window-or-buffer)
(global-set-key (kbd "s-h") #'smart-next-window-or-buffer)
