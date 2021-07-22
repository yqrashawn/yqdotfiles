;;; tty/autoloads.el -*- lexical-binding: t; -*-

;;;###autoload
(defun yq/tmux-command (&rest args)
  (emamux:check-tmux-running)
  (apply #'emamux:tmux-run-command nil args))

;;;###autoload
(defun yq/tmux-select-pane (dir)
  (if (or (ignore-errors (funcall (intern (concat "windmove-" dir)))) (display-graphic-p)) nil
    (cond ((string= dir "up")
           (yq/tmux-command "select-pane" "-U"))
          ((string= dir "down")
           (yq/tmux-command "select-pane" "-D"))
          ((string= dir "left")
           (yq/tmux-command "select-pane" "-L"))
          ((string= dir "right")
           (yq/tmux-command "select-pane" "-R")))))

;;;###autoload
(defun yq/split-window-right-tmux (arg)
  (interactive "P")
  (if arg
      (emamux:split-window-horizontally)
    (yq/split-window-right)))

;;;###autoload
(defun yq/split-window-below-tmux (arg)
  (interactive "P")
  (if arg
      (emamux:split-window)
    (yq/split-window-below)))

;;;###autoload
(defun yq/toggle-maximize-buffer-tmux (arg)
  (interactive "P")
  (if arg
      (yq/tmux-command "resize-pane" "-Z")
    (spacemacs/toggle-maximize-buffer)))
