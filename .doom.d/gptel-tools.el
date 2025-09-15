;;; .nixpkgs/.doom.d/gptel-tools.el -*- lexical-binding: t; -*-

(require 'smartparens nil t)

(load! "gptel-tools/utils.el")
(load! "gptel-tools/todo.el")
(load! "gptel-tools/buffer.el")
(load! "gptel-tools/read.el")
(load! "gptel-tools/edit-file.el")
(load! "gptel-tools/create-file.el")
(load! "gptel-tools/ripgrep.el")
(load! "gptel-tools/elisp.el")

(comment
  (+gptel-reload-tools)

  (mcp-stop-server "emacs")
  (mcp-hub--start-server
   (cl-find "emacs" mcp-hub-servers :key #'car :test #'equal)))

(defun +gptel-reload-tools ()
  (interactive)
  (setq mcp-server-lib--tools (make-hash-table :test 'equal))
  (setq gptel--known-tools nil)
  (mcp-stop-server "emacs")
  (load! "gptel-tools.el")
  (mcp-hub--start-server
   (cl-find "emacs" mcp-hub-servers :key #'car :test #'equal)
   nil t)
  (gptel-mcp-connect)
  (load! "gptel-tools.el")
  (+gptel-make-my-presets))

;;; End
