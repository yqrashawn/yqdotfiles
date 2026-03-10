;;; .nixpkgs/.doom.d/gptel-tools.el -*- lexical-binding: t; -*-

(require 'smartparens nil t)

(load! "gptel-tools/utils.el")
;; (load! "gptel-tools/todo.el")
(load! "gptel-tools/buffer.el")
(load! "gptel-tools/read.el")
;; (load! "gptel-tools/edit-file.el")
;; (load! "gptel-tools/create-file.el")
(load! "gptel-tools/lint.el")
;; (load! "gptel-tools/ripgrep.el")
(load! "gptel-tools/elisp.el")
(load! "gptel-tools/clj.el")
(load! "gptel-tools/cljs.el")
(load! "gptel-tools/pext.el")
;; (load! "gptel-tools/shell.el")
(load! "gptel-tools/workspace.el")
(load! "gptel-tools/ask-user-question.el")

(comment
  (+gptel-reinit))

(defun +gptel-reinit ()
  (interactive)
  (require 'mcp-server-lib)
  (require 'gptel)
  (require 'mcp)
  (require 'mcp-hub)
  (require 'gptel-integrations)
  (mcp-hub-close-all-server)
  (setq mcp-server-lib--tools (make-hash-table :test 'equal))
  (setq gptel--known-tools nil)
  (when (mcp--server-running-p "emacs")
    (mcp-stop-server "emacs"))
  (when mcp-server-lib--running
    (mcp-server-lib-http-stop)
    (mcp-server-lib-stop))
  (load! "gptel-tools.el")
  (when (fboundp 'gptel-claude-code-mcp-register-tools)
    (gptel-claude-code-mcp-register-tools))
  (mcp-server-lib-start)
  (mcp-server-lib-http-start)
  (mcp-hub-start-all-server
   (lambda ()
     (gptel-mcp-connect
      nil (lambda ()
            (+gptel-make-my-presets)
            (gptel--apply-preset +gptel-default-preset))))))

;;; End
