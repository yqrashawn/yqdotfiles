;;; .nixpkgs/.doom.d/gptel-tools/shell.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Shell command execution tool for gptel/MCP
;; Allows LLMs to run shell commands asynchronously without blocking Emacs

;;; Code:

;; Note: gptelt-make-tool and gptelt--get-project-root are provided by utils.el
;; which is loaded by the main gptel-tools configuration

;;; Customizable Variables
(defgroup gptelt-shell nil
  "Shell command execution settings for gptel tools."
  :group 'gptel-tools)

(defcustom gptelt-shell-command-default-shell
  (or (executable-find "zsh") "/bin/sh")
  "Default shell for running commands."
  :type 'string
  :group 'gptelt-shell)

(defcustom gptelt-shell-command-max-output-bytes (* 100 1024 1024)
  "Maximum output size in bytes before truncation (default 100MB)."
  :type 'integer
  :group 'gptelt-shell)

(defcustom gptelt-shell-command-default-timeout 300
  "Default timeout in seconds for shell commands."
  :type 'integer
  :group 'gptelt-shell)

;;; Internal Functions

(defun gptelt--shell-truncate-output (output max-bytes)
  "Truncate OUTPUT if it exceeds MAX-BYTES.
Returns truncated string with a notice if truncation occurred."
  (if (> (string-bytes output) max-bytes)
      (let ((truncated (substring output 0 (min (length output)
                                                (/ max-bytes 2)))))  ; rough estimate
        (concat truncated
                "\n\n[OUTPUT TRUNCATED: exceeded "
                (file-size-human-readable max-bytes)
                " limit]"))
    output))

;;; Main Async Function
(defun gptelt-run-shell-command (callback command &optional directory timeout shell)
  "Run COMMAND asynchronously and call CALLBACK with result.

CALLBACK is called with a string containing the combined stdout/stderr output.
COMMAND is the shell command string to execute.
DIRECTORY is the working directory (defaults to project root).
TIMEOUT is the timeout in seconds (defaults to `gptelt-shell-command-default-timeout').
SHELL is the shell executable (defaults to `gptelt-shell-command-default-shell')."
  (let* ((work-dir (or directory (gptelt--get-project-root) default-directory))
         (default-directory work-dir)
         (timeout-secs (or timeout gptelt-shell-command-default-timeout))
         (shell-exe (or shell gptelt-shell-command-default-shell))
         (output-buffer (generate-new-buffer " *gptelt-shell-output*"))
         (timeout-timer nil)
         (callback-called nil)  ; Track if callback has been invoked
         (process nil))

    ;; Set up the process
    (setq process
          (make-process
           :name "gptelt-shell-command"
           :buffer output-buffer
           :command (list shell-exe "-c" command)
           :connection-type 'pipe
           :sentinel
           (lambda (proc event)
             ;; Only run sentinel if callback hasn't been called yet (not timed out)
             (unless callback-called
               ;; Cancel timeout timer if still active
               (when timeout-timer
                 (cancel-timer timeout-timer)
                 (setq timeout-timer nil))

               (let ((exit-status (process-exit-status proc))
                     (output ""))
                 ;; Collect output from buffer
                 (when (buffer-live-p output-buffer)
                   (with-current-buffer output-buffer
                     (setq output (buffer-string)))
                   (kill-buffer output-buffer))

                 ;; Truncate if needed
                 (setq output (gptelt--shell-truncate-output
                               output
                               gptelt-shell-command-max-output-bytes))

                 ;; Mark callback as called and invoke it
                 (setq callback-called t)
                 (let ((result (format "Exit code: %d\nWorking directory: %s\n\n%s"
                                       exit-status
                                       work-dir
                                       output)))
                   (funcall callback result)))))))

    ;; Set up timeout
    (setq timeout-timer
          (run-at-time
           timeout-secs nil
           (lambda ()
             (when (and (process-live-p process) (not callback-called))
               ;; Mark callback as called FIRST (before killing process)
               ;; This prevents the sentinel from running when we kill the process
               (setq callback-called t)
               
               ;; Collect partial output before killing
               (let ((output ""))
                 (when (buffer-live-p output-buffer)
                   (with-current-buffer output-buffer
                     (setq output (buffer-string))))
                 
                 ;; Now kill the process (sentinel won't call callback)
                 (let ((confirm-kill-processes (log/spy nil)))
                   (kill-process process))
                 
                 ;; Clean up buffer
                 (when (buffer-live-p output-buffer)
                   (let ((kill-buffer-query-functions
                          (delq 'process-kill-buffer-query-function kill-buffer-query-functions)))
                     (kill-buffer output-buffer)))
                 
                 (setq output (gptelt--shell-truncate-output
                               output
                               gptelt-shell-command-max-output-bytes))
                 
                 ;; Invoke callback with timeout message
                 (funcall callback
                          (format "TIMEOUT: Command exceeded %d seconds and was killed\nExit code: 9\nWorking directory: %s\n\n%s"
                                  timeout-secs
                                  work-dir
                                  output)))))))

    ;; Return nil immediately (non-blocking)
    nil))

(comment
  (gptelt-run-shell-command #'message "ls -al" nil 0)
  (gptelt-run-shell-command #'message "tsc --noEmit" "/Users/yqrashawn/workspace/office/perpdex-frontend"))

;;; Tool Registration
(gptelt-make-tool
 :name "run_shell_command"
 :function #'gptelt-run-shell-command
 :async t
 :confirm t
 :description "Run a shell command and return the combined stdout/stderr output. Use this for executing system commands, build tools, scripts, etc. The command runs in a zsh subshell and returns when complete or when timeout is reached."
 :args '((:name "command"
          :type "string"
          :description "The shell command to execute"
          :required t)
         (:name "directory"
          :type "string"
          :optional t
          :description "Working directory for command execution. Defaults to workspace/project root if not specified.")
         (:name "timeout"
          :type "integer"
          :optional t
          :description "Timeout in seconds. Process is killed if it exceeds this. Default: 300 seconds.")
         ;; (:name "shell"
         ;;  :type "string"
         ;;  :description "Shell executable path. Default: zsh (if available) or /bin/sh.")
         )
 :category "shell"
 :include t)

(provide 'gptel-tools/shell)
;;; shell.el ends here
