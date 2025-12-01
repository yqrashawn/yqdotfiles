;;; .nixpkgs/.doom.d/gptel-tools/shell.el -*- lexical-binding: t; -*-

(require 'detached)

(defun gptelt-shell-run-command (callback command &optional cwd)
  "Run COMMAND asynchronously using detached.el with smart timing detection.

CALLBACK is called with results based on command completion timing:
  - Commands finishing < 300ms: Returns full results immediately
  - Commands finishing 300ms-1.2s: Returns full results after socket creation
  - Commands taking > 1.2s: Returns session ID for manual polling

Optional CWD sets the working directory for the command.

DESIGN RATIONALE:
This hybrid approach optimizes for UX:
- Fast commands feel synchronous (no polling needed)
- Medium-speed commands are caught after dtach socket creation (~1.0s)
- Long-running commands return immediately with session ID for async polling

TIMING WINDOWS:
  0ms ────────────────────────────────────> time
       ↓                    ↓               ↓
    instant              socket          still running
    (<300ms)           creation          (>1.2s)
                       (~1.0s)

  Timer1 (300ms):  Catches instant completions
  Timer2 (1.2s):   Catches post-socket completions OR returns session ID

IMPORTANT:
- This function is ASYNC for Emacs (non-blocking, returns immediately)
- The CALLBACK is invoked when timing checks complete
- Dual-timer prevents race conditions with dtach socket creation
- already-responded flag prevents duplicate callbacks"
  (let* ((default-directory (or cwd default-directory))
         (detached-session-origin 'gptel)
         (detached-session-mode 'detached)
         ;; Disable notifications for MCP shell commands
         (detached-notification-function #'ignore)
         ;; Flag to prevent duplicate callbacks from dual-timer approach
         (already-responded nil)
         (detached-session-action
          `(:attach detached-shell-command-attach-session
            :view detached-view-dwim
            :run detached-start-shell-command-session))
         (session (detached-create-session command))
         (session-id (detached-session-id session)))
    (detached-start-session session)

    ;; TIMER 1 (300ms): Check for instant command completion
    ;; Catches commands that finish before dtach socket is created
    ;; Uses when-let for safe session access (may not exist yet)
    (run-with-timer
     0.3 nil
     (lambda ()
       (when-let ((updated-session (detached--db-get-session session-id)))
         ;; Verify session is both validated AND inactive before reading results
         (when (and (detached-session-validated-p updated-session)
                    (detached-session-inactive-p updated-session))
           (setq already-responded t)  ; Prevent Timer 2 from firing
           (let* ((status (detached-session-status updated-session))
                  (exit-code (detached-session-exit-code updated-session))
                  (output (+safe-detached-session-output updated-session))
                  (duration (detached-session-duration updated-session)))
             (funcall callback
                      (format "Command completed in %.2fs\nSession ID: %s\nStatus: %s\nExit Code: %s\n\nOutput:\n%s"
                              duration
                              (symbol-name session-id)
                              status
                              exit-code
                              output)))))))

    ;; TIMER 2 (1.2s): Check after dtach socket creation window
    ;; This catches commands that:
    ;;   1. Finish between 300ms-1.2s (after socket created)
    ;;   2. Are still running (return session ID for polling)
    ;; 
    ;; TIMING RATIONALE:
    ;;   - detached-dtach-socket-creation-delay = 1.0s (default)
    ;;   - Add 0.2s margin for file-notify to trigger
    ;;   - Total: 1.2s ensures socket exists and state is updated
    (run-with-timer
     1.2 nil
     (lambda ()
       (unless already-responded  ; Only execute if Timer 1 didn't respond
         (setq already-responded t)  ; Mark as responded regardless of outcome
         (let ((updated-session (detached--db-get-session session-id)))
           ;; Check if command completed in the 300ms-1.2s window
           (if (and updated-session
                    (detached-session-validated-p updated-session)
                    (detached-session-inactive-p updated-session))
               ;; SUCCESS PATH: Command finished in the 300ms-1.2s window
               ;; Return full results (exit code, output, duration)
               (let* ((status (detached-session-status updated-session))
                      (exit-code (detached-session-exit-code updated-session))
                      (output (+safe-detached-session-output updated-session))
                      (duration (detached-session-duration updated-session)))
                 (funcall callback
                          (format "Command completed in %.2fs\nSession ID: %s\nStatus: %s\nExit Code: %s\n\nOutput:\n%s"
                                  duration
                                  (symbol-name session-id)
                                  status
                                  exit-code
                                  output)))
             ;; ASYNC PATH: Command is still running after 1.2s
             ;; Return session ID for manual polling via other tools:
             ;;   - get_shell_command_session_info (check status)
             ;;   - get_shell_command_session_output (get output)
             ;;   - kill_shell_command_session (terminate)
             (funcall callback
                      (format "Command started with session ID: %s"
                              (symbol-name session-id))))))))))

(comment
  (gptelt-shell-run-command 'message "date"))

(defun gptelt-shell-run-command-sync (callback command &optional cwd)
  "Run COMMAND synchronously using detached.el, waiting for completion.
CALLBACK is called with full session info and output when command finishes.
If CWD is provided, run command in that directory.

This is an async tool (non-blocking for Emacs) but synchronous in behavior
as it only returns results after the command completes."
  (let* ((default-directory (or cwd default-directory))
         (detached-session-origin 'gptel)
         (detached-session-mode 'detached)
         ;; Disable notifications for MCP shell commands
         (detached-notification-function #'ignore)
         (session-callback
          (lambda (session)
            (condition-case err
                (let* ((session-id (detached-session-id session))
                       (status (detached-session-status session))
                       (exit-code (detached-session-exit-code session))
                       (output (+safe-detached-session-output session))
                       (duration (detached-session-duration session)))
                  (funcall callback
                           (format "Command completed in %.2fs\nSession ID: %s\nStatus: %s\nExit Code: %s\n\nOutput:\n%s"
                                   duration
                                   (symbol-name session-id)
                                   status
                                   exit-code
                                   output)))
              (error
               (funcall callback
                        (format "Error in callback: %s\nSession ID: %s"
                                (error-message-string err)
                                (symbol-name (detached-session-id session))))))))
         (detached-session-action
          `(:attach detached-shell-command-attach-session
            :view detached-view-dwim
            :run detached-start-shell-command-session
            :callback ,session-callback))
         (session (detached-create-session command)))
    (detached-start-session session)))

(comment
  (gptelt-shell-run-command-sync 'message "date")
  (gptelt-shell-run-command-sync 'message "sleep 2 && echo done")
  (gptelt-shell-run-command-sync 'message "cd ~/workspace/office/perpdex-nextjs && bunx tsc --noEmit"))

(defun gptelt-shell-get-shell-command-session-info (callback session-id)
  "Get information about a detached SESSION-ID.
CALLBACK is called with a property list containing session info."
  (let* ((id (if (stringp session-id) (intern session-id) session-id))
         (session (detached--db-get-session id)))
    (if (not session)
        (funcall callback
                 (format "Session not found: %s" session-id))
      (let* ((state (detached-session-state session))
             (status (detached-session-status session))
             (exit-code (detached-session-exit-code session))
             (command (detached-session-command session))
             (cwd (detached-session-working-directory session))
             (host (detached-session-host-name session))
             (start-time (detached-session-start-time session))
             (duration (detached-session-duration session))
             (output-file (detached--session-file session 'log))
             (info (format "Session: %s
State: %s
Status: %s
Exit Code: %s
Command: %s
Working Directory: %s
Host: %s
Start Time: %s
Duration: %s seconds
Output File: %s"
                           (symbol-name (detached-session-id session))
                           state
                           status
                           exit-code
                           command
                           cwd
                           host
                           (if (> start-time 0)
                               (format-time-string "%Y-%m-%d %H:%M:%S" start-time)
                             "Not started")
                           (if (detached-session-inactive-p session)
                               (format "%.2f" duration)
                             (format "%.2f (running)" duration))
                           output-file)))
        (funcall callback info)))))

(defun gptelt-shell-get-shell-command-session-output
    (callback session-id &optional lines)
  "Get output from SESSION-ID.
CALLBACK is called with the output string.
If LINES is provided, return only the last N lines."
  (let* ((id (if (stringp session-id) (intern session-id) session-id))
         (session (detached--db-get-session id)))
    (if (not session)
        (funcall callback
                 (format "Session not found: %s" session-id))
      (let ((output (+safe-detached-session-output session)))
        (if lines
            (let* ((lines-list (split-string output "\n"))
                   (total-lines (length lines-list))
                   (start (max 0 (- total-lines lines)))
                   (last-n-lines (seq-subseq lines-list start)))
              (funcall callback (string-join last-n-lines "\n")))
          (funcall callback output))))))

(comment
  (gptelt-shell-get-shell-command-session-output
   'message
   "43f3532b4ecb931ddd928b45ec7e1a84"))

(defun gptelt-shell-kill-shell-command-session (callback session-id)
  "Kill the detached session with SESSION-ID.
CALLBACK is called with a status message."
  (let* ((id (if (stringp session-id) (intern session-id) session-id))
         (session (detached--db-get-session id)))
    (if (not session)
        (funcall callback
                 (format "Session not found: %s" session-id))
      (if (not (detached-session-active-p session))
          (funcall callback
                   (format "Session %s is not active (state: %s)"
                           session-id
                           (detached-session-state session)))
        (detached-kill-session session)
        (funcall callback
                 (format "Sent termination signal to session: %s" session-id))))))

(defun gptelt-shell-list-shell-command-sessions (callback &optional active-only)
  "List all detached sessions.
CALLBACK is called with a formatted string listing sessions.
If ACTIVE-ONLY is non-nil, only list active sessions."
  (let* ((all-sessions (detached-get-sessions))
         (sessions (if active-only
                       (seq-filter #'detached-session-active-p all-sessions)
                     all-sessions))
         (formatted-sessions
          (mapcar
           (lambda (session)
             (format "%s | %s | %s | %s | %s"
                     (symbol-name (detached-session-id session))
                     (detached-session-state session)
                     (detached-session-status session)
                     (detached-session-host-name session)
                     (truncate-string-to-width
                      (detached-session-command session)
                      60 nil nil "...")))
           sessions)))
    (funcall callback
             (if (null formatted-sessions)
                 "No sessions found"
               (concat "Session ID | State | Status | Host | Command\n"
                       (string-join formatted-sessions "\n"))))))

(when (fboundp 'gptelt-make-tool)
  (gptelt-make-tool
   :name "run_shell_command"
   :function #'gptelt-shell-run-command-sync
   :async t
   :description 
   "Run a shell command and wait for completion.

Returns complete session info including:
- Exit status (success/failure)
- Exit code
- Full command output
- Execution duration

Use this for:
- Commands where you need the output before proceeding
- Short to medium duration tasks (up to 1 hour)
- Build commands, tests, file operations

For fire-and-forget long-running tasks, use run_shell_command_async instead."
   :args '((:name "command" :type string
            :description "The shell command to run")
           (:name "cwd" :type string :optional t
            :description "Working directory for the command (defaults to current directory)"))
   :category "shell"
   :confirm nil
   :include t)

  (gptelt-make-tool
   :name "run_shell_command_async"
   :function #'gptelt-shell-run-command
   :async t
   :description 
   "Run a shell command asynchronously.

For quick commands (completing within 1.2s):
- Returns complete session info and output immediately
- No need to call additional tools

For longer-running commands:
- Returns session ID only
- Use get_shell_command_session_info to check status
- Use get_shell_command_session_output to retrieve output

Use this to:
- Run long-running commands
- Execute background tasks
- Start build processes
- Launch development servers"
   :args '((:name "command" :type string
            :description "The shell command to run")
           (:name "cwd" :type string :optional t
            :description "Working directory for the command (defaults to current directory)"))
   :category "shell"
   :confirm nil
   :include t)

  (gptelt-make-tool
   :name "get_shell_command_session_info"
   :function #'gptelt-shell-get-shell-command-session-info
   :async t
   :description (concat "Get detailed information about a detached session.\n\n"
                        "Returns:\n"
                        "- State: active (running), inactive (finished), or unknown\n"
                        "- Status: success or failure (only for inactive sessions)\n"
                        "- Exit code: the command's exit code\n"
                        "- Command: the original command string\n"
                        "- Working directory\n"
                        "- Host: where the command is running\n"
                        "- Start time\n"
                        "- Duration: elapsed time (for running sessions) or total time (for finished sessions)\n"
                        "- Output file: path to the log file\n\n"
                        "Use this to check if a command has finished and whether it succeeded.")
   :args '((:name "session_id" :type string
            :description "The session ID returned from run_shell_command"))
   :category "shell"
   :confirm nil
   :include t)

  (gptelt-make-tool
   :name "get_shell_command_session_output"
   :function #'gptelt-shell-get-shell-command-session-output
   :async t
   :description (concat "Get the stdout/stderr output from a detached session.\n\n"
                        "Returns the complete output captured from the command. "
                        "For active (running) sessions, returns output captured so far. "
                        "For inactive (finished) sessions, returns the complete output.\n\n"
                        "Use the 'lines' parameter to get only the last N lines (useful for monitoring active sessions).")
   :args '((:name "session_id" :type string
            :description "The session ID returned from run_shell_command")
           (:name "lines" :type number :optional t
            :description "If provided, return only the last N lines of output"))
   :category "shell"
   :confirm nil
   :include t)

  (gptelt-make-tool
   :name "kill_shell_command_session"
   :function #'gptelt-shell-kill-shell-command-session
   :async t
   :description (concat "Kill a running detached session by sending a termination signal.\n\n"
                        "Only works for active sessions. Use get_session_info first to verify the session is active.\n\n"
                        "IMPORTANT: This sends SIGTERM to the process. The session will transition to inactive state.")
   :args '((:name "session_id" :type string
            :description "The session ID of the session to kill"))
   :category "shell"
   :confirm nil
   :include t)

  (gptelt-make-tool
   :name "list_shell_command_sessions"
   :function #'gptelt-shell-list-shell-command-sessions
   :async t
   :description (concat "List all detached sessions or only active ones.\n\n"
                        "Returns a table showing:\n"
                        "- Session ID\n"
                        "- State (active/inactive/unknown)\n"
                        "- Status (success/failure)\n"
                        "- Host\n"
                        "- Command (truncated to 60 chars)\n\n"
                        "Use this to discover existing sessions or monitor active commands.")
   :args '((:name "active_only" :type boolean :optional t
            :description "If true, only list active (running) sessions"))
   :category "shell"
   :confirm nil
   :include t))

;;; shell.el ends here
