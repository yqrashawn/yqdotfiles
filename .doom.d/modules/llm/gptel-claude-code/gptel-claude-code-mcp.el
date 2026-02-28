;;; gptel-claude-code-mcp.el --- MCP session routing & permission prompt for Claude Code  -*- lexical-binding: t; -*-

;; Author: yqrashawn
;; Keywords: tools, convenience

;;; Commentary:

;; Provides:
;; 1. Session-aware MCP HTTP routing so Claude Code's MCP tool calls
;;    are routed to the correct gptel buffer.
;; 2. A permission_prompt MCP tool that shows a popup buffer when
;;    Claude Code asks the user a question.
;; 3. MCP config JSON builder for --mcp-config and --permission-prompt-tool
;;    CLI args.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'mcp-server-lib)
(require 'mcp-server-lib-http)
(require 'simple-httpd)

;; Forward declarations
(declare-function mcp-server-lib-http--log "mcp-server-lib-http")
(declare-function mcp-server-lib-http--send-error "mcp-server-lib-http")
(declare-function mcp-server-lib-http--send-response "mcp-server-lib-http")
(declare-function mcp-server-lib-process-jsonrpc "mcp-server-lib")
(declare-function mcp-server-lib-register-tool "mcp-server-lib")
(defvar json-encoding-pretty-print)


;;; ---- Session Routing ----

(defvar gptel-claude-code--current-session nil
  "Dynamic variable holding the current MCP session ID during tool dispatch.")

(defvar gptel-claude-code--session-map (make-hash-table :test 'equal)
  "Hash-table mapping session-id to gptel buffer.")

(defun gptel-claude-code--register-session (session-id buffer)
  "Register SESSION-ID as belonging to BUFFER."
  (when (and session-id buffer)
    (puthash session-id buffer gptel-claude-code--session-map)))

(defun gptel-claude-code--session-buffer (session-id)
  "Return the gptel buffer for SESSION-ID, or nil."
  (gethash session-id gptel-claude-code--session-map))

;;; Session-aware HTTP route

(defun httpd/mcp/v1/sessions (proc uri-path _uri-query request)
  "Handle MCP requests with session routing.
Matches /mcp/v1/sessions/{session-id}/messages.
Extracts the session-id from URI-PATH, binds it dynamically,
then delegates to the standard MCP JSON-RPC processor."
  ;; parts: ("" "mcp" "v1" "sessions" "SESSION-ID" "messages")
  (let* ((parts (split-string uri-path "/"))
         (session-id (nth 4 parts)))
    (mcp-server-lib-http--log "Session MCP request for session: %s" session-id)
    (let* ((method (caar request))
           (content (cadr (assoc "Content" request)))
           (body (or content "")))
      (mcp-server-lib-http--log "Session MCP request for \nsession: %s\nbody:\n %s" session-id body)
      (cond
       ((string= method "OPTIONS")
        (with-temp-buffer
          (httpd-send-header proc "text/plain" 204
                             :Access-Control-Allow-Origin "*")))
       ((string= method "POST")
        (if (string-empty-p body)
            (mcp-server-lib-http--send-error proc 400 "Empty request body")
          ;; Schedule on main thread — same reason as mcp-server-lib-http.el:
          ;; async tools use `recursive-edit' which needs the main thread.
          (let ((sid session-id))
            (run-at-time
             0 nil
             (lambda ()
               (let ((gptel-claude-code--current-session sid))
                 (condition-case err
                     (let ((response (mcp-server-lib-process-jsonrpc body)))
                       (if response
                           (mcp-server-lib-http--send-response proc response)
                         (with-temp-buffer
                           (httpd-send-header proc "text/plain" 204
                                              :Access-Control-Allow-Origin "*"))))
                   (error
                    (mcp-server-lib-http--send-error
                     proc 500 (format "Internal error: %s"
                                      (error-message-string err)))))))))))
       (t (mcp-server-lib-http--send-error proc 405 "Method not allowed"))))))

;;; ---- MCP Config JSON Builder ----

(defun gptel-claude-code--mcp-config-json (session-id port)
  "Build MCP config JSON string for --mcp-config.
SESSION-ID is the Claude Code session identifier.
PORT is the MCP HTTP server port."
  (format "{\"mcpServers\":{\"emacs\":{\"type\":\"http\",\"url\":\"http://localhost:%d/mcp/v1/sessions/%s/messages\"}}}"
          port session-id))

;;; ---- Permission Prompt Tool ----

(defvar gptel-claude-code--permission-buffer-name "*gptel-permission*"
  "Buffer name for the permission prompt popup.")

(defvar gptel-claude-code--permission-pending nil
  "Pending permission prompt state for deferred answering.
When non-nil, a plist with:
  :tool-name      - name of the tool requesting permission
  :tool-input     - original tool input alist (for updatedInput in allow response)
  :input-summary  - human-readable summary of tool input
  :callback       - the MCP async callback")

(defvar gptel-claude-code--permission-in-recursive-edit nil
  "Non-nil when inside a `recursive-edit' for permission prompt.
Used to prevent nested recursive-edits.")

(defun gptel-claude-code--format-tool-input-summary (tool-name tool-input)
  "Format a human-readable summary of TOOL-INPUT for TOOL-NAME."
  (cond
   ((and (equal tool-name "Bash")
         (plist-get tool-input :command))
    (format "$ %s" (plist-get tool-input :command)))
   ((and (member tool-name '("Write" "Read" "Edit"))
         (plist-get tool-input :file_path))
    (plist-get tool-input :file_path))
   (tool-input
    (let ((json-encoding-pretty-print t))
      (condition-case nil
          (json-encode tool-input)
        (error (format "%S" tool-input)))))
   (t "")))

(defun gptel-claude-code--permission-build-response (behavior &optional tool-input)
  "Build the JSON response string for permission BEHAVIOR and TOOL-INPUT.
BEHAVIOR is \"allow\" or \"deny\".
TOOL-INPUT is the original tool input alist — required for \"allow\",
ignored for \"deny\".

The MCP tool response is parsed directly by Claude Code as a flat object.
For allow: {behavior: \"allow\", updatedInput: {...original tool input...}}
For deny:  {behavior: \"deny\", message: \"reason\"}
No hookSpecificOutput wrapping — that format is for shell hooks, not MCP tools."
  (json-encode
   (if (string= behavior "allow")
       `((behavior . "allow")
         (updatedInput . ,(or tool-input (make-hash-table))))
     `((behavior . "deny")
       (message . "Denied by user")))))

(defun gptel-claude-code--permission-respond (allowed)
  "Respond to the pending permission prompt.
ALLOWED is non-nil to allow, nil to deny.
Calls the stored MCP async callback, then `exit-recursive-edit'
to return control to the handler."
  (unless gptel-claude-code--permission-pending
    (user-error "No pending permission prompt"))
  (let ((callback (plist-get gptel-claude-code--permission-pending :callback))
        (tool-input (plist-get gptel-claude-code--permission-pending :tool-input))
        (behavior (if allowed "allow" "deny")))
    (setq gptel-claude-code--permission-pending nil)
    (gptel-claude-code--cleanup-permission-buffer)
    (message "Permission %s" (if allowed "allowed" "denied"))
    (funcall callback
             (gptel-claude-code--permission-build-response behavior tool-input))
    ;; exit-recursive-edit throws (non-local exit), so this must be last.
    (when gptel-claude-code--permission-in-recursive-edit
      (exit-recursive-edit))))

(defun gptel-claude-code--permission-allow ()
  "Allow the pending permission request."
  (interactive)
  (gptel-claude-code--permission-respond t))

(defun gptel-claude-code--permission-deny ()
  "Deny the pending permission request."
  (interactive)
  (gptel-claude-code--permission-respond nil))

(defvar gptel-claude-code--permission-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" #'gptel-claude-code--permission-allow)
    (define-key map "d" #'gptel-claude-code--permission-deny)
    (define-key map "q" #'gptel-claude-code--permission-deny)
    map)
  "Keymap for the permission prompt buffer.")

(define-minor-mode gptel-claude-code--permission-mode
  "Minor mode for interacting with permission prompt buffers.
Press `a' to allow, `d' or `q' to deny."
  :lighter " Perm"
  :keymap gptel-claude-code--permission-mode-map)

(defun gptel-claude-code--handle-permission-prompt (callback &rest args)
  "Handle permission_prompt tool call from Claude Code.

Claude Code sends a PermissionRequest hook event with fields:
  session_id, transcript_path, cwd, permission_mode,
  hook_event_name, tool_name, tool_input, permission_suggestions.

CALLBACK is the MCP async callback (first arg due to :async t).
ARGS are the parameter values passed positionally by mcp-server-lib.

This handler runs on the main thread (the HTTP route uses
`run-at-time 0' instead of `make-thread').  It shows a popup
buffer and enters `recursive-edit' to block until the user
responds with a/d/q.  `recursive-edit' starts a nested command
loop that processes keyboard input, timers, and network I/O,
keeping Emacs fully responsive.

When the user presses a key, the keybinding handler calls the
callback and `exit-recursive-edit' to return control here."
  (let* ((param-names '(session_id transcript_path cwd permission_mode
                        hook_event_name tool_name tool_input
                        permission_suggestions))
         (params (cl-mapcar #'cons param-names args))
         (tool-name (or (cdr (assq 'tool_name params)) "unknown"))
         (tool-input (cdr (assq 'tool_input params)))
         (tool-input-plist
          (when tool-input
            (if (and (consp tool-input) (consp (car tool-input)))
                (cl-loop for (k . v) in tool-input
                         append (list (intern (format ":%s" k)) v))
              tool-input)))
         (input-summary (gptel-claude-code--format-tool-input-summary
                         tool-name tool-input-plist)))
    ;; Store state for keybinding handlers
    (setq gptel-claude-code--permission-pending
          (list :tool-name tool-name
                :tool-input tool-input
                :input-summary input-summary
                :callback callback))
    ;; Show popup directly (we're on the main thread)
    (gptel-claude-code--show-permission-popup tool-name input-summary)
    (message "Permission requested for %s. Press `a' to allow, `d' to deny in *gptel-permission* buffer."
             tool-name)
    ;; Enter recursive-edit — blocks here but keeps Emacs responsive.
    ;; The user can interact freely; pressing a/d/q calls the callback
    ;; and exit-recursive-edit to return control here.
    ;; Guard against nesting (e.g. if ask_user_question is also active).
    (unless gptel-claude-code--permission-in-recursive-edit
      (setq gptel-claude-code--permission-in-recursive-edit t)
      (unwind-protect
          (recursive-edit)
        (setq gptel-claude-code--permission-in-recursive-edit nil)
        ;; If user hit C-g without responding, send deny
        (when gptel-claude-code--permission-pending
          (let ((cb (plist-get gptel-claude-code--permission-pending :callback)))
            (setq gptel-claude-code--permission-pending nil)
            (gptel-claude-code--cleanup-permission-buffer)
            (message "Permission denied (cancelled)")
            (ignore-errors
              (funcall cb (gptel-claude-code--permission-build-response "deny")))))))))

(defun gptel-claude-code--cleanup-permission-buffer ()
  "Kill the permission popup buffer and its window."
  (when-let* ((buf (get-buffer gptel-claude-code--permission-buffer-name)))
    (when-let* ((win (get-buffer-window buf)))
      (delete-window win))
    (kill-buffer buf)))

(defun gptel-claude-code--show-permission-popup (tool-name input-summary)
  "Show permission popup for TOOL-NAME with INPUT-SUMMARY.
Uses `display-buffer' (not `pop-to-buffer') to avoid selecting
the buffer from a thread context."
  (let ((buf (get-buffer-create gptel-claude-code--permission-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Permission Request" 'face 'bold) "\n\n")
        (insert (format "Claude Code wants to use: %s\n\n"
                        (propertize tool-name 'face 'font-lock-function-name-face)))
        (when (and input-summary (not (string-empty-p input-summary)))
          (insert input-summary "\n\n"))
        (insert (propertize "Keys: " 'face 'font-lock-comment-face)
                (propertize "[a]" 'face 'bold) " allow  "
                (propertize "[d]" 'face 'bold) " deny  "
                (propertize "[q]" 'face 'bold) " deny (cancel)\n"))
      (setq buffer-read-only t)
      (goto-char (point-min))
      (gptel-claude-code--permission-mode 1))
    (display-buffer buf
                    '((display-buffer-reuse-window display-buffer-below-selected)
                      (window-height . fit-window-to-buffer)))))

;;; ---- Tool Registration ----

(defun gptel-claude-code-mcp-register-tools ()
  "Register the permission_prompt MCP tool with mcp-server-lib."
  (mcp-server-lib-register-tool
   #'gptel-claude-code--handle-permission-prompt
   :id "permission_prompt"
   :async t
   :description "Handle permission prompts from Claude Code. Shows a popup for user to allow or deny tool execution."
   :args (list '(:name "session_id"
                 :type string
                 :description "Claude Code session identifier")
               '(:name "transcript_path"
                 :type string
                 :description "Path to the session transcript file")
               '(:name "cwd"
                 :type string
                 :description "Current working directory")
               '(:name "permission_mode"
                 :type string
                 :description "Current permission mode")
               '(:name "hook_event_name"
                 :type string
                 :description "Hook event name (PermissionRequest)")
               '(:name "tool_name"
                 :type string
                 :description "Name of the tool requesting permission")
               '(:name "tool_input"
                 :type object
                 :description "The tool input parameters")
               '(:name "permission_suggestions"
                 :type array
                 :description "Suggested permission options"
                 :optional t))))

;;; ---- Auto-register on load ----

;; Register MCP tools when this module is loaded, so the permission_prompt
;; tool is available before Claude Code tries to use it.

(gptel-claude-code-mcp-register-tools)
(add-hook 'gptel-mode-hook 'gptel-claude-code-mcp-register-tools)

(provide 'gptel-claude-code-mcp)
;;; gptel-claude-code-mcp.el ends here
