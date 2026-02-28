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
(require 'mcp-server-lib)
(require 'mcp-server-lib-http)
(require 'simple-httpd)

;; Forward declarations
(declare-function mcp-server-lib-http--log "mcp-server-lib-http")
(declare-function mcp-server-lib-http--send-error "mcp-server-lib-http")
(declare-function mcp-server-lib-http--send-response "mcp-server-lib-http")
(declare-function mcp-server-lib-process-jsonrpc "mcp-server-lib")
(declare-function mcp-server-lib-register-tool "mcp-server-lib")


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
      (cond
       ((string= method "OPTIONS")
        (with-temp-buffer
          (httpd-send-header proc "text/plain" 204
                             :Access-Control-Allow-Origin "*")))
       ((string= method "POST")
        (if (string-empty-p body)
            (mcp-server-lib-http--send-error proc 400 "Empty request body")
          (make-thread
           (lambda ()
             (let ((gptel-claude-code--current-session session-id))
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
                                    (error-message-string err)))))))
           "mcp-session-handler")))
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

(defun gptel-claude-code--handle-permission-prompt (type title message &optional options)
  "Handle permission_prompt tool call.
TYPE is the type of prompt.
TITLE is the title of the prompt.
MESSAGE is the full message/description.
OPTIONS is an optional list of available choices."
  (let ((result nil)
        (done nil)
        (mutex (make-mutex "permission-prompt"))
        (condvar (make-condition-variable mutex "permission-prompt-cv")))
    ;; Schedule UI display on the main thread
    (run-at-time 0 nil
                 (lambda ()
                   (gptel-claude-code--show-permission-popup
                    title message
                    (if (vectorp options) (append options nil) options)
                    (lambda (selected)
                      (with-mutex mutex
                        (setq result selected
                              done t)
                        (condition-notify condvar))))))
    ;; Wait for user response (with timeout)
    (with-mutex mutex
      (let ((deadline (+ (float-time) 300))) ; 5 minute timeout
        (while (and (not done) (< (float-time) deadline))
          (condition-wait condvar 1.0))))
    ;; Return result
    (or result "cancelled")))

(defun gptel-claude-code--show-permission-popup (title message options callback)
  "Show permission popup with TITLE, MESSAGE, OPTIONS.
Call CALLBACK with the selected option string, or nil if cancelled."
  (let ((buf (get-buffer-create gptel-claude-code--permission-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (or title "Permission Request") 'face 'bold) "\n\n")
        (when (and message (not (string-empty-p message)))
          (insert message "\n\n"))
        (when options
          (cl-loop for opt in options
                   for i from 1
                   do (insert (format "  %d. %s\n" i opt))))
        (insert "\n  q. Cancel\n"))
      ;; Set up keymap
      (use-local-map (make-sparse-keymap))
      (when options
        (cl-loop for opt in options
                 for i from 1
                 when (<= i 9)
                 do (let ((o opt))
                      (local-set-key (kbd (number-to-string i))
                                     (lambda () (interactive)
                                       (funcall callback o)
                                       (quit-window t))))))
      (local-set-key (kbd "q")
                     (lambda () (interactive)
                       (funcall callback nil)
                       (quit-window t)))
      (local-set-key (kbd "C-g")
                     (lambda () (interactive)
                       (funcall callback nil)
                       (quit-window t)))
      (setq buffer-read-only t))
    (pop-to-buffer buf
                   '((display-buffer-at-bottom)
                     (window-height . fit-window-to-buffer)))))

;;; ---- Tool Registration ----

(defun gptel-claude-code-mcp-register-tools ()
  "Register the permission_prompt MCP tool with mcp-server-lib."
  (mcp-server-lib-register-tool
   #'gptel-claude-code--handle-permission-prompt
   :id "permission_prompt"
   :description "Handle permission prompts and user questions from Claude Code"
   :args (list '(:name "type"
                 :type string
                 :description "Type of prompt")
               '(:name "title"
                 :type string
                 :description "Title of the prompt")
               '(:name "message"
                 :type string
                 :description "Full message/description")
               '(:name "options"
                 :type array
                 :description "Available choices"
                 :optional t
                 :items (:type "string")))))

;;; ---- Auto-register on load ----

;; Register MCP tools when this module is loaded, so the permission_prompt
;; tool is available before Claude Code tries to use it.
(gptel-claude-code-mcp-register-tools)

(provide 'gptel-claude-code-mcp)
;;; gptel-claude-code-mcp.el ends here
