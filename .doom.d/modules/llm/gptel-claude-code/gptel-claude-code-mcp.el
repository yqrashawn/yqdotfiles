;;; gptel-claude-code-mcp.el --- MCP session routing for Claude Code  -*- lexical-binding: t; -*-

;; Author: yqrashawn
;; Keywords: tools, convenience

;;; Commentary:

;; Provides:
;; 1. Session map (session-id → buffer) and session-cwd lookup.
;; 2. MCP config JSON builder for the --mcp-config CLI arg.
;; 3. Session-aware default-directory function for MCP tool calls.
;;
;; The session-scoped HTTP endpoint (/mcp/v1/sessions/{id}/messages) lives
;; in mcp-server-lib-http.el.  This module provides the session-id → cwd
;; resolution via `mcp-server-lib-default-directory-function'.

;;; Code:

(require 'mcp-server-lib)
(require 'mcp-server-lib-http)


;;; ---- Session Map ----

(defvar gptel-claude-code--session-map (make-hash-table :test 'equal)
  "Hash-table mapping session-id to gptel buffer.")

(defun gptel-claude-code--register-session (session-id buffer)
  "Register SESSION-ID as belonging to BUFFER."
  (when (and session-id buffer)
    (puthash session-id buffer gptel-claude-code--session-map)))

(defun gptel-claude-code--session-buffer (session-id)
  "Return the gptel buffer for SESSION-ID, or nil."
  (gethash session-id gptel-claude-code--session-map))

(defun gptel-claude-code--session-cwd (session-id)
  "Return the working directory for SESSION-ID, or nil.
Looks up the buffer associated with SESSION-ID and returns
its buffer-local `gptel-claude-code--team-cwd' value."
  (when-let* ((buf (gptel-claude-code--session-buffer session-id)))
    (when (buffer-live-p buf)
      (buffer-local-value 'gptel-claude-code--team-cwd buf))))

;;; ---- MCP Config JSON Builder ----

(defun gptel-claude-code--mcp-config-json (session-id port)
  "Build MCP config JSON string for --mcp-config.
SESSION-ID is the Claude Code session identifier.
PORT is the MCP HTTP server port."
  (format "{\"mcpServers\":{\"emacs\":{\"type\":\"http\",\"url\":\"http://localhost:%d/mcp/v1/sessions/%s/messages\"}}}"
          port session-id))

;;; ---- Session-aware default-directory ----

(defun gptel-claude-code--mcp-default-directory (session-id)
  "Return the working directory for SESSION-ID, or nil.
Intended as the value of `mcp-server-lib-default-directory-function'.
SESSION-ID is passed by `mcp-server-lib--handle-tools-call-apply'
from `mcp-server-lib--request-session-id'."
  (when session-id
    (gptel-claude-code--session-cwd session-id)))

;; Example: Set session-aware default-directory for MCP tool calls
;; (setq mcp-server-lib-default-directory-function
;;       #'gptel-claude-code--mcp-default-directory)

(provide 'gptel-claude-code-mcp)
;;; gptel-claude-code-mcp.el ends here
