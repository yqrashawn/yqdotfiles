;;; gptel-claude-code-stream.el --- Stream-JSON parser for Claude Code CLI  -*- lexical-binding: t; -*-

;; Author: yqrashawn
;; Keywords: tools, convenience

;;; Commentary:

;; Parses Claude Code CLI stream-json output (newline-delimited JSON).
;; Each line is a JSON object with a "type" field that determines how
;; it should be handled.  The parser dispatches to type-specific handlers
;; that extract text deltas, reasoning, tool use, and session metadata.

;;; Code:

(require 'cl-lib)
(require 'gptel-claude-code-display)

;; Forward declarations -- session module
(declare-function gptel-claude-code--on-session-init "gptel-claude-code-session")

;; Forward declarations -- MCP module
(declare-function gptel-claude-code--register-session "gptel-claude-code-mcp")

;; Forward declarations -- team module
(declare-function gptel-claude-code--detect-teammate-spawn "gptel-claude-code-team")
(declare-function gptel-claude-code--cleanup-watchers "gptel-claude-code-team")

;; Forward declarations -- gptel core
(defvar gptel--request-alist)
(declare-function gptel-fsm-info "gptel-request")
(declare-function gptel--fsm-transition "gptel-request")
(declare-function gptel-curl--stream-insert-response "gptel")
(declare-function gptel--log "gptel-request")
(defvar gptel-log-level)
;; gptel--json-read-string is a macro from gptel, available via the
;; parent file's (require 'gptel)

;;; Buffer-local state

(defvar-local gptel-claude-code--session-id nil
  "Session ID from the Claude Code init message.
Set when an init system message is received.")

(defvar-local gptel-claude-code--session-model nil
  "Model name from the Claude Code init message.")

;;; ANSI stripping

(defun gptel-claude-code--strip-ansi (string)
  "Remove ANSI escape sequences from STRING."
  (replace-regexp-in-string "\033\\[[0-9;]*[a-zA-Z]" "" string))

;;; Message handlers

(defun gptel-claude-code--handle-system (msg info)
  "Handle a system-type message MSG with request INFO.
Extracts session_id from init messages and triggers FSM transition."
  (when (equal (plist-get msg :subtype) "init")
    (let ((session-id (plist-get msg :session_id))
          (model (plist-get msg :model))
          (buf (plist-get info :buffer)))
      ;; Store session info in the chat buffer
      (when (and buf (buffer-live-p buf))
        (with-current-buffer buf
          (setq gptel-claude-code--session-id session-id)
          ;; Store the gptel model name (not Claude Code's full model ID)
          ;; so that it matches (gptel--model-name gptel-model) in session
          ;; state comparisons.
          (setq gptel-claude-code--session-model
                (gptel--model-name gptel-model)))
        ;; Save session as org property for multi-turn/branching
        (gptel-claude-code--on-session-init session-id model buf)
        ;; Register session-id -> buffer mapping for MCP routing
        (gptel-claude-code--register-session session-id buf))
      ;; Mark HTTP status as OK -- this signals to the sentinel that
      ;; the process produced valid output
      (plist-put info :http-status "200")
      ;; Trigger FSM transition (mirrors curl backend's behavior when
      ;; HTTP 200 status line is found)
      (when-let* ((process (plist-get info :claude-code-process))
                  (entry (alist-get process gptel--request-alist))
                  (fsm (car entry)))
        (gptel--fsm-transition fsm))))
  nil)

(defun gptel-claude-code--handle-stream-event (msg info)
  "Handle a stream_event-type message MSG with request INFO.
Dispatches on event type to extract text deltas and reasoning."
  (let* ((event (plist-get msg :event))
         (event-type (plist-get event :type)))
    (pcase event-type
      ("content_block_start"
       (let* ((cblock (plist-get event :content_block))
              (block-type (plist-get cblock :type)))
         (plist-put info :claude-code-current-block block-type)
         (pcase block-type
           ("tool_use"
            (let ((name (plist-get cblock :name))
                  (id (plist-get cblock :id)))
              ;; Push new tool-use entry onto :claude-code-tools list
              ;; NOTE: We use :claude-code-tools (not :tool-use) to avoid
              ;; triggering gptel's built-in tool execution FSM.  Claude Code
              ;; handles all tool execution internally.
              (plist-put info :claude-code-tools
                         (cons (list :id id :name name)
                               (plist-get info :claude-code-tools)))
              ;; Return tool header for display (include tool_use_id)
              (gptel-claude-code--format-tool-use-header name id)))
           ("thinking"
            ;; Mark reasoning block as active; initial thinking text
            ;; (if any) will arrive via thinking_delta events
            (plist-put info :reasoning-block 'in)
            (when-let* ((thinking (plist-get cblock :thinking)))
              (plist-put info :reasoning thinking))
            nil)
           (_ nil))))

      ("content_block_delta"
       (let* ((delta (plist-get event :delta))
              (delta-type (plist-get delta :type)))
         (pcase delta-type
           ("text_delta"
            ;; Return text for display -- the callback will insert it
            (plist-get delta :text))

           ("thinking_delta"
            ;; Store reasoning for gptel's reasoning display mechanism
            (plist-put info :reasoning (plist-get delta :thinking))
            nil)

           ("input_json_delta"
            ;; Accumulate partial JSON for tool-use arguments
            (plist-put info :partial_json
                       (cons (plist-get delta :partial_json)
                             (plist-get info :partial_json)))
            nil)

           ("signature_delta"
            ;; Ignore signature deltas
            nil)

           (_ nil))))

      ("content_block_stop"
       (let ((current-block (plist-get info :claude-code-current-block))
             (display-str nil))
         (cond
          ;; End of tool_use block: combine partial JSON into args
          ((plist-get info :partial_json)
           (condition-case nil
               (let* ((args-json (apply #'concat
                                        (nreverse (plist-get info :partial_json))))
                      (args-decoded
                       (if (string-empty-p args-json)
                           nil
                         (gptel--json-read-string args-json)))
                      (tool-entry (car (plist-get info :claude-code-tools))))
                 (plist-put tool-entry :input args-decoded)
                 ;; Format the tool input and closing tag for display
                 (let* ((name (plist-get tool-entry :name))
                        (formatted-input
                         (gptel-claude-code--format-tool-input name args-decoded)))
                   (setq display-str
                         (concat
                          (unless (string-empty-p formatted-input)
                            (concat formatted-input "\n"))
                          (gptel-claude-code--format-tool-use-footer)))))
             (error (pop (plist-get info :claude-code-tools))))
           (plist-put info :partial_json nil))

          ;; End of tool_use block with no accumulated JSON (empty args)
          ((equal current-block "tool_use")
           (setq display-str (gptel-claude-code--format-tool-use-footer)))

          ;; End of thinking block: signal end of reasoning stream
          ((equal current-block "thinking")
           (when (eq (plist-get info :reasoning-block) 'in)
             (plist-put info :reasoning-block t))))

         (plist-put info :claude-code-current-block nil)
         ;; Flush any pending tool results that were queued while
         ;; a tool_use block was open (parallel tool call interleaving).
         (when-let* ((pending (plist-get info :claude-code-pending-results)))
           (setq display-str (concat (or display-str "") pending))
           (plist-put info :claude-code-pending-results nil))
         display-str))

      ;; message_start, message_delta, message_stop: ignore
      (_ nil))))

(defun gptel-claude-code--handle-assistant (msg info)
  "Handle an assistant-type message MSG with request INFO.
Extracts thinking content for reasoning display.  Tool use blocks
are tracked via :claude-code-tools (set by the stream_event handler)
for display purposes only -- Claude Code executes all tools internally."
  (when-let* ((message (plist-get msg :message))
              (content (plist-get message :content)))
    ;; content is a vector of content blocks
    (cl-loop for cblock across content
             for ctype = (plist-get cblock :type)
             do (pcase ctype
                  ("thinking"
                   ;; Store full thinking text for potential later use
                   (when-let* ((thinking (plist-get cblock :thinking)))
                     (plist-put info :partial_reasoning
                                (concat (plist-get info :partial_reasoning)
                                        thinking)))))))
  nil)

(defun gptel-claude-code--handle-user (msg info)
  "Handle a user-type message MSG with request INFO.
Formats tool_result content blocks for display.
Also detects teammate spawn events from toolUseResult fields."
  ;; Check for teammate spawn events (top-level toolUseResult field)
  (let ((spawn-str (gptel-claude-code--detect-teammate-spawn msg info))
        (tool-str nil))
    ;; Process tool_result content blocks
    (when-let* ((message (plist-get msg :message))
                (content (plist-get message :content)))
      ;; content is a vector of content blocks
      (let ((result-parts nil))
        (cl-loop for cblock across content
                 for ctype = (plist-get cblock :type)
                 do (pcase ctype
                      ("tool_result"
                       (let* ((tool-use-id (plist-get cblock :tool_use_id))
                              (is-error (eq (plist-get cblock :is_error) t))
                              ;; Find the tool name from recorded tool entries
                              (tool-entry
                               (cl-find-if
                                (lambda (tu) (equal (plist-get tu :id) tool-use-id))
                                (plist-get info :claude-code-tools)))
                              (name (or (plist-get tool-entry :name) "unknown"))
                              ;; Extract result content -- can be a string or vector
                              (result-content (plist-get cblock :content))
                              (result-text
                               (cond
                                ((stringp result-content) result-content)
                                ((vectorp result-content)
                                 (mapconcat
                                  (lambda (rc)
                                    (or (plist-get rc :text) ""))
                                  result-content "\n"))
                                (t ""))))
                         (push (gptel-claude-code--format-tool-result
                                name result-text is-error tool-use-id)
                               result-parts)))))
        (when result-parts
          (setq tool-str (apply #'concat (nreverse result-parts))))))
    ;; Combine spawn notification with tool results
    (let ((combined (cond
                     ((and spawn-str tool-str) (concat spawn-str tool-str))
                     (spawn-str spawn-str)
                     (tool-str tool-str))))
      (if (and combined
               (equal (plist-get info :claude-code-current-block) "tool_use"))
          ;; A tool_use block is currently being streamed.  Queue this result
          ;; so it gets inserted AFTER the #+end_tool line, not inside it.
          (progn
            (plist-put info :claude-code-pending-results
                       (concat (or (plist-get info :claude-code-pending-results) "")
                               combined))
            nil)
        combined))))

(defun gptel-claude-code--handle-result (msg info)
  "Handle a result-type message MSG with request INFO.
Extracts usage statistics, error messages, and stores them.
For error results received without a prior init message, triggers
the FSM transition that handle-system would normally perform so
that the sentinel can properly advance the FSM to ERRS."
  (let ((usage (plist-get msg :usage)))
    (when usage
      (plist-put info :output-tokens (plist-get usage :output_tokens))
      (plist-put info :input-tokens (plist-get usage :input_tokens))))
  (plist-put info :stop-reason
             (if (equal (plist-get msg :subtype) "success")
                 "end_turn"
               (or (plist-get msg :subtype) "error")))
  ;; Extract error information from the errors array
  (when (eq (plist-get msg :is_error) t)
    (let* ((errors (plist-get msg :errors))
           (error-msg (if (and errors (> (length errors) 0))
                          (mapconcat #'identity (append errors nil) "\n")
                        (or (plist-get msg :subtype) "Unknown error"))))
      (plist-put info :error error-msg)
      ;; Set :status for gptel--handle-error which uses it for display
      (plist-put info :status (or (plist-get msg :subtype) "error"))
      ;; If no init message was received (no :http-status), we need to
      ;; advance the FSM from WAIT→TYPE so the sentinel can then
      ;; transition TYPE→ERRS.  Without this, the sentinel's single
      ;; transition only goes WAIT→TYPE, leaving "Typing..." stuck.
      (unless (plist-get info :http-status)
        (plist-put info :http-status "200")
        (when-let* ((process (plist-get info :claude-code-process))
                    (entry (alist-get process gptel--request-alist))
                    (fsm (car entry)))
          (gptel--fsm-transition fsm)))
      ;; Insert error into buffer wrapped in #+begin_error/#+end_error
      (let ((callback (or (plist-get info :callback)
                          #'gptel-curl--stream-insert-response))
            (buf (plist-get info :buffer)))
        (when (and callback buf (buffer-live-p buf))
          (let ((error-block
                 (with-current-buffer buf
                   (concat
                    (if (derived-mode-p 'org-mode)
                        "\n#+begin_error\n" "\n``` error\n")
                    error-msg
                    (if (derived-mode-p 'org-mode)
                        "\n#+end_error" "\n```")))))
            ;; Mark entire block as gptel ignore so it's not sent to LLM
            (add-text-properties 0 (length error-block)
                                 '(gptel ignore front-sticky (gptel))
                                 error-block)
            ;; Use raw=t to preserve our properties (skip gptel response tagging)
            (funcall callback error-block info t))))))
  nil)

(defvar gptel-claude-code--message-handlers
  `(("system"       . gptel-claude-code--handle-system)
    ("stream_event" . gptel-claude-code--handle-stream-event)
    ("assistant"    . gptel-claude-code--handle-assistant)
    ("user"         . gptel-claude-code--handle-user)
    ("result"       . gptel-claude-code--handle-result))
  "Alist mapping Claude Code message type strings to handler functions.
Each handler takes (MSG INFO) and returns a string to display, or nil.")

;;; Block folding

(defun gptel-claude-code--fold-blocks-in-region (info start)
  "Fold tool/result org blocks inserted after buffer position START.
INFO is the request info plist containing :buffer and :tracking-marker.
Searches for #+end_tool and #+end_result lines between START and
the tracking marker, folding each with `org-cycle'."
  (when-let* ((buf (plist-get info :buffer))
              (tracking-marker (plist-get info :tracking-marker)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (derived-mode-p 'org-mode)
          (ignore-errors
            (save-excursion
              (goto-char start)
              (while (re-search-forward
                      "^#\\+end_\\(tool\\|result\\)"
                      tracking-marker t)
                (let ((resume-pos (point)))
                  (forward-line 0)
                  (org-cycle)
                  (goto-char resume-pos))))))))))

;;; Stream filter

(defun gptel-claude-code--stream-filter (process output)
  "Process filter for Claude Code CLI streaming output.

Accumulates OUTPUT in PROCESS's buffer, then parses complete
JSON lines and dispatches to the appropriate handler.  Text
deltas are forwarded to the gptel callback for display."
  (when (buffer-live-p (process-buffer process))
    (let* ((entry (alist-get process gptel--request-alist))
           (fsm (car entry))
           (proc-info (gptel-fsm-info fsm))
           (callback (or (plist-get proc-info :callback)
                         #'gptel-curl--stream-insert-response)))
      ;; Store process reference in info so handlers can find the FSM
      (unless (plist-get proc-info :claude-code-process)
        (plist-put proc-info :claude-code-process process))
      (with-current-buffer (process-buffer process)
        ;; Insert raw output
        (save-excursion
          (goto-char (process-mark process))
          (insert output)
          (set-marker (process-mark process) (point)))

        ;; Initialize parse position if needed
        (unless (plist-get proc-info :claude-code-parse-pos)
          (plist-put proc-info :claude-code-parse-pos (point-min)))

        ;; Parse complete JSON lines
        (let ((parse-pos (plist-get proc-info :claude-code-parse-pos)))
          (save-excursion
            (goto-char parse-pos)
            (while (search-forward "\n" nil t)
              (let* ((line-end (point))
                     (line-start parse-pos)
                     (line (buffer-substring-no-properties line-start line-end))
                     (line (string-trim (gptel-claude-code--strip-ansi line))))
                (setq parse-pos line-end)
                (when (and (not (string-empty-p line))
                           (eq (aref line 0) ?{))  ; Quick JSON check
                  (condition-case err
                      (let* ((msg (gptel--json-read-string line))
                             (msg-type (plist-get msg :type))
                             (handler (cdr (assoc msg-type
                                                  gptel-claude-code--message-handlers))))
                        (when handler
                          (let ((response (funcall handler msg proc-info)))
                            ;; Handle reasoning display (same logic as curl filter)
                            (let ((reasoning (plist-get proc-info :reasoning))
                                  (reasoning-block (plist-get proc-info :reasoning-block)))
                              (when (stringp reasoning)
                                (funcall callback (cons 'reasoning reasoning) proc-info)
                                (unless reasoning-block
                                  (plist-put proc-info :reasoning-block 'in))
                                (plist-put proc-info :reasoning nil))
                              (when (eq reasoning-block t)
                                (funcall callback '(reasoning . t) proc-info)
                                (plist-put proc-info :reasoning-block 'done)))
                            ;; Forward text to callback
                            (when (and response (stringp response)
                                       (not (string-empty-p response)))
                              ;; Track partial text for tool-use message construction
                              (plist-put proc-info :partial_text
                                         (cons response
                                               (plist-get proc-info :partial_text)))
                              (let ((pre-pos
                                     (if-let* ((tm (plist-get proc-info :tracking-marker)))
                                         (marker-position tm)
                                       (marker-position (plist-get proc-info :position)))))
                                (funcall callback response proc-info)
                                ;; Auto-fold completed tool/result blocks
                                (when (string-match-p "#\\+end_\\(tool\\|result\\)" response)
                                  (gptel-claude-code--fold-blocks-in-region
                                   proc-info pre-pos)))))))
                    (error
                     (when (eq gptel-log-level 'debug)
                       (gptel--log (format "Claude Code parse error: %S\nLine: %s"
                                           err line)
                                   "parse error" 'no-json))))))))
          (plist-put proc-info :claude-code-parse-pos parse-pos))))))

;;; Stream sentinel (cleanup)

(defun gptel-claude-code--stream-cleanup (process _status)
  "Process sentinel for Claude Code CLI requests.

Cleans up PROCESS, signals completion to the callback,
advances the FSM, and stops any teammate transcript watchers."
  (let ((proc-buf (process-buffer process)))
    (when-let* ((entry (alist-get process gptel--request-alist))
                (fsm (car entry))
                (info (gptel-fsm-info fsm)))
      ;; Use unwind-protect to guarantee cleanup even if FSM transition
      ;; handlers error (e.g. gptel--handle-error, gptel--parse-tool-results).
      ;; Without this, stale entries accumulate in gptel--request-alist and
      ;; the "Typing..." header-line status persists forever.
      (unwind-protect
          (progn
            ;; Cancel timeout timer if set
            (when-let* ((timer (plist-get info :claude-code-timer)))
              (cancel-timer timer)
              (plist-put info :claude-code-timer nil))
            (if (plist-get info :http-status)
                ;; Success path: we received an init message
                (with-demoted-errors "gptel callback error: %S"
                  (funcall (plist-get info :callback) t info))
              ;; Failure path: process ended without init message
              (unless (plist-get info :error)
                (plist-put info :error
                           (format "Claude Code process exited with status: %s"
                                   (process-exit-status process))))
              (with-demoted-errors "gptel callback error: %S"
                (funcall (plist-get info :callback) nil info)))
            ;; Clean up teammate transcript watchers
            (when-let* ((buf (plist-get info :buffer)))
              (when (buffer-live-p buf)
                (gptel-claude-code--cleanup-watchers buf)))
            ;; IMPORTANT: Clear :tool-use to prevent the FSM from entering TOOL
            ;; state.  Claude Code handles all tool execution internally via MCP.
            ;; If :tool-use somehow got set (e.g. by gptel core detecting tool_use
            ;; content blocks), clearing it here ensures TYPE -> DONE, not TYPE -> TOOL.
            (plist-put info :tool-use nil)
            (with-demoted-errors "gptel Claude Code FSM transition error: %S"
              (gptel--fsm-transition fsm)))
        ;; CLEANUP (always runs): remove from request alist
        (setf (alist-get process gptel--request-alist nil 'remove) nil)))
    ;; Kill the process buffer
    (when (buffer-live-p proc-buf)
      (kill-buffer proc-buf))))

(provide 'gptel-claude-code-stream)
;;; gptel-claude-code-stream.el ends here
