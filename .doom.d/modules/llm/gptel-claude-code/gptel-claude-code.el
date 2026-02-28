;;; gptel-claude-code.el --- Claude Code CLI backend for gptel  -*- lexical-binding: t; -*-

;; Author: yqrashawn
;; Keywords: tools, convenience
;; Package-Requires: ((emacs "28.1") (gptel "0.9"))

;;; Commentary:

;; A gptel backend that talks to the Claude Code CLI instead of an HTTP API.
;; Claude Code is spawned as a subprocess with --print mode, communicating
;; via stdin/stdout JSON messages rather than HTTP requests.

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'org-id)

;;; Customization group

(defgroup gptel-claude-code nil
  "Claude Code CLI backend for gptel."
  :group 'gptel)

;;; User options

(defcustom gptel-claude-code-timeout 300
  "Timeout in seconds for Claude Code queries.
Set to nil to disable timeout."
  :type '(choice (integer :tag "Seconds")
          (const :tag "No timeout" nil))
  :group 'gptel-claude-code)

(defcustom gptel-claude-code-skip-permissions nil
  "When non-nil, pass --dangerously-skip-permissions to Claude Code.
This bypasses all permission checks in Claude Code."
  :type 'boolean
  :group 'gptel-claude-code)

;; Forward declarations -- gptel core
(defvar gptel--known-backends)
(defvar gptel--request-alist)
(defvar gptel-model)
(defvar gptel--system-message)
(defvar gptel-org-convert-response)
(defvar gptel-log-level)
(defvar gptel-mode)
(defvar gptel-track-response)
(defvar gptel-track-media)
(defvar gptel-context)
(defvar gptel-use-context)
(declare-function gptel--process-models "gptel-openai")
(declare-function gptel--model-name "gptel")
(declare-function gptel--log "gptel-request")
(declare-function gptel-fsm-info "gptel-request")
(declare-function gptel--fsm-transition "gptel-request")
(declare-function gptel--insert-response "gptel")
(declare-function gptel-curl--stream-insert-response "gptel")
(declare-function gptel--stream-convert-markdown->org "gptel-org")
(declare-function gptel--trim-prefixes "gptel")
(declare-function gptel--parse-media-links "gptel")
(declare-function gptel--model-capable-p "gptel")
(declare-function gptel--base64-encode "gptel")
(declare-function gptel-context--collect-media "gptel-context")
;; gptel--temp-buffer is a macro from gptel-request, available via (require 'gptel)

;; Forward declarations -- team module buffer-local vars
(defvar gptel-claude-code--team-cwd)

;; Forward declarations -- MCP module
(declare-function gptel-claude-code--mcp-config-json "gptel-claude-code-mcp")

;;; Backend struct

(cl-defstruct (gptel-claude-code (:constructor gptel--make-claude-code)
                                 (:copier nil)
                                 (:include gptel-backend)
                                 (:predicate nil))
  "A gptel backend for the Claude Code CLI.

Spawns Claude Code as a subprocess using --print mode.
Communicates via stdin/stdout JSON messages."
  (claude-command "claude"
                  :documentation "Path or name of the claude CLI executable.")
  (permission-mode "bypassPermissions"
                   :documentation
                   "Permission mode passed to --permission-mode.
One of \"default\", \"acceptEdits\", or \"bypassPermissions\".")
  (default-flags '("--print" "--output-format" "stream-json"
                   "--verbose" "--chrome" "--include-partial-messages"
                   "--teammate-mode" "in-process")
                 :documentation
                 "List of CLI flags always passed to Claude Code.")
  (extra-args nil
              :documentation
              "User-configurable extra CLI arguments (list of strings).")
  (mcp-port 18684
            :documentation "Port for MCP session routing.")
  (cwd-fn #'gptel-claude-code--default-cwd
          :documentation
          "Function (zero-arg) returning the working directory for Claude Code.
Called each time a process is spawned."))

;;; Predicate

(defun gptel-claude-code-p (backend)
  "Return non-nil if BACKEND is a `gptel-claude-code' instance."
  (cl-typep backend 'gptel-claude-code))

;;; Default cwd function

(defun gptel-claude-code--default-cwd ()
  "Return a suitable working directory for Claude Code.

Tries, in order:
1. doom workspace project root
2. `project-root' of current project (if available)
3. `default-directory' as fallback."
  (or
   (and (fboundp 'persp-parameter)
        (persp-parameter '++workspace-project-root))
   (and (fboundp 'project-root)
        (when-let* ((proj (project-current)))
          (project-root proj)))
   default-directory))

;;; Factory function

;;;###autoload
(cl-defun gptel-make-claude-code
    (name &key
          models stream
          claude-command permission-mode
          default-flags extra-args
          mcp-port cwd-fn)
  "Register a Claude Code CLI backend for gptel with NAME.

Keyword arguments:

MODELS is a list of available model names, as symbols.
Each can be a plain symbol or a (symbol . plist) cons like
other gptel backends.

STREAM is a boolean to toggle streaming responses.

CLAUDE-COMMAND (optional) is the path or name of the claude
CLI executable.  Defaults to \"claude\".

PERMISSION-MODE (optional) is the --permission-mode value.
Defaults to \"bypassPermissions\".

DEFAULT-FLAGS (optional) is a list of CLI flags always passed
to the Claude Code process.

EXTRA-ARGS (optional) is a list of additional CLI arguments.

MCP-PORT (optional) is the port for MCP session routing.
Defaults to 18684.

CWD-FN (optional) is a zero-argument function returning
the working directory for spawned Claude Code processes."
  (declare (indent 1))
  (let ((backend (gptel--make-claude-code
                  :name name
                  :host "localhost"
                  :protocol "file"
                  :endpoint ""
                  :stream stream
                  :models (gptel--process-models models)
                  :claude-command (or claude-command "claude")
                  :permission-mode (or permission-mode "bypassPermissions")
                  :default-flags (or default-flags
                                     '("--print"
                                       "--output-format" "stream-json"
                                       "--verbose" "--chrome"
                                       "--include-partial-messages"))
                  :extra-args extra-args
                  :mcp-port (or mcp-port 18684)
                  :cwd-fn (or cwd-fn #'gptel-claude-code--default-cwd))))
    (prog1 backend
      (setf (alist-get name gptel--known-backends
                       nil nil #'equal)
            backend))))

;;; Buffer parsing method

(cl-defmethod gptel--parse-buffer ((_backend gptel-claude-code) &optional max-entries)
  "Parse current buffer and return a list of prompt plists for Claude Code.

MAX-ENTRIES limits how many messages to parse.
Parses conversation history from the buffer, handling both plain
text and media-containing user messages.  Media links (images,
files) are parsed when `gptel-track-media' is non-nil."
  (let ((prompts) (prev-pt (point))
        (include-media (and gptel-track-media (gptel--model-capable-p 'media))))
    (if (or gptel-mode gptel-track-response)
        (while (and (or (not max-entries) (>= max-entries 0))
                    (/= prev-pt (point-min))
                    (goto-char (previous-single-property-change
                                (point) 'gptel nil (point-min))))
          (pcase (get-char-property (point) 'gptel)
            ('response
             (when-let* ((content (gptel--trim-prefixes
                                   (buffer-substring-no-properties (point) prev-pt))))
               (push (list :role "assistant" :content content) prompts)))
            ('ignore)
            ('nil
             (and max-entries (cl-decf max-entries))
             (let ((content
                    (if include-media
                        ;; Parse media links and convert to our format
                        (gptel-claude-code--parse-multipart
                         (gptel--parse-media-links major-mode (point) prev-pt))
                      (gptel--trim-prefixes
                       (buffer-substring-no-properties (point) prev-pt)))))
               (when content
                 (push (list :role "user" :content content) prompts)))))
          (setq prev-pt (point)))
      ;; Not in gptel-mode, just grab all content
      (let ((content (string-trim (buffer-substring-no-properties
                                   (point-min) (point-max)))))
        (push (list :role "user" :content content) prompts)))
    prompts))

;;; Media/file attachment support

(defun gptel-claude-code--parse-multipart (parts)
  "Convert multipart PARTS to a text prompt with file path references.

PARTS is a list of plists as returned by `gptel--parse-media-links':
  ((:text \"some text\")
   (:media \"/path/to/image.png\" :mime \"image/png\")
   (:text \"more text\")
   (:textfile \"/path/to/file.txt\"))

Since Claude Code can read files directly from paths, media files
are referenced by path rather than base64-encoded.  Text file
content is inserted inline.

Returns a string suitable for sending via stdin to Claude Code."
  (let ((text-parts nil)
        (file-refs nil))
    (dolist (part parts)
      (cond
       ;; Plain text content
       ((plist-get part :text)
        (let ((text (gptel--trim-prefixes (plist-get part :text))))
          (when text (push text text-parts))))
       ;; Binary media file (image, PDF, etc.) -- reference by path
       ((plist-get part :media)
        (let ((path (plist-get part :media)))
          (push (format "[Attached file: %s]" (expand-file-name path)) file-refs)))
       ;; Text file -- insert content inline (same as OpenAI behavior)
       ((plist-get part :textfile)
        (let ((path (plist-get part :textfile)))
          (push (format "[Attached file: %s]" (expand-file-name path)) file-refs)))
       ;; URL-based media
       ((plist-get part :url)
        (push (format "[Attached URL: %s]" (plist-get part :url)) file-refs))))
    ;; Combine: file references first, then text content
    (let ((refs (and file-refs (mapconcat #'identity (nreverse file-refs) "\n")))
          (text (and text-parts (mapconcat #'identity (nreverse text-parts) "\n"))))
      (cond
       ((and refs text) (concat refs "\n\n" text))
       (refs refs)
       (text text)))))

(cl-defmethod gptel--inject-media ((_backend gptel-claude-code) prompts)
  "Inject media files from `gptel-context' into PROMPTS for Claude Code.

Media files in `gptel-context' (added via `gptel-add-file' etc.)
are prepended to the first user prompt as file path references.
Claude Code can read files directly, so we include paths rather
than base64-encoding content."
  (when-let* ((media-list (gptel-context--collect-media)))
    ;; media-list entries are (:media "/path/to/file" :mime "image/png")
    (let ((file-refs
           (mapconcat
            (lambda (entry)
              (format "[Attached file: %s]"
                      (expand-file-name (plist-get entry :media))))
            media-list "\n")))
      (cl-callf (lambda (current)
                  (if (stringp current)
                      (concat file-refs "\n\n" current)
                    ;; Should not happen for our backend, but handle gracefully
                    (concat file-refs "\n\n" (format "%s" current))))
          (plist-get (car prompts) :content)))))

;;; Request data method

(cl-defmethod gptel--request-data ((_backend gptel-claude-code) prompts)
  "Return a plist with prompt text and system message for Claude Code CLI.

PROMPTS is a list of plists like (:role \"user\" :content \"text\").
We extract the last user message's content as plain text.

Returns a plist (:prompt TEXT :system-message MSG) so that the
augmented system message (with gptel context injected) is preserved.
This method runs inside the data buffer where `gptel--system-message'
has been modified by `gptel-context--wrap-in-buffer', so we capture
it here before the data buffer is killed.

Content can be:
- A string (normal text, possibly with file references already injected)
- A vector (multipart content from other backends -- extract text and paths)"
  (let* ((last-user-msg
          (cl-find-if (lambda (msg) (equal (plist-get msg :role) "user"))
                      (reverse prompts)))
         (content (or (plist-get last-user-msg :content) ""))
         ;; Capture the augmented system message from data buffer.
         ;; gptel-context--wrap-in-buffer may have prepended context to it.
         (system-msg gptel--system-message))
    ;; Content should already be a string from our gptel--parse-buffer
    ;; and gptel--inject-media, but handle vector gracefully as a safeguard
    (let ((prompt-text
           (if (stringp content)
               content
             ;; Vector content (shouldn't happen with our parse-buffer, but be safe)
             (if (vectorp content)
                 (let ((text-parts nil)
                       (file-parts nil))
                   (cl-loop for part across content
                            do (cond
                                ((equal (plist-get part :type) "text")
                                 (push (plist-get part :text) text-parts))
                                ((equal (plist-get part :type) "image_url")
                                 (when-let* ((url (plist-get
                                                   (plist-get part :image_url) :url)))
                                   (cond
                                    ((string-prefix-p "file://" url)
                                     (push (substring url 7) file-parts))
                                    ((string-prefix-p "data:" url)
                                     ;; Base64 data URI -- write to temp file
                                     (let* ((mime-end (string-search ";" url))
                                            (mime (and mime-end (substring url 5 mime-end)))
                                            (ext (or (and mime (cadr (split-string mime "/")))
                                                     "bin"))
                                            (tmp (make-temp-file "gptel-media-" nil
                                                                 (concat "." ext)))
                                            (b64-start (+ (string-search "," url) 1))
                                            (b64-data (substring url b64-start)))
                                       (with-temp-file tmp
                                         (insert (base64-decode-string b64-data)))
                                       (push tmp file-parts)))
                                    (t (push url file-parts)))))
                                ;; Anthropic-style document
                                ((equal (plist-get part :type) "document")
                                 (when-let* ((source (plist-get part :source))
                                             (data (plist-get source :data)))
                                   (let* ((media-type (plist-get source :media_type))
                                          (ext (or (and media-type
                                                        (cadr (split-string media-type "/")))
                                                   "bin"))
                                          (tmp (make-temp-file "gptel-media-" nil
                                                               (concat "." ext))))
                                     (with-temp-file tmp
                                       (insert (base64-decode-string data)))
                                     (push tmp file-parts))))))
                   ;; Build final prompt
                   (let ((refs (and file-parts
                                    (mapconcat
                                     (lambda (f)
                                       (format "[Attached file: %s]" f))
                                     (nreverse file-parts) "\n")))
                         (text (and text-parts
                                    (mapconcat #'identity
                                               (nreverse text-parts) "\n"))))
                     (cond
                      ((and refs text) (concat refs "\n\n" text))
                      (refs refs)
                      (text text)
                      (t ""))))
               ;; Unknown content type, coerce to string
               (format "%s" content)))))
      ;; Return plist with prompt and augmented system message
      (list :prompt prompt-text :system-message system-msg))))

;;; CLI args builder

(defun gptel-claude-code--effective-session-id (session-state)
  "Return the effective session-id for MCP routing from SESSION-STATE.
This is the session-id that Claude Code will use for the new process."
  (pcase (plist-get session-state :state)
    ((or :new :continue :fork)
     (plist-get session-state :session-id))
    (:fork-model
     (plist-get session-state :new-session-id))))

(defun gptel-claude-code--build-args (info backend)
  "Build CLI argument list from INFO plist and BACKEND.

INFO is the FSM info plist containing :buffer and other request data.
Returns a list of strings suitable for `start-process'.

IMPORTANT: This function must access buffer-local variables from the
chat buffer (gptel-model, gptel--system-message, session state, org
properties).  The WAIT handler may run after the prompt buffer is
killed, leaving the current buffer as some random Emacs buffer.
We use `with-current-buffer' to ensure correct context."
  (let* ((chat-buf (plist-get info :buffer))
         ;; Use model from info plist (captured in gptel--realize-query)
         ;; as a reliable source regardless of current buffer context.
         (model (or (plist-get info :model) gptel-model))
         (model-name (gptel--model-name model))
         (args (copy-sequence (gptel-claude-code-default-flags backend))))
    ;; --model
    (setq args (append args (list "--model" model-name)))
    ;; --permission-mode
    (setq args (append args (list "--permission-mode"
                                  (gptel-claude-code-permission-mode backend))))
    ;; --dangerously-skip-permissions
    (when gptel-claude-code-skip-permissions
      (setq args (append args (list "--dangerously-skip-permissions"))))
    ;; --append-system-prompt
    ;; Prefer the augmented system message from info :data (which includes
    ;; gptel context injected by gptel-context--wrap-in-buffer).  Fall back
    ;; to the chat buffer's buffer-local value if :data doesn't carry it.
    (let* ((data (plist-get info :data))
           (system-msg (or (and (listp data) (plist-get data :system-message))
                           (if (and chat-buf (buffer-live-p chat-buf))
                               (buffer-local-value 'gptel--system-message chat-buf)
                             gptel--system-message))))
      (when (and system-msg
                 (stringp system-msg)
                 (not (string-empty-p system-msg)))
        (setq args (append args (list "--append-system-prompt" system-msg)))))
    ;; Session management: --resume, --fork-session, --session-id
    ;; Must run in the chat buffer to access org properties and buffer-local
    ;; session vars.
    (let* ((session-state (if (and chat-buf (buffer-live-p chat-buf))
                              (with-current-buffer chat-buf
                                (gptel-claude-code--session-state backend))
                            (gptel-claude-code--session-state backend)))
           (session-args (gptel-claude-code--session-args session-state))
           (effective-sid (gptel-claude-code--effective-session-id session-state)))
      ;; Log session state transition
      (when (eq gptel-log-level 'debug)
        (gptel--log (format "Session state: %S, id: %s"
                            (plist-get session-state :state)
                            (or effective-sid "none"))
                    "session" 'no-json))
      (setq args (append args session-args))
      ;; MCP config: --mcp-config with session-aware URL
      (when effective-sid
        (let ((mcp-port (gptel-claude-code-mcp-port backend)))
          (when mcp-port
            (setq args (append args
                               (list "--mcp-config"
                                     (gptel-claude-code--mcp-config-json
                                      effective-sid mcp-port))))
            (setq args (append args
                               (list "--permission-prompt-tool"
                                     "mcp__emacs__permission_prompt"))))))
      ;; extra-args from backend
      (when-let* ((extra (gptel-claude-code-extra-args backend)))
        (setq args (append args extra)))
      ;; Log full args at debug level
      (when (eq gptel-log-level 'debug)
        (gptel--log (format "Claude Code args: %S" args)
                    "build-args" 'no-json))
      args)))

;;; Process spawner

(defun gptel-claude-code--get-response (fsm)
  "Spawn a Claude Code CLI process for the request in FSM.

FSM is a `gptel-fsm' state machine.  Its INFO slot contains the
data required for the request.  This function is the Claude Code
equivalent of `gptel-curl-get-response'."
  (let* ((token (md5 (format "%s%s%s%s"
                             (random) (emacs-pid) (user-full-name)
                             (recent-keys))))
         (info (gptel-fsm-info fsm))
         (backend (plist-get info :backend))
         (args (gptel-claude-code--build-args info backend))
         (stream (plist-get info :stream))
         (cwd (funcall (gptel-claude-code-cwd-fn backend)))
         (buf (gptel--temp-buffer " *gptel-claude-code*")))
    ;; Append prompt text as positional argument (Claude Code reads prompt
    ;; from CLI args, not stdin, in --print mode)
    (let* ((data (plist-get info :data))
           (prompt-text (if (and (listp data) (plist-get data :prompt))
                            (plist-get data :prompt)
                          data)))
      (when (and prompt-text (stringp prompt-text) (not (string-empty-p prompt-text)))
        (setq args (append args (list prompt-text)))))
    (condition-case err
        (let ((process (let ((default-directory cwd))
                         (apply #'start-process "gptel-claude-code" buf
                                (gptel-claude-code-claude-command backend) args))))
          ;; Log command at debug level
          (when (eq gptel-log-level 'debug)
            (gptel--log (mapconcat #'shell-quote-argument
                                   (cons (gptel-claude-code-claude-command backend) args)
                                   " \\\n")
                        "request Claude Code command" 'no-json))
          ;; Log process start with cwd
          (when (eq gptel-log-level 'debug)
            (gptel--log (format "Claude Code process started, cwd: %s" cwd)
                        "process" 'no-json))
          ;; Store cwd in chat buffer for team transcript watching
          (when-let* ((chat-buf (plist-get info :buffer)))
            (when (buffer-live-p chat-buf)
              (with-current-buffer chat-buf
                (setq gptel-claude-code--team-cwd cwd))))
          (with-current-buffer (process-buffer process)
            (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
            (set-process-query-on-exit-flag process nil)
            ;; Set up FSM info -- mirrors gptel-curl-get-response logic
            (if (plist-get info :token) ;not the first run, set only the token
                (plist-put info :token token)
              (setf (gptel-fsm-info fsm) ;first run, set all process parameters
                    (nconc (list :token token
                                 :transformer
                                 (when (with-current-buffer (plist-get info :buffer)
                                         (and (derived-mode-p 'org-mode)
                                              gptel-org-convert-response))
                                   (gptel--stream-convert-markdown->org
                                    (plist-get info :position))))
                           (unless (plist-get info :callback)
                             (list :callback (if stream
                                                 #'gptel-curl--stream-insert-response
                                               #'gptel--insert-response)))
                           info)))
            ;; Always use streaming filter/sentinel (Claude Code --print is stream-json)
            (set-process-filter process #'gptel-claude-code--stream-filter)
            (set-process-sentinel process #'gptel-claude-code--stream-cleanup)
            ;; Register in request alist
            ;; The abort-fn is called by gptel-abort to clean up.
            ;; It must cancel timers, stop file watchers, and kill the process.
            (setf (alist-get process gptel--request-alist)
                  (cons fsm
                        #'(lambda ()
                            ;; Cancel timeout timer
                            (when-let* ((timer (plist-get info :claude-code-timer)))
                              (cancel-timer timer))
                            ;; Stop teammate file watchers
                            (when-let* ((buf (plist-get info :buffer)))
                              (when (buffer-live-p buf)
                                (gptel-claude-code--cleanup-watchers buf)))
                            ;; Kill the process and its buffer
                            (set-process-sentinel process #'ignore)
                            (delete-process process)
                            (when (buffer-live-p (process-buffer process))
                              (kill-buffer (process-buffer process)))))))
          ;; Set up timeout timer
          (when gptel-claude-code-timeout
            (let ((timer (run-at-time gptel-claude-code-timeout nil
                                      (lambda ()
                                        (when (process-live-p process)
                                          (plist-put info :error
                                                     "Claude Code query timed out")
                                          (delete-process process))))))
              (plist-put info :claude-code-timer timer))))
      (file-missing
       ;; Claude binary not found
       (gptel--log (format "Claude CLI not found: %s"
                           (gptel-claude-code-claude-command backend))
                   "error" 'no-json)
       (plist-put info :error
                  (format "Claude CLI not found: %s"
                          (gptel-claude-code-claude-command backend)))
       (funcall (or (plist-get info :callback) #'gptel--insert-response)
                nil info)
       (gptel--fsm-transition fsm))
      (error
       ;; Other spawn errors
       (gptel--log (format "Failed to start Claude Code: %s"
                           (error-message-string err))
                   "error" 'no-json)
       (plist-put info :error
                  (format "Failed to start Claude Code: %s"
                          (error-message-string err)))
       (funcall (or (plist-get info :callback) #'gptel--insert-response)
                nil info)
       (gptel--fsm-transition fsm)))))

;;; WAIT handler advice

(define-advice gptel--handle-wait (:around (orig-fn fsm) claude-code)
  "Dispatch to Claude Code process spawner for gptel-claude-code backends."
  (let* ((info (gptel-fsm-info fsm))
         (backend (plist-get info :backend)))
    (if (gptel-claude-code-p backend)
        (progn
          ;; Reset flags (same as original function does)
          (dolist (key '(:tool-success :tool-use :claude-code-tools
                         :error :http-status :reasoning))
            (when (plist-get info key)
              (plist-put info key nil)))
          (gptel-claude-code--get-response fsm)
          (run-hooks 'gptel-post-request-hook))
      (funcall orig-fn fsm))))

;;; Safety-net generic methods
;;
;; Claude Code handles all tool execution internally via MCP.  The gptel
;; tool-use FSM should never reach TOOL state for this backend, but if it
;; does (e.g. due to advices or future gptel changes), these methods
;; prevent cl-no-applicable-method crashes.

(cl-defmethod gptel--parse-tool-results ((_backend gptel-claude-code) _tool-use)
  "No-op: Claude Code handles tool results internally."
  nil)

(eval-and-compile
  (let ((dir (file-name-directory (or load-file-name byte-compile-current-file ""))))
    (unless (member dir load-path)
      (add-to-list 'load-path dir))))
(require 'gptel-claude-code-display)
(require 'gptel-claude-code-session)
(require 'gptel-claude-code-team)
(require 'gptel-claude-code-mcp)
(require 'gptel-claude-code-stream)

(provide 'gptel-claude-code)
;;; gptel-claude-code.el ends here
