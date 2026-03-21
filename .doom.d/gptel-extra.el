;;; .nixpkgs/.doom.d/gptel-extra.el -*- lexical-binding: t; -*-

;;; Idle buffer cleanup

(defvar +gptel-idle-timeout (* 6 60 60)
  "Seconds of idle time before a gptel-mode buffer is killed.
Defaults to 6 hours.")

(defvar +gptel-idle-check-interval (* 60 60)
  "Seconds between idle buffer checks.
Defaults to 1 hour.")

(defvar +gptel--idle-timer nil
  "Timer for periodic gptel idle buffer checks.")

(defun +gptel--kill-idle-buffers ()
  "Kill file-visiting gptel-mode buffers idle for `+gptel-idle-timeout' seconds.
Uses `buffer-display-time' (last time the buffer was displayed in a window)
to determine idleness.  Only considers buffers whose file exists on disk."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when-let* (((eq gptel-mode t))
                    (file (buffer-file-name))
                    ((file-exists-p file))
                    (display-time (buffer-local-value 'buffer-display-time buf))
                    ((> (float-time (time-subtract (current-time) display-time))
                        +gptel-idle-timeout)))
          (when (buffer-modified-p)
            (save-buffer))
          (kill-buffer buf))))))

(defun +gptel-start-idle-timer ()
  "Start the periodic timer to kill idle gptel-mode buffers."
  (when +gptel--idle-timer
    (cancel-timer +gptel--idle-timer))
  (setq +gptel--idle-timer
        (run-with-timer +gptel-idle-check-interval
                        +gptel-idle-check-interval
                        #'+gptel--kill-idle-buffers)))

(defun +gptel-stop-idle-timer ()
  "Stop the periodic idle buffer check timer."
  (when +gptel--idle-timer
    (cancel-timer +gptel--idle-timer)
    (setq +gptel--idle-timer nil)))

;;; Response data

(defvar-local +gptel--last-response nil
  "Complete response data from the LLM backend in the last request.
This is an alist with the following structure:
  \\='((headers . ((\"Header-Name\" . \"value\") ...))
    (body . <parsed-json-plist>)
    (metadata . <metadata-plist-if-present>))

This can be used to access server-provided metadata or other
response data.")

(defun +gptel--ccl-backend-p ()
  "Return non-nil if current gptel backend is ccl or ccld."
  (and (boundp 'gptel-backend)
       gptel-backend
       (boundp 'gptel--ccl)
       (boundp 'gptel--ccld)
       (or (eq gptel-backend gptel--ccl)
           (eq gptel-backend gptel--ccld))))

(defun +gptel--new-session-id ()
  "Generate a new session ID for ccl/ccld backends.
Always creates a fresh UUID.  Stores it as an org property on the
current heading (respecting gptel branching context) and in the
buffer-local var.  Returns the new UUID, or nil for non-ccl backends."
  (when (+gptel--ccl-backend-p)
    (let ((new-id (org-id-uuid)))
      (when (boundp 'gptel-claude-code--session-id)
        (setq-local gptel-claude-code--session-id new-id))
      (when (derived-mode-p 'org-mode)
        (org-with-wide-buffer
         (org-back-to-heading-or-point-min t)
         (org-set-property "GPTEL_CCL_SESSION_ID" new-id)))
      new-id)))

(defun +gptel--current-heading-session-id ()
  "Read session ID from the current org heading.
Respects gptel branching context — returns the session ID for
the heading at point, not the file-level property.
Returns session-id string or nil."
  (when (+gptel--ccl-backend-p)
    (or (and (derived-mode-p 'org-mode)
             (org-with-wide-buffer
              (org-back-to-heading-or-point-min t)
              (org-entry-get nil "GPTEL_CCL_SESSION_ID")))
        (and (boundp 'gptel-claude-code--session-id)
             gptel-claude-code--session-id))))

(defun +gptel--update-org-file-properties ()
  "Update org file-level properties for workspace root.
Uses `org-entry-put' at point-min so properties live in the same
:PROPERTIES: drawer as gptel's own state."
  (when (derived-mode-p 'org-mode)
    (org-with-wide-buffer
     (goto-char (point-min))
     (when (org-at-heading-p)
       (org-open-line 1))
     ;; Only set workspace root if not already present -- the value from
     ;; the before-send hook (when the user is in the right workspace) is
     ;; correct; the after-response hook may run in a different workspace.
     (unless (org-entry-get (point-min) "GPTEL_WORKSPACE_ROOT")
       (let ((root (or (++workspace-current-project-root) default-directory)))
         (org-entry-put (point-min) "GPTEL_WORKSPACE_ROOT" root))))))

(defun +gptel--add-workspace-context ()
  "Add workspace context to gptel request params.
This sets buffer-local `gptel--request-params' with workspace metadata
that will be included in each gptel request.  Generates a new session
ID on every send (each send is a new Claude Code session)."
  (when gptel-mode
    (+gptel--update-org-file-properties)
    (let ((session-id (+gptel--new-session-id)))
      (setq-local
       gptel--request-params
       (list
        :metadata
        (append
         (list
          :workspace_root (or (++workspace-current-project-root) default-directory)
          :working_dir default-directory)
         (when session-id
           (list :session_id session-id))
         (when-let ((root (++workspace-current-project-root)))
           (list :project_name (file-name-nondirectory
                                (directory-file-name root))))))))))

(defun +gptel--parse-http-headers (headers-text)
  "Parse HTTP HEADERS-TEXT into an alist of (name . value) pairs."
  (let ((headers '()))
    (with-temp-buffer
      (insert headers-text)
      (goto-char (point-min))
      ;; Skip the status line
      (forward-line 1)
      ;; Parse each header line
      (while (not (eobp))
        (when-let* ((line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position)))
                    ((not (string-blank-p line)))
                    (colon-pos (string-match-p ":" line))
                    (name (substring line 0 colon-pos))
                    (value (string-trim (substring line (1+ colon-pos)))))
          (push (cons name value) headers))
        (forward-line 1)))
    (nreverse headers)))

(defun +gptel--capture-response-data (headers-text body-plist info)
  "Store complete response data (headers + body) from LLM backend.

HEADERS-TEXT is the raw HTTP headers as a string.
BODY-PLIST is the parsed JSON response body.
INFO is the gptel process info plist."
  (when-let* ((buf (plist-get info :buffer)))
    (let* ((headers-alist (+gptel--parse-http-headers headers-text))
           (metadata (plist-get body-plist :metadata))
           (response-data
            `((headers . ,headers-alist)
              (body . ,body-plist)
              (metadata . ,metadata))))
      (with-current-buffer buf
        (setq-local +gptel--last-response response-data)))))

(after! gptel
  ;; Hook into streaming response cleanup to capture metadata
  (defadvice! +gptel-curl--stream-cleanup-capture-metadata (orig-fn process status)
    "Capture response metadata (headers, body) from streaming response."
    :around #'gptel-curl--stream-cleanup
    (let* ((proc-buf (process-buffer process))
            (fsm (car (alist-get process gptel--request-alist)))
            (info (and fsm (gptel-fsm-info fsm)))
            (backend (and info (plist-get info :backend))))
      ;; Only capture for CCL backend
      (when (and backend (or (eq backend gptel--ccl) (eq backend gptel--ccld)))
        (with-current-buffer proc-buf
          (save-excursion
            (goto-char (point-min))
            (when (re-search-forward "\n\n" nil t)
              (let ((headers-text (buffer-substring-no-properties (point-min) (point))))
                (+gptel--capture-response-data headers-text nil info)))))))
    (funcall orig-fn process status))

  ;; Also hook into non-streaming responses
  ;; Unlike streaming, non-streaming has the HTTP buffer available
  (cl-defmethod gptel--parse-response :after ((_backend (eql gptel--ccl)) response info)
    "Capture response metadata from CCL backend non-streaming response."
    (when-let* ((buf (plist-get info :buffer)))
      (let* ((headers-text (save-excursion
                             (goto-char (point-min))
                             (when (re-search-forward "^\n" nil t)
                               (buffer-substring-no-properties (point-min) (point)))))
              (headers-alist (when headers-text
                               (+gptel--parse-http-headers headers-text)))
              (metadata (plist-get response :metadata)))
        (with-current-buffer buf
          (setq-local +gptel--last-response
            `((headers . ,headers-alist)
               (body . ,response)
               (metadata . ,metadata)))))))

  ;; Initialize workspace context setup
  (defadvice! +add-workspace-context-before-gptel-send (&optional _args)
    :before #'gptel-send
    (+gptel--add-workspace-context))

  ;; Auto-enable gptel-mode in org buffers that have GPTEL_* properties
  (defun +gptel-auto-enable-in-org ()
    "Auto-enable `gptel-mode' if org buffer has GPTEL_* file properties."
    (when (and (derived-mode-p 'org-mode)
               (not gptel-mode))
      (save-restriction
        (widen)
        (when (cl-some (lambda (prop)
                         (org-entry-get (point-min) prop))
                       '("GPTEL_PRESET" "GPTEL_SYSTEM" "GPTEL_BACKEND"
                         "GPTEL_MODEL" "GPTEL_BOUNDS" "GPTEL_TOOLS"))
          (gptel-mode 1)))))

  (add-hook 'org-mode-hook #'+gptel-auto-enable-in-org)

  ;; Start the idle buffer cleanup timer
  (+gptel-start-idle-timer))

;;; Inject message into running Claude Code turn

(defvar +gptel-inject-message-dir
  (expand-file-name "inject-messages" "~/.claude/")
  "Directory where inject message files are written for Claude Code hooks.")

(defvar-local +gptel-inject--session-id nil
  "Session ID stored for the inject message buffer.")

(defvar +gptel-inject-message-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'+gptel-inject-message-send)
    (define-key map (kbd "C-c C-k") #'+gptel-inject-message-cancel)
    map)
  "Keymap for `+gptel-inject-message-mode'.")

(define-minor-mode +gptel-inject-message-mode
  "Minor mode for composing messages to inject into a running Claude Code turn.
\\<+gptel-inject-message-mode-map>
\\[+gptel-inject-message-send] to send, \\[+gptel-inject-message-cancel] to cancel."
  :lighter " Inject"
  :keymap +gptel-inject-message-mode-map)

(defun +gptel-inject--write-message (content session-id)
  "Write CONTENT to the inject file for SESSION-ID.
Returns t if written, nil if content was empty."
  (if (string-empty-p (string-trim content))
      (progn (message "Empty message, not sending.") nil)
    (let ((file (expand-file-name (concat session-id ".txt")
                                  +gptel-inject-message-dir)))
      (unless (file-directory-p +gptel-inject-message-dir)
        (make-directory +gptel-inject-message-dir t))
      (write-region (string-trim content) nil file nil 'silent)
      (message "Message queued for session %s" session-id)
      t)))

(defun +gptel-inject-message-send ()
  "Send the composed message to the Claude Code session."
  (interactive)
  (+gptel-inject--write-message (buffer-string) +gptel-inject--session-id)
  (let ((buf (current-buffer)))
    (when (window-parameter nil 'quit-restore)
      (quit-window t))
    (when (buffer-live-p buf) (kill-buffer buf))))

(defun +gptel-inject-message-cancel ()
  "Cancel composing the inject message."
  (interactive)
  (let ((buf (current-buffer)))
    (quit-window t)
    (when (buffer-live-p buf) (kill-buffer buf))))

(defun +gptel--buffer-session-id (&optional buf)
  "Return session ID for the current heading in BUF, or nil.
Respects gptel branching context — reads from the heading at point."
  (with-current-buffer (or buf (current-buffer))
    (when (bound-and-true-p gptel-mode)
      (+gptel--current-heading-session-id))))

(defun +gptel--find-session-id ()
  "Find a Claude Code session ID from gptel buffers.
If the current buffer is gptel-mode with a session ID, use it directly.
Otherwise scan all buffers; prompt if multiple found.
Returns (session-id . buffer) or nil."
  ;; Try current buffer first
  (if-let ((sid (+gptel--buffer-session-id)))
      (cons sid (current-buffer))
    ;; Fall back to scanning all buffers
    (let (candidates)
      (dolist (buf (buffer-list))
        (when-let ((sid (+gptel--buffer-session-id buf)))
          (push (cons sid buf) candidates)))
      (cond
       ((null candidates) nil)
       ((= 1 (length candidates)) (car candidates))
       (t (let* ((choices (mapcar (lambda (c)
                                    (cons (format "%s [%s]" (cdr c) (car c))
                                          c))
                                  candidates))
                 (choice (completing-read "Session: " choices nil t)))
            (cdr (assoc choice choices))))))))

;;;###autoload
(defun +gptel-inject-message ()
  "Compose a message to inject into a running Claude Code turn.
The message will be picked up by the PreToolUse hook before the next
tool call and delivered as additionalContext."
  (interactive)
  (let ((found (+gptel--find-session-id)))
    (unless found
      (user-error "No active Claude Code session found in gptel buffers"))
    (let ((session-id (car found))
          (source-buf (cdr found))
          (buf (generate-new-buffer "*inject-message*")))
      (pop-to-buffer buf
                     '((display-buffer-below-selected)
                       (window-height . 8)))
      (org-mode)
      (+gptel-inject-message-mode 1)
      (setq-local +gptel-inject--session-id session-id)
      (setq header-line-format
            (format " Inject message into session %s (%s)  |  C-c C-c send  |  C-c C-k cancel"
                    (substring session-id 0 (min 8 (length session-id)))
                    (buffer-name source-buf)))
      (message "Compose message, C-c C-c to send, C-c C-k to cancel"))))
