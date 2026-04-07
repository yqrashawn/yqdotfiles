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
  "Return non-nil if current gptel backend is ccl, ccld, or ccl-new."
  (and (boundp 'gptel-backend)
       gptel-backend
       (boundp 'gptel--ccl)
       (boundp 'gptel--ccld)
       (or (eq gptel-backend gptel--ccl)
           (eq gptel-backend gptel--ccld)
           (eq gptel-backend gptel--ccl-new)
           (eq gptel-backend gptel--ccl-new-dev))))

(defun +gptel--ccl-new-backend-p ()
  "Return non-nil if current gptel backend is ccl-new (session-resume enabled)."
  (and (boundp 'gptel-backend)
       (or
        (and
         (boundp 'gptel--ccl-new)
         (eq gptel-backend gptel--ccl-new))
        (and
         (boundp 'gptel--ccl-new-dev)
         (eq gptel-backend gptel--ccl-new-dev)))))

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

(defun +gptel--find-resume-session-id ()
  "Find resume session ID by walking up the org heading lineage.
Checks the current heading first, then walks up to ancestors.
Returns the nearest GPTEL_CCL_SESSION_ID or nil."
  (when (and (+gptel--ccl-new-backend-p)
             (derived-mode-p 'org-mode))
    (org-with-wide-buffer
     (org-back-to-heading-or-point-min t)
     ;; Check current heading first
     (or (org-entry-get nil "GPTEL_CCL_SESSION_ID")
         ;; Walk up to find nearest ancestor with session-id
         (cl-loop while (org-up-heading-safe)
                  thereis (org-entry-get nil "GPTEL_CCL_SESSION_ID"))
         ;; Check file-level property (point-min) as last resort
         (org-entry-get (point-min) "GPTEL_CCL_SESSION_ID")))))

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
ID on every send (each send is a new Claude Code session).
For ccl-new backend, also finds and passes resume_session_id from
the org heading lineage for session fork/resume."
  (when gptel-mode
    (+gptel--update-org-file-properties)
    ;; IMPORTANT: find resume ID BEFORE generating new session ID,
    ;; because new-session-id writes to the heading property that
    ;; find-resume-session-id reads.
    (let* ((resume-session-id (+gptel--find-resume-session-id))
           (session-id (+gptel--new-session-id)))
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
         (when resume-session-id
           (list :resume_session_id resume-session-id))
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

(defun +gptel--store-session-id-on-heading (session-id &optional position)
  "Store SESSION-ID on the org heading at POSITION as GPTEL_CCL_SESSION_ID.
POSITION is a marker or buffer position; defaults to point.
Used by ccl-new backend to enable session resume on next send."
  (when (and session-id
             (derived-mode-p 'org-mode))
    (org-with-wide-buffer
     (when position (goto-char position))
     (org-back-to-heading-or-point-min t)
     (org-set-property "GPTEL_CCL_SESSION_ID" session-id))))

(defun +gptel--capture-response-data (headers-text body-plist info)
  "Store complete response data (headers + body) from LLM backend.

HEADERS-TEXT is the raw HTTP headers as a string.
BODY-PLIST is the parsed JSON response body.
INFO is the gptel process info plist.
For ccl-new backend, also captures the session_id from the
X-Session-Id header and stores it on the current org heading."
  (when-let* ((buf (plist-get info :buffer)))
    (let* ((headers-alist (+gptel--parse-http-headers headers-text))
           (metadata (plist-get body-plist :metadata))
           (response-data
            `((headers . ,headers-alist)
              (body . ,body-plist)
              (metadata . ,metadata))))
      (with-current-buffer buf
        (setq-local +gptel--last-response response-data)
        ;; For ccl-new backend, store session_id from response on heading
        (when-let* ((sid (cdr (assoc "X-Session-Id" headers-alist))))
          (+gptel--store-session-id-on-heading sid (plist-get info :position)))))))

(after! gptel
  ;; Hook into streaming response cleanup to capture metadata
  (defadvice! +gptel-curl--stream-cleanup-capture-metadata (orig-fn process status)
    "Capture response metadata (headers, body) from streaming response.
Also extracts session_id from SSE chunks for ccl-new session resume."
    :around #'gptel-curl--stream-cleanup
    (let* ((proc-buf (process-buffer process))
           (fsm (car (alist-get process gptel--request-alist)))
           (info (and fsm (gptel-fsm-info fsm)))
           (backend (and info (plist-get info :backend))))
      ;; Only capture for CCL backends
      (when (and backend (or (eq backend gptel--ccl)
                             (eq backend gptel--ccld)
                             (and (boundp 'gptel--ccl-new)
                                  (eq backend gptel--ccl-new))
                             (and (boundp 'gptel--ccl-new-dev)
                                  (eq backend gptel--ccl-new-dev))))
        (with-current-buffer proc-buf
          (save-excursion
            (goto-char (point-min))
            (when (re-search-forward "\n\n" nil t)
              (let ((headers-text (buffer-substring-no-properties (point-min) (point))))
                (+gptel--capture-response-data headers-text nil info)
                ;; For streaming, session_id is in the first SSE data chunk,
                ;; not in HTTP headers. Scan SSE body for "session_id" field.
                (when-let* ((buf (plist-get info :buffer)))
                  (save-excursion
                    (when (re-search-forward "\"session_id\"\\s-*:\\s-*\"\\([^\"]+\\)\"" nil t)
                      (let ((sid (match-string 1)))
                        (with-current-buffer buf
                          (+gptel--store-session-id-on-heading sid (plist-get info :position)))))))))))))
    (funcall orig-fn process status))

  ;; Also hook into non-streaming responses
  ;; Unlike streaming, non-streaming has the HTTP buffer available
  (defun +gptel--handle-non-streaming-response (response info)
    "Common handler for non-streaming response metadata capture."
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
                        (metadata . ,metadata)))
          ;; Store session_id on heading for resume
          (when-let* ((sid (cdr (assoc "X-Session-Id" headers-alist))))
            (+gptel--store-session-id-on-heading sid (plist-get info :position)))))))

  (cl-defmethod gptel--parse-response :after ((_backend (eql gptel--ccl)) response info)
    "Capture response metadata from CCL backend non-streaming response."
    (+gptel--handle-non-streaming-response response info))

  (cl-defmethod gptel--parse-response :after ((_backend (eql gptel--ccl-new)) response info)
    "Capture response metadata from CCL-new backend non-streaming response."
    (+gptel--handle-non-streaming-response response info))

  (cl-defmethod gptel--parse-response :after ((_backend (eql gptel--ccl-new-dev)) response info)
    "Capture response metadata from CCL-new-dev backend non-streaming response."
    (+gptel--handle-non-streaming-response response info))

  (cl-defmethod gptel--parse-response :after ((_backend (eql gptel--ccld)) response info)
    "Capture response metadata from CCLd backend non-streaming response."
    (+gptel--handle-non-streaming-response response info))

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

(defun +gptel-find-session-id ()
  "Find a Claude Code session ID from gptel buffers.
If the current buffer is gptel-mode with a session ID, use it directly.
Otherwise scan all buffers; prompt if multiple found.
When called interactively, also copies the session ID to the kill ring.
Returns (session-id . buffer) or nil."
  (interactive)
  (let ((result
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
                   (cdr (assoc choice choices)))))))))
    (when (and result (called-interactively-p 'interactive))
      (kill-new (car result))
      (message "Session ID copied: %s" (car result)))
    result))

(defun +gptel-claude-here ()
  (interactive)
  (when-let ((session-id (+gptel-find-session-id)))
    (let ((root (++workspace-current-project-root)))
      (+kitten
       (format!
        "launch --type tab --tab-title 'cc %s' --cwd '%s' zsh -l -c 'claude --resume %s --fork-session --chrome --permission-mode bypassPermissions --dangerously-skip-permissions'"
        (++workspace-current-project-root)
        (++workspace-current-project-root)
        (car session-id))))))

;;; Comment on quoted region

(defvar-local +gptel-comment--quoted-text nil
  "The quoted text stored for the comment buffer.")

(defvar-local +gptel-comment--source-buffer nil
  "The source gptel buffer to insert the comment into.")

(defvar +gptel-comment-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'+gptel-comment-send)
    (define-key map (kbd "C-c C-k") #'+gptel-comment-cancel)
    map)
  "Keymap for `+gptel-comment-mode'.")

(define-minor-mode +gptel-comment-mode
  "Minor mode for composing comments on quoted gptel text.
\\<+gptel-comment-mode-map>
\\[+gptel-comment-send] to insert, \\[+gptel-comment-cancel] to cancel."
  :lighter " Comment"
  :keymap +gptel-comment-mode-map)

(defun +gptel-comment--insert (quoted-text comment source-buffer)
  "Insert QUOTED-TEXT and COMMENT into SOURCE-BUFFER.
Appends a quote block followed by the comment at the end of the buffer.
QUOTED-TEXT and COMMENT should be pre-trimmed."
  (with-current-buffer source-buffer
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        ;; Ensure blank line before quote block for proper org rendering
        (unless (or (bobp) (eq (char-before (1- (point))) ?\n))
          (insert "\n"))
        (insert "#+begin_quote\n" quoted-text "\n#+end_quote\n" comment "\n")))))

(defun +gptel-comment-send ()
  "Insert the quote+comment block into the source gptel buffer."
  (interactive)
  (let ((comment (string-trim (buffer-string)))
        (quoted +gptel-comment--quoted-text)
        (source +gptel-comment--source-buffer))
    (when (string-empty-p comment)
      (user-error "Empty comment"))
    (unless (buffer-live-p source)
      (user-error "Source buffer no longer exists"))
    (let ((buf (current-buffer)))
      (when (window-parameter nil 'quit-restore)
        (quit-window t))
      (when (buffer-live-p buf) (kill-buffer buf)))
    (+gptel-comment--insert quoted comment source)))

(defun +gptel-comment-cancel ()
  "Cancel composing the comment."
  (interactive)
  (let ((buf (current-buffer)))
    (quit-window t)
    (when (buffer-live-p buf) (kill-buffer buf))))

;;;###autoload
(defun +gptel-comment-region (beg end)
  "Quote the selected region and compose a comment to append.
Select text in a gptel-mode buffer, then call this to open a
comment buffer.  On C-c C-c, the quote+comment block is appended
at the end of the gptel buffer."
  (interactive "r")
  (unless (bound-and-true-p gptel-mode)
    (user-error "Not in a gptel-mode buffer"))
  (let ((quoted-text (string-trim (buffer-substring-no-properties beg end)))
        (source-buf (current-buffer))
        (buf (generate-new-buffer "*gptel-comment*")))
    (when (string-empty-p quoted-text)
      (user-error "No text selected"))
    (deactivate-mark)
    (pop-to-buffer buf
                   '((display-buffer-below-selected)
                     (window-height . 8)))
    (org-mode)
    (+gptel-comment-mode 1)
    (call-interactively #'evil-append)
    (setq-local +gptel-comment--quoted-text quoted-text)
    (setq-local +gptel-comment--source-buffer source-buf)
    (setq header-line-format
          (format " Comment on quote (%s)  |  C-c C-c insert  |  C-c C-k cancel"
                  (buffer-name source-buf)))
    (message "Type your comment, C-c C-c to insert, C-c C-k to cancel")))

;;;###autoload
(defun +gptel-inject-message ()
  "Compose a message to inject into a running Claude Code turn.
The message will be picked up by the PreToolUse hook before the next
tool call and delivered as additionalContext."
  (interactive)
  (let ((found (+gptel-find-session-id)))
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
      (call-interactively #'evil-append)
      (setq-local +gptel-inject--session-id session-id)
      (setq header-line-format
            (format " Inject message into session %s (%s)  |  C-c C-c send  |  C-c C-k cancel"
                    (substring session-id 0 (min 8 (length session-id)))
                    (buffer-name source-buf)))
      (message "Compose message, C-c C-c to send, C-c C-k to cancel"))))

;;; gptel heading navigation

(defun +gptel--marker-regexp ()
  "Build a regexp matching gptel turn markers at BOL for the current buffer.
Returns nil if `gptel-mode' is not active or no non-empty prefixes are
configured for the current major mode."
  (when (bound-and-true-p gptel-mode)
    (let* ((prompt (alist-get major-mode gptel-prompt-prefix-alist))
           (response (alist-get major-mode gptel-response-prefix-alist))
           (parts (cl-remove-if
                   (lambda (s) (or (null s) (string-empty-p s)))
                   (list prompt response))))
      (when parts
        (mapconcat (lambda (s) (concat "^" (regexp-quote s)))
                   parts "\\|")))))

(defun +gptel--in-org-block-p ()
  "Return non-nil if point is inside any org begin/end block."
  (org-between-regexps-p
   "^[ \t]*#\\+begin_"
   "^[ \t]*#\\+end_"))

(defadvice! +gptel-org-forward-heading-same-level-a (orig-fn arg &optional invisible-ok)
  "In `gptel-mode', also stop at conversation turn markers.
Reads prefixes dynamically from `gptel-prompt-prefix-alist' and
`gptel-response-prefix-alist'.  Markers inside org blocks are skipped.
Only one advice is needed: `org-backward-heading-same-level' delegates
to this function with negative ARG."
  :around #'org-forward-heading-same-level
  (let ((marker-re (+gptel--marker-regexp)))
    (if (not marker-re)
        (funcall orig-fn arg invisible-ok)
      (let* ((backward? (and arg (< arg 0)))
             (count (if arg (abs arg) 1))
             (combined-re (concat org-outline-regexp-bol "\\|" marker-re))
             (f (if backward? #'re-search-backward #'re-search-forward))
             (at-heading (org-at-heading-p))
             (at-marker (and (not at-heading)
                             (save-excursion
                               (forward-line 0)
                               (looking-at marker-re))))
             (level (when at-heading
                      (save-excursion
                        (org-back-to-heading invisible-ok)
                        (org-current-level))))
             (result (point)))
        (save-excursion
          (when (or at-heading at-marker)
            (forward-line 0)
            (unless backward? (end-of-line)))
          (while (and (> count 0)
                      (funcall f combined-re nil 'move))
            (let ((mb (match-beginning 0)))
              (save-excursion
                (goto-char mb)
                (if (looking-at org-outline-regexp-bol)
                    ;; Heading match — apply same-level check
                    (let ((l (- (match-end 0) (match-beginning 0) 1)))
                      (cond
                       ((and level (< l level))
                        (setq count 0))
                       ((or (null level) (= l level))
                        (when (or invisible-ok
                                  (not (org--line-fully-invisible-p)))
                          (cl-decf count)
                          (setq result mb)))))
                  ;; Marker match — skip if inside org block
                  (unless (+gptel--in-org-block-p)
                    (cl-decf count)
                    (setq result mb)))))))
        (goto-char result)
        (forward-line 0)))))
