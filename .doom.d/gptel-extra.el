;;; .nixpkgs/.doom.d/gptel-extra.el -*- lexical-binding: t; -*-

(defvar-local +gptel--last-response nil
  "Complete response data from the LLM backend in the last request.
This is an alist with the following structure:
  \\='((headers . ((\"Header-Name\" . \"value\") ...))
    (body . <parsed-json-plist>)
    (session-id . \"session-id-from-header\")
    (metadata . <metadata-plist-if-present>))

This can be used to access server-provided context, session info,
or other data to pass to subsequent requests.")

(defun +gptel--update-org-file-properties ()
  "Update org file-level properties for workspace root and CCL session ID.
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
         (org-entry-put (point-min) "GPTEL_WORKSPACE_ROOT" root)))
     (when (and (boundp 'gptel-backend)
                gptel-backend
                (boundp 'gptel--ccl)
                (boundp 'gptel--ccld)
                (or (eq gptel-backend gptel--ccl)
                    (eq gptel-backend gptel--ccld)))
       (let ((session-id (or (and (boundp 'gptel-claude-code--session-id)
                                  gptel-claude-code--session-id)
                             (alist-get 'session-id +gptel--last-response))))
         (when session-id
           (org-entry-put (point-min) "GPTEL_CCL_SESSION_ID" session-id)))))))

(defun +gptel--add-workspace-context ()
  "Add workspace context to gptel request params.
This sets buffer-local `gptel--request-params' with workspace metadata
that will be included in each gptel request.  Also includes any server
metadata from the previous response."
  (when gptel-mode
    (+gptel--update-org-file-properties)
    (let ((session-id (alist-get 'session-id +gptel--last-response)))
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
           (session-id (alist-get "X-Session-Id" headers-alist nil nil #'equal))
           (metadata (plist-get body-plist :metadata))
           (response-data
            `((headers . ,headers-alist)
              (body . ,body-plist)
              (session-id . ,session-id)
              (metadata . ,metadata))))
      (with-current-buffer buf
        (setq-local +gptel--last-response response-data)
        ;; Update workspace context for next request in the correct buffer
        (+gptel--add-workspace-context)))))

(after! gptel
  ;; Hook into streaming response cleanup to capture metadata
  (defadvice! +gptel-curl--stream-cleanup-capture-metadata (orig-fn process status)
    "Capture metadata from streaming response before cleanup."
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
            ;; Skip HTTP headers - look for double newline
            (when (re-search-forward "\n\n" nil t)
              (let ((headers-text (buffer-substring-no-properties (point-min) (point)))
                    (session-id nil))
                ;; Parse SSE data lines for session_id from :session-start event
                (save-excursion
                  (while (and (not session-id)
                              (re-search-forward "^data: " nil t))
                    (let* ((line-end (line-end-position))
                           (json-str (buffer-substring-no-properties (point) line-end)))
                      (unless (string= json-str "[DONE]")
                        (ignore-errors
                          (let* ((json-obj (gptel--json-read-string json-str))
                                 (sid (plist-get json-obj :session_id)))
                            (when sid (setq session-id sid))))))))
                (+gptel--capture-response-data headers-text nil info)
                ;; Override session-id from SSE data if found
                (when (and session-id (plist-get info :buffer))
                  (with-current-buffer (plist-get info :buffer)
                    (setf (alist-get 'session-id +gptel--last-response) session-id)
                    (+gptel--add-workspace-context)))))))))
    ;; Call original function
    (funcall orig-fn process status))

  ;; Also hook into non-streaming responses
  ;; Unlike streaming, non-streaming has the HTTP buffer available
  (cl-defmethod gptel--parse-response :after ((_backend (eql gptel--ccl)) response info)
    "Extract complete response data from CCL backend non-streaming response."
    (when-let* ((buf (plist-get info :buffer)))
      ;; The current buffer is the url-retrieve response buffer with headers
      (let* ((headers-text (save-excursion
                             (goto-char (point-min))
                             (when (re-search-forward "^\n" nil t)
                               (buffer-substring-no-properties (point-min) (point)))))
              (headers-alist (when headers-text
                               (+gptel--parse-http-headers headers-text)))
              (session-id (when headers-alist
                            (alist-get "X-Session-Id" headers-alist nil nil #'equal)))
              (metadata (plist-get response :metadata)))
        
        (with-current-buffer buf
          (setq-local +gptel--last-response
            `((headers . ,headers-alist)
               (body . ,response)
               (session-id . ,session-id)
               (metadata . ,metadata)))
          ;; Update workspace context for next request in the correct buffer
          (+gptel--add-workspace-context)))))

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

  (add-hook 'org-mode-hook #'+gptel-auto-enable-in-org))
