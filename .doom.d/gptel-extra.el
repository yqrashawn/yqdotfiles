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

(defun +gptel--add-workspace-context ()
  "Add workspace context to gptel request params.
This sets buffer-local `gptel--request-params' with workspace metadata
that will be included in each gptel request.  Also includes any server
metadata from the previous response."
  (when gptel-mode
    (let ((session-id (alist-get 'session-id +gptel--last-response)))
      (setq-local
       gptel--request-params
       (list
        :metadata
        (append
         (list :workspace_root (or (++workspace-current-project-root) default-directory)
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

(defun gptel--setup-workspace-context ()
  "Setup workspace context for gptel buffers.
Adds hooks to set workspace context before each request."
  ;; Add to gptel-mode-hook for chat buffers
  (add-hook! 'gptel-mode-hook #'+gptel--add-workspace-context))

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
      (when (and backend (eq backend gptel--ccl))
        (with-current-buffer proc-buf
          (save-excursion
            ;; For streaming responses, we need to reconstruct the full response
            ;; from the stream chunks. Look for "data:" lines and collect them.
            (goto-char (point-min))
            ;; Skip HTTP headers - look for double newline
            (when (re-search-forward "\n\n" nil t)
              (let ((headers-text (buffer-substring-no-properties (point-min) (point))))
                ;; Now we're at the start of the response body
                ;; Collect all the response text from data: lines
                (let ((full-response ""))
                  (while (re-search-forward "^data: " nil t)
                    (let ((start (point)))
                      (end-of-line)
                      (let ((line (buffer-substring-no-properties start (point))))
                        (unless (string-prefix-p "[DONE]" line)
                          (setq full-response (concat full-response line))))))
                  ;; Try to parse the accumulated response as JSON
                  (condition-case err
                      (when (> (length full-response) 0)
                        (let ((body-plist (gptel--json-read-string full-response)))
                          (+gptel--capture-response-data headers-text body-plist info)))
                    (error
                     (message "[+gptel stream] Failed to parse response: %S" err)
                     (message "[+gptel stream] Response text (first 500 chars): %s"
                              (substring full-response 0 (min 500 (length full-response))))))))))))
      ;; Call original function
      (funcall orig-fn process status)))

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
  (gptel--setup-workspace-context))

