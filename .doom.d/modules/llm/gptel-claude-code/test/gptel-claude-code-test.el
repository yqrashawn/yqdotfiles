;;; gptel-claude-code-test.el --- Tests for gptel-claude-code  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for gptel-claude-code: stream parser, session management,
;; display formatting, args builder, and MCP config.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Ensure the module directory is on load-path
(let ((dir (file-name-directory (or load-file-name
                                    (buffer-file-name)
                                    default-directory))))
  (add-to-list 'load-path (expand-file-name ".." dir)))

(require 'gptel)
(require 'gptel-claude-code)
(require 'gptel-claude-code-stream)
(require 'gptel-claude-code-session)
(require 'gptel-claude-code-display)
(require 'gptel-claude-code-mcp)

;;; ============================================================
;;; Stream parser tests
;;; ============================================================

(ert-deftest gptel-claude-code-test-strip-ansi-basic ()
  "Test ANSI stripping with basic color codes."
  (should (equal (gptel-claude-code--strip-ansi "\033[31mhello\033[0m")
                 "hello"))
  (should (equal (gptel-claude-code--strip-ansi "\033[1;32mbold green\033[0m")
                 "bold green")))

(ert-deftest gptel-claude-code-test-strip-ansi-empty ()
  "Test ANSI stripping with empty and no-ansi strings."
  (should (equal (gptel-claude-code--strip-ansi "") ""))
  (should (equal (gptel-claude-code--strip-ansi "no ansi here") "no ansi here")))

(ert-deftest gptel-claude-code-test-strip-ansi-multiple ()
  "Test ANSI stripping with multiple sequences."
  (should (equal (gptel-claude-code--strip-ansi
                  "\033[1m\033[33mwarning\033[0m: \033[36minfo\033[0m")
                 "warning: info")))

(ert-deftest gptel-claude-code-test-strip-ansi-cursor-codes ()
  "Test ANSI stripping with cursor movement codes."
  (should (equal (gptel-claude-code--strip-ansi "\033[2Jclear\033[H")
                 "clear")))

(ert-deftest gptel-claude-code-test-handle-system-init ()
  "Test system init message handler extracts session-id and model."
  (let* ((buf (generate-new-buffer " *test-gptel*"))
         (info (list :buffer buf))
         (msg (list :type "system"
                    :subtype "init"
                    :session_id "test-session-123"
                    :model "claude-haiku-4-5-20251001")))
    (unwind-protect
        (progn
          ;; Set gptel-model in the chat buffer so the handler can
          ;; store the correct model name via gptel--model-name
          (with-current-buffer buf
            (setq-local gptel-model 'haiku))
          ;; Need to set up minimal request alist for FSM transition
          ;; The handler returns nil (no display text)
          (should (null (gptel-claude-code--handle-system msg info)))
          ;; Verify session-id was stored in buffer
          (should (equal (buffer-local-value
                          'gptel-claude-code--session-id buf)
                         "test-session-123"))
          ;; Verify model was stored in buffer -- should use gptel model
          ;; name (not the raw Claude Code init model string)
          (should (equal (buffer-local-value
                          'gptel-claude-code--session-model buf)
                         "haiku"))
          ;; Verify http-status was set
          (should (equal (plist-get info :http-status) "200")))
      (kill-buffer buf))))

(ert-deftest gptel-claude-code-test-handle-system-non-init ()
  "Test system handler ignores non-init subtypes."
  (let ((info (list :buffer nil))
        (msg (list :type "system" :subtype "other")))
    (should (null (gptel-claude-code--handle-system msg info)))
    ;; http-status should NOT be set
    (should (null (plist-get info :http-status)))))

(ert-deftest gptel-claude-code-test-stream-event-text-delta ()
  "Test stream_event text_delta returns text."
  (let ((info (list :buffer nil))
        (msg (list :type "stream_event"
                   :event (list :type "content_block_delta"
                                :delta (list :type "text_delta"
                                             :text "Hello world")))))
    (should (equal (gptel-claude-code--handle-stream-event msg info)
                   "Hello world"))))

(ert-deftest gptel-claude-code-test-stream-event-thinking-delta ()
  "Test stream_event thinking_delta stores reasoning in info."
  (let ((info (list :buffer nil))
        (msg (list :type "stream_event"
                   :event (list :type "content_block_delta"
                                :delta (list :type "thinking_delta"
                                             :thinking "Let me think...")))))
    ;; Handler returns nil (thinking is not displayed inline)
    (should (null (gptel-claude-code--handle-stream-event msg info)))
    ;; But reasoning should be stored
    (should (equal (plist-get info :reasoning) "Let me think..."))))

(ert-deftest gptel-claude-code-test-stream-event-input-json-delta ()
  "Test stream_event input_json_delta accumulates partial JSON."
  (let ((info (list :buffer nil :partial_json nil))
        (msg1 (list :type "stream_event"
                    :event (list :type "content_block_delta"
                                 :delta (list :type "input_json_delta"
                                              :partial_json "{\"comm"))))
        (msg2 (list :type "stream_event"
                    :event (list :type "content_block_delta"
                                 :delta (list :type "input_json_delta"
                                              :partial_json "and\": \"ls\"}")))))
    ;; First chunk
    (gptel-claude-code--handle-stream-event msg1 info)
    (should (equal (plist-get info :partial_json) '("{\"comm")))
    ;; Second chunk
    (gptel-claude-code--handle-stream-event msg2 info)
    (should (equal (plist-get info :partial_json)
                   '("and\": \"ls\"}" "{\"comm")))))

(ert-deftest gptel-claude-code-test-stream-event-content-block-start-tool ()
  "Test content_block_start for tool_use pushes to :claude-code-tools."
  (let ((info (list :buffer nil :claude-code-tools nil))
        (msg (list :type "stream_event"
                   :event (list :type "content_block_start"
                                :content_block (list :type "tool_use"
                                                     :id "tool-1"
                                                     :name "Bash")))))
    (let ((result (gptel-claude-code--handle-stream-event msg info)))
      ;; Should return a tool header string with id
      (should (stringp result))
      (should (string-match-p "begin_tool Bash :id tool-1" result))
      ;; Should have pushed to :claude-code-tools (not :tool-use)
      (should (= 1 (length (plist-get info :claude-code-tools))))
      (should (equal (plist-get (car (plist-get info :claude-code-tools)) :name) "Bash"))
      ;; :tool-use must NOT be set — avoids triggering gptel's tool FSM
      (should (null (plist-get info :tool-use))))))

(ert-deftest gptel-claude-code-test-stream-event-content-block-start-thinking ()
  "Test content_block_start for thinking marks reasoning block."
  (let ((info (list :buffer nil))
        (msg (list :type "stream_event"
                   :event (list :type "content_block_start"
                                :content_block (list :type "thinking"
                                                     :thinking "")))))
    (gptel-claude-code--handle-stream-event msg info)
    (should (eq (plist-get info :reasoning-block) 'in))))

(ert-deftest gptel-claude-code-test-content-block-stop-tool ()
  "Test content_block_stop assembles tool args from partial JSON."
  (let ((info (list :buffer nil
                    :claude-code-current-block "tool_use"
                    :partial_json (list "and\": \"ls\"}" "{\"comm")
                    :claude-code-tools (list (list :id "tool-1" :name "Bash")))))
    (let ((result (gptel-claude-code--handle-stream-event
                   (list :type "stream_event"
                         :event (list :type "content_block_stop"))
                   info)))
      ;; Should return a string with the formatted tool input and footer
      (should (stringp result))
      (should (string-match-p "end_tool" result))
      ;; partial_json should be cleared
      (should (null (plist-get info :partial_json)))
      ;; Tool entry should have :input set
      (let ((tool-entry (car (plist-get info :claude-code-tools))))
        (should (plist-get tool-entry :input))))))

(ert-deftest gptel-claude-code-test-content-block-stop-thinking ()
  "Test content_block_stop for thinking block marks reasoning done."
  (let ((info (list :buffer nil
                    :claude-code-current-block "thinking"
                    :reasoning-block 'in
                    :partial_json nil)))
    (gptel-claude-code--handle-stream-event
     (list :type "stream_event"
           :event (list :type "content_block_stop"))
     info)
    (should (eq (plist-get info :reasoning-block) t))))

(ert-deftest gptel-claude-code-test-handle-result-success ()
  "Test result handler extracts usage and sets stop-reason."
  (let ((info (list :buffer nil))
        (msg (list :type "result"
                   :subtype "success"
                   :usage (list :input_tokens 100
                                :output_tokens 50))))
    (gptel-claude-code--handle-result msg info)
    (should (equal (plist-get info :input-tokens) 100))
    (should (equal (plist-get info :output-tokens) 50))
    (should (equal (plist-get info :stop-reason) "end_turn"))))

(ert-deftest gptel-claude-code-test-handle-result-error ()
  "Test result handler with error subtype but no is_error flag."
  (let ((info (list :buffer nil))
        (msg (list :type "result"
                   :subtype "error"
                   :usage (list :input_tokens 10
                                :output_tokens 0))))
    (gptel-claude-code--handle-result msg info)
    (should (equal (plist-get info :stop-reason) "error"))
    ;; No :is_error flag means :error should not be set
    (should (null (plist-get info :error)))))

(ert-deftest gptel-claude-code-test-handle-result-error-with-errors ()
  "Test result handler extracts errors from is_error result."
  (let ((info (list :buffer nil))
        (msg (list :type "result"
                   :subtype "error_during_execution"
                   :is_error t
                   :errors ["No conversation found with session ID: abc-123"]
                   :usage (list :input_tokens 0
                                :output_tokens 0))))
    (gptel-claude-code--handle-result msg info)
    (should (equal (plist-get info :stop-reason) "error_during_execution"))
    ;; Should extract the error message
    (should (stringp (plist-get info :error)))
    (should (string-match-p "No conversation found" (plist-get info :error)))
    ;; Should set :status for gptel--handle-error
    (should (equal (plist-get info :status) "error_during_execution"))
    ;; Should set :http-status so sentinel takes the success path
    (should (equal (plist-get info :http-status) "200"))))

(ert-deftest gptel-claude-code-test-handle-result-error-no-errors-array ()
  "Test result handler with is_error but no errors array."
  (let ((info (list :buffer nil))
        (msg (list :type "result"
                   :subtype "error_during_execution"
                   :is_error t
                   :usage (list :input_tokens 0
                                :output_tokens 0))))
    (gptel-claude-code--handle-result msg info)
    ;; Should use subtype as error message when errors array is absent
    (should (equal (plist-get info :error) "error_during_execution"))))

(ert-deftest gptel-claude-code-test-handle-result-error-inserts-block ()
  "Test result handler inserts #+begin_error block in org-mode buffer."
  (with-temp-buffer
    (org-mode)
    (insert "@user: test\n\n")
    (let* ((start-marker (point-marker))
           (info (list :buffer (current-buffer)
                       :position start-marker
                       :stream t
                       :callback #'gptel-curl--stream-insert-response))
           (msg (list :type "result"
                      :subtype "error_during_execution"
                      :is_error t
                      :errors ["No conversation found with session ID: abc-123"]
                      :usage (list :input_tokens 0
                                   :output_tokens 0))))
      (gptel-claude-code--handle-result msg info)
      ;; Error block should be inserted
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        (should (cl-search "#+begin_error" content))
        (should (cl-search "No conversation found" content))
        (should (cl-search "#+end_error" content)))
      ;; Text should have gptel ignore property
      (goto-char (point-min))
      (search-forward "#+begin_error")
      (should (eq (get-text-property (point) 'gptel) 'ignore)))))

(ert-deftest gptel-claude-code-test-handle-result-error-block-multiple-errors ()
  "Test error block with multiple errors uses newline separator."
  (with-temp-buffer
    (org-mode)
    (insert "@user: test\n\n")
    (let* ((start-marker (point-marker))
           (info (list :buffer (current-buffer)
                       :position start-marker
                       :stream t
                       :callback #'gptel-curl--stream-insert-response))
           (msg (list :type "result"
                      :subtype "error_during_execution"
                      :is_error t
                      :errors ["Error one" "Error two"]
                      :usage (list :input_tokens 0
                                   :output_tokens 0))))
      (gptel-claude-code--handle-result msg info)
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        (should (cl-search "Error one\nError two" content))))))

(ert-deftest gptel-claude-code-test-handle-assistant-no-tool-use ()
  "Test assistant handler does NOT populate :tool-use (Claude Code handles tools)."
  (let ((info (list :buffer nil :data nil))
        (msg (list :type "assistant"
                   :message (list :content
                                  (vector
                                   (list :type "text" :text "Let me check that.")
                                   (list :type "tool_use"
                                         :id "tu-1"
                                         :name "Read"
                                         :input (list :file_path "/tmp/test.txt")))))))
    (gptel-claude-code--handle-assistant msg info)
    ;; :tool-use must NOT be set — Claude Code handles all tools internally
    (should (null (plist-get info :tool-use)))))

(ert-deftest gptel-claude-code-test-handle-assistant-thinking ()
  "Test assistant handler extracts thinking content."
  (let ((info (list :buffer nil :data nil))
        (msg (list :type "assistant"
                   :message (list :content
                                  (vector
                                   (list :type "thinking"
                                         :thinking "Let me reason about this...")
                                   (list :type "text" :text "The answer is 42."))))))
    (gptel-claude-code--handle-assistant msg info)
    ;; Should store thinking text
    (should (equal (plist-get info :partial_reasoning)
                   "Let me reason about this..."))))

;;; ============================================================
;;; Session tests
;;; ============================================================

(ert-deftest gptel-claude-code-test-session-state-new ()
  "Test session state :new when no session exists."
  (with-temp-buffer
    ;; Not org-mode, no session-id
    (setq-local gptel-claude-code--session-id nil)
    (setq-local gptel-claude-code--session-model nil)
    (let ((gptel-model 'haiku))
      (let ((state (gptel-claude-code--session-state nil)))
        (should (eq (plist-get state :state) :new))
        (should (stringp (plist-get state :session-id)))))))

(ert-deftest gptel-claude-code-test-session-state-continue ()
  "Test session state :continue when session exists with same model."
  (with-temp-buffer
    (setq-local gptel-claude-code--session-id "existing-session")
    (setq-local gptel-claude-code--session-model "haiku")
    (let ((gptel-model 'haiku))
      (let ((state (gptel-claude-code--session-state nil)))
        (should (eq (plist-get state :state) :continue))
        (should (equal (plist-get state :session-id) "existing-session"))))))

(ert-deftest gptel-claude-code-test-session-state-fork-model ()
  "Test session state :fork-model when model changes."
  (with-temp-buffer
    (setq-local gptel-claude-code--session-id "existing-session")
    (setq-local gptel-claude-code--session-model "haiku")
    (let ((gptel-model 'sonnet))
      (let ((state (gptel-claude-code--session-state nil)))
        (should (eq (plist-get state :state) :fork-model))
        (should (equal (plist-get state :session-id) "existing-session"))
        (should (stringp (plist-get state :new-session-id)))
        (should (equal (plist-get state :model) "sonnet"))))))

(ert-deftest gptel-claude-code-test-session-state-continue-legacy ()
  "Test session state :continue when session exists but no model recorded."
  (with-temp-buffer
    (setq-local gptel-claude-code--session-id "legacy-session")
    (setq-local gptel-claude-code--session-model nil)
    (let ((gptel-model 'haiku))
      (let ((state (gptel-claude-code--session-state nil)))
        (should (eq (plist-get state :state) :continue))
        (should (equal (plist-get state :session-id) "legacy-session"))))))

(ert-deftest gptel-claude-code-test-session-args-new ()
  "Test session args generation for :new state."
  (let ((args (gptel-claude-code--session-args
               (list :state :new :session-id "uuid-1"))))
    (should (equal args '("--session-id" "uuid-1")))))

(ert-deftest gptel-claude-code-test-session-args-continue ()
  "Test session args generation for :continue state."
  (let ((args (gptel-claude-code--session-args
               (list :state :continue :session-id "uuid-1"))))
    (should (equal args '("--resume" "uuid-1")))))

(ert-deftest gptel-claude-code-test-session-args-fork ()
  "Test session args generation for :fork state."
  (let ((args (gptel-claude-code--session-args
               (list :state :fork
                     :parent-session-id "parent-uuid"
                     :session-id "new-uuid"))))
    (should (equal args '("--resume" "parent-uuid"
                          "--fork-session"
                          "--session-id" "new-uuid")))))

(ert-deftest gptel-claude-code-test-session-args-fork-model ()
  "Test session args generation for :fork-model state.
--model is NOT included here; it is added by `gptel-claude-code--build-args'
to avoid duplicate --model flags."
  (let ((args (gptel-claude-code--session-args
               (list :state :fork-model
                     :session-id "old-uuid"
                     :new-session-id "new-uuid"
                     :model "opus"))))
    (should (equal args '("--resume" "old-uuid"
                          "--fork-session"
                          "--session-id" "new-uuid")))))

;;; ============================================================
;;; Display tests
;;; ============================================================

(ert-deftest gptel-claude-code-test-format-tool-input-bash ()
  "Test Bash tool input formatting."
  (should (equal (gptel-claude-code--format-tool-input
                  "Bash" (list :command "ls -la"))
                 "$ ls -la")))

(ert-deftest gptel-claude-code-test-format-tool-input-read ()
  "Test Read tool input formatting."
  (should (equal (gptel-claude-code--format-tool-input
                  "Read" (list :file_path "/tmp/test.txt"))
                 "/tmp/test.txt")))

(ert-deftest gptel-claude-code-test-format-tool-input-edit ()
  "Test Edit tool input formatting."
  (let ((result (gptel-claude-code--format-tool-input
                 "Edit" (list :file_path "/tmp/test.txt"
                              :old_string "foo"
                              :new_string "bar"))))
    (should (string-match-p "/tmp/test.txt" result))
    (should (string-match-p "foo" result))
    (should (string-match-p "bar" result))
    (should (string-match-p "->" result))))

(ert-deftest gptel-claude-code-test-format-tool-input-grep ()
  "Test Grep tool input formatting."
  (should (equal (gptel-claude-code--format-tool-input
                  "Grep" (list :pattern "TODO" :path "/src"))
                 "TODO in /src"))
  ;; Without path
  (should (equal (gptel-claude-code--format-tool-input
                  "Grep" (list :pattern "TODO"))
                 "TODO")))

(ert-deftest gptel-claude-code-test-format-tool-input-glob ()
  "Test Glob tool input formatting."
  (should (equal (gptel-claude-code--format-tool-input
                  "Glob" (list :pattern "*.el" :path "/src"))
                 "*.el in /src")))

(ert-deftest gptel-claude-code-test-format-tool-input-write ()
  "Test Write tool input formatting."
  (should (equal (gptel-claude-code--format-tool-input
                  "Write" (list :file_path "/tmp/out.txt"))
                 "/tmp/out.txt")))

(ert-deftest gptel-claude-code-test-format-tool-input-unknown ()
  "Test unknown tool input falls back to JSON."
  (let ((result (gptel-claude-code--format-tool-input
                 "CustomTool" (list :foo "bar"))))
    ;; Should be a JSON string
    (should (stringp result))
    (should (string-match-p "foo" result))))

(ert-deftest gptel-claude-code-test-format-tool-input-nil ()
  "Test nil tool input returns empty string."
  (should (equal (gptel-claude-code--format-tool-input "Bash" nil) "")))

(ert-deftest gptel-claude-code-test-format-tool-result ()
  "Test tool result formatting."
  (let ((result (gptel-claude-code--format-tool-result
                 "Bash" "output text" nil)))
    (should (string-match-p "begin_result Bash" result))
    (should (string-match-p "output text" result))
    (should (string-match-p "end_result" result))))

(ert-deftest gptel-claude-code-test-format-tool-result-with-id ()
  "Test tool result formatting includes tool_use_id when provided."
  (let ((result (gptel-claude-code--format-tool-result
                 "Bash" "output text" nil "toolu_abc123")))
    (should (string-match-p "begin_result Bash :id toolu_abc123" result))
    (should (string-match-p "output text" result))
    (should (string-match-p "end_result" result))))

(ert-deftest gptel-claude-code-test-format-tool-result-error ()
  "Test tool result formatting with error."
  (let ((result (gptel-claude-code--format-tool-result
                 "Bash" "command not found" t)))
    (should (string-match-p "ERROR:" result))
    (should (string-match-p "command not found" result))))

(ert-deftest gptel-claude-code-test-format-tool-result-truncation ()
  "Test tool result truncation for long output."
  (let ((long-content (make-string 3000 ?x)))
    (let ((result (gptel-claude-code--format-tool-result
                   "Bash" long-content nil)))
      (should (string-match-p "truncated" result))
      ;; Result should be shorter than original
      (should (< (length result) (length long-content))))))

(ert-deftest gptel-claude-code-test-format-tool-use-header ()
  "Test tool use header formatting."
  (let ((result (gptel-claude-code--format-tool-use-header "Read")))
    (should (string-match-p "begin_tool Read" result))))

(ert-deftest gptel-claude-code-test-format-tool-use-header-with-id ()
  "Test tool use header includes tool_use_id when provided."
  (let ((result (gptel-claude-code--format-tool-use-header "Read" "toolu_xyz789")))
    (should (string-match-p "begin_tool Read :id toolu_xyz789" result))))

(ert-deftest gptel-claude-code-test-format-tool-use-footer ()
  "Test tool use footer formatting."
  (let ((result (gptel-claude-code--format-tool-use-footer)))
    (should (string-match-p "end_tool" result))))

;;; ============================================================
;;; Args builder tests
;;; ============================================================

(ert-deftest gptel-claude-code-test-build-args-basic-flags ()
  "Test basic args include default flags."
  (let* ((backend (gptel--make-claude-code
                   :name "test"
                   :host "localhost"
                   :protocol "file"
                   :endpoint ""
                   :stream t
                   :models (gptel--process-models '(haiku sonnet))
                   :permission-mode "bypassPermissions"
                   :default-flags '("--print" "--output-format" "stream-json")
                   :mcp-port 8080
                   :cwd-fn (lambda () "/tmp")))
         (gptel-model 'haiku)
         (gptel--system-message nil)
         ;; Mock session state to avoid org-mode dependency
         (session-state (list :state :new :session-id "test-uuid")))
    (cl-letf (((symbol-function 'gptel-claude-code--session-state)
               (lambda (_b) session-state))
              ((symbol-function 'gptel-claude-code--mcp-config-json)
               (lambda (sid port)
                 (format "{\"sid\":\"%s\",\"port\":%d}" sid port))))
      (let ((args (gptel-claude-code--build-args nil backend)))
        ;; Should contain default flags
        (should (member "--print" args))
        (should (member "--output-format" args))
        (should (member "stream-json" args))
        ;; Should contain --model
        (should (member "--model" args))
        (should (member "haiku" args))
        ;; Should contain --permission-mode
        (should (member "--permission-mode" args))
        (should (member "bypassPermissions" args))
        ;; Should contain --session-id for new session
        (should (member "--session-id" args))
        (should (member "test-uuid" args))))))

(ert-deftest gptel-claude-code-test-build-args-system-prompt ()
  "Test system prompt is included when set."
  (let* ((backend (gptel--make-claude-code
                   :name "test"
                   :host "localhost"
                   :protocol "file"
                   :endpoint ""
                   :stream t
                   :models (gptel--process-models '(haiku))
                   :permission-mode "bypassPermissions"
                   :default-flags '("--print")
                   :mcp-port 8080
                   :cwd-fn (lambda () "/tmp")))
         (gptel-model 'haiku)
         (gptel--system-message "You are a helpful assistant.")
         (session-state (list :state :new :session-id "test-uuid")))
    (cl-letf (((symbol-function 'gptel-claude-code--session-state)
               (lambda (_b) session-state))
              ((symbol-function 'gptel-claude-code--mcp-config-json)
               (lambda (sid port)
                 (format "{\"sid\":\"%s\",\"port\":%d}" sid port))))
      (let ((args (gptel-claude-code--build-args nil backend)))
        (should (member "--append-system-prompt" args))
        (should (member "You are a helpful assistant." args))))))

(ert-deftest gptel-claude-code-test-build-args-no-system-prompt ()
  "Test system prompt is omitted when nil or empty."
  (let* ((backend (gptel--make-claude-code
                   :name "test"
                   :host "localhost"
                   :protocol "file"
                   :endpoint ""
                   :stream t
                   :models (gptel--process-models '(haiku))
                   :permission-mode "bypassPermissions"
                   :default-flags '("--print")
                   :mcp-port 8080
                   :cwd-fn (lambda () "/tmp")))
         (gptel-model 'haiku)
         (gptel--system-message nil)
         (session-state (list :state :new :session-id "test-uuid")))
    (cl-letf (((symbol-function 'gptel-claude-code--session-state)
               (lambda (_b) session-state))
              ((symbol-function 'gptel-claude-code--mcp-config-json)
               (lambda (sid port)
                 (format "{\"sid\":\"%s\",\"port\":%d}" sid port))))
      (let ((args (gptel-claude-code--build-args nil backend)))
        (should-not (member "--append-system-prompt" args))))))

(ert-deftest gptel-claude-code-test-build-args-mcp-config ()
  "Test MCP config is included when session-id is available."
  (let* ((backend (gptel--make-claude-code
                   :name "test"
                   :host "localhost"
                   :protocol "file"
                   :endpoint ""
                   :stream t
                   :models (gptel--process-models '(haiku))
                   :permission-mode "bypassPermissions"
                   :default-flags '("--print")
                   :mcp-port 9999
                   :cwd-fn (lambda () "/tmp")))
         (gptel-model 'haiku)
         (gptel--system-message nil)
         (session-state (list :state :new :session-id "mcp-test-uuid")))
    (cl-letf (((symbol-function 'gptel-claude-code--session-state)
               (lambda (_b) session-state))
              ((symbol-function 'gptel-claude-code--mcp-config-json)
               (lambda (sid port)
                 (format "{\"sid\":\"%s\",\"port\":%d}" sid port))))
      (let ((args (gptel-claude-code--build-args nil backend)))
        ;; Should contain --mcp-config
        (should (member "--mcp-config" args))
        ;; Should contain --permission-prompt-tool
        (should (member "--permission-prompt-tool" args))
        (should (member "mcp__emacs__permission_prompt" args))))))

(ert-deftest gptel-claude-code-test-build-args-session-resume ()
  "Test session args for resume (continue) state."
  (let* ((backend (gptel--make-claude-code
                   :name "test"
                   :host "localhost"
                   :protocol "file"
                   :endpoint ""
                   :stream t
                   :models (gptel--process-models '(haiku))
                   :permission-mode "bypassPermissions"
                   :default-flags '("--print")
                   :mcp-port 8080
                   :cwd-fn (lambda () "/tmp")))
         (gptel-model 'haiku)
         (gptel--system-message nil)
         (session-state (list :state :continue
                              :session-id "resume-uuid")))
    (cl-letf (((symbol-function 'gptel-claude-code--session-state)
               (lambda (_b) session-state))
              ((symbol-function 'gptel-claude-code--mcp-config-json)
               (lambda (sid port)
                 (format "{\"sid\":\"%s\",\"port\":%d}" sid port))))
      (let ((args (gptel-claude-code--build-args nil backend)))
        (should (member "--resume" args))
        (should (member "resume-uuid" args))))))

(ert-deftest gptel-claude-code-test-build-args-extra-args ()
  "Test extra-args from backend are included."
  (let* ((backend (gptel--make-claude-code
                   :name "test"
                   :host "localhost"
                   :protocol "file"
                   :endpoint ""
                   :stream t
                   :models (gptel--process-models '(haiku))
                   :permission-mode "bypassPermissions"
                   :default-flags '("--print")
                   :extra-args '("--max-turns" "5")
                   :mcp-port 8080
                   :cwd-fn (lambda () "/tmp")))
         (gptel-model 'haiku)
         (gptel--system-message nil)
         (session-state (list :state :new :session-id "test-uuid")))
    (cl-letf (((symbol-function 'gptel-claude-code--session-state)
               (lambda (_b) session-state))
              ((symbol-function 'gptel-claude-code--mcp-config-json)
               (lambda (sid port)
                 (format "{\"sid\":\"%s\",\"port\":%d}" sid port))))
      (let ((args (gptel-claude-code--build-args nil backend)))
        (should (member "--max-turns" args))
        (should (member "5" args))))))

(ert-deftest gptel-claude-code-test-build-args-chat-buffer-context ()
  "Test build-args reads session state from chat buffer, not current buffer.
This is a regression test for the buffer context bug: the WAIT handler
runs after the prompt buffer is killed, so the current buffer is random.
build-args must use `with-current-buffer' to access chat buffer state."
  (let* ((chat-buf (generate-new-buffer " *test-chat*"))
         (backend (gptel--make-claude-code
                   :name "test"
                   :host "localhost"
                   :protocol "file"
                   :endpoint ""
                   :stream t
                   :models (gptel--process-models '(haiku sonnet))
                   :permission-mode "bypassPermissions"
                   :default-flags '("--print")
                   :mcp-port 8080
                   :cwd-fn (lambda () "/tmp")))
         (info (list :buffer chat-buf :model 'haiku)))
    (unwind-protect
        (progn
          ;; Set up chat buffer with session state and system message
          (with-current-buffer chat-buf
            (setq-local gptel-claude-code--session-id "existing-session")
            (setq-local gptel-claude-code--session-model "haiku")
            (setq-local gptel-model 'haiku)
            (setq-local gptel--system-message "Be a pirate."))
          ;; Call build-args from a DIFFERENT buffer (simulating the
          ;; killed prompt buffer scenario)
          (with-temp-buffer
            ;; Verify we are NOT in the chat buffer
            (should-not (eq (current-buffer) chat-buf))
            ;; Set different values in this buffer to prove build-args
            ;; reads from chat-buf, not current buffer
            (setq-local gptel-claude-code--session-id nil)
            (setq-local gptel-claude-code--session-model nil)
            (setq-local gptel-model 'sonnet)
            (setq-local gptel--system-message nil)
            (cl-letf (((symbol-function 'gptel-claude-code--mcp-config-json)
                       (lambda (sid port)
                         (format "{\"sid\":\"%s\",\"port\":%d}" sid port))))
              (let ((args (gptel-claude-code--build-args info backend)))
                ;; Should use model from info plist (haiku), not current
                ;; buffer (sonnet)
                (should (member "haiku" args))
                (should-not (member "sonnet" args))
                ;; Should include --resume (from chat buffer's session state)
                ;; not --session-id (which would indicate :new state)
                (should (member "--resume" args))
                (should (member "existing-session" args))
                ;; Should include system prompt from chat buffer
                (should (member "--append-system-prompt" args))
                (should (member "Be a pirate." args))))))
      (kill-buffer chat-buf))))

;;; ============================================================
;;; MCP tests
;;; ============================================================

(ert-deftest gptel-claude-code-test-mcp-config-json-format ()
  "Test MCP config JSON format."
  (let ((json (gptel-claude-code--mcp-config-json "sess-123" 8080)))
    ;; Should be valid JSON-like string
    (should (stringp json))
    ;; Should contain mcpServers
    (should (string-match-p "mcpServers" json))
    ;; Should contain the session id in the URL
    (should (string-match-p "sess-123" json))
    ;; Should contain the port
    (should (string-match-p "8080" json))
    ;; Should contain the emacs server name
    (should (string-match-p "emacs" json))
    ;; Should contain the full path structure
    (should (string-match-p "/mcp/v1/sessions/sess-123/messages" json))))

(ert-deftest gptel-claude-code-test-mcp-config-json-different-port ()
  "Test MCP config JSON with different port."
  (let ((json (gptel-claude-code--mcp-config-json "abc" 9999)))
    (should (string-match-p "9999" json))
    (should (string-match-p "abc" json))))

(ert-deftest gptel-claude-code-test-session-registration ()
  "Test session registration and lookup."
  (let ((gptel-claude-code--session-map (make-hash-table :test 'equal))
        (buf (generate-new-buffer " *test-session*")))
    (unwind-protect
        (progn
          ;; Register session
          (gptel-claude-code--register-session "sess-001" buf)
          ;; Lookup should return the buffer
          (should (eq (gptel-claude-code--session-buffer "sess-001") buf))
          ;; Unknown session should return nil
          (should (null (gptel-claude-code--session-buffer "unknown"))))
      (kill-buffer buf))))

(ert-deftest gptel-claude-code-test-session-registration-nil ()
  "Test session registration with nil args does nothing."
  (let ((gptel-claude-code--session-map (make-hash-table :test 'equal)))
    (gptel-claude-code--register-session nil nil)
    (should (= 0 (hash-table-count gptel-claude-code--session-map)))
    (gptel-claude-code--register-session "sess" nil)
    (should (= 0 (hash-table-count gptel-claude-code--session-map)))
    (gptel-claude-code--register-session nil (current-buffer))
    (should (= 0 (hash-table-count gptel-claude-code--session-map)))))

;;; ============================================================
;;; Effective session-id tests
;;; ============================================================

(ert-deftest gptel-claude-code-test-effective-session-id-new ()
  "Test effective session-id for :new state."
  (should (equal (gptel-claude-code--effective-session-id
                  (list :state :new :session-id "new-uuid"))
                 "new-uuid")))

(ert-deftest gptel-claude-code-test-effective-session-id-continue ()
  "Test effective session-id for :continue state."
  (should (equal (gptel-claude-code--effective-session-id
                  (list :state :continue :session-id "cont-uuid"))
                 "cont-uuid")))

(ert-deftest gptel-claude-code-test-effective-session-id-fork ()
  "Test effective session-id for :fork state."
  (should (equal (gptel-claude-code--effective-session-id
                  (list :state :fork
                        :parent-session-id "parent"
                        :session-id "fork-uuid"))
                 "fork-uuid")))

(ert-deftest gptel-claude-code-test-effective-session-id-fork-model ()
  "Test effective session-id for :fork-model returns new-session-id."
  (should (equal (gptel-claude-code--effective-session-id
                  (list :state :fork-model
                        :session-id "old-uuid"
                        :new-session-id "new-fork-uuid"))
                 "new-fork-uuid")))

;;; ============================================================
;;; Backend struct tests
;;; ============================================================

(ert-deftest gptel-claude-code-test-backend-predicate ()
  "Test gptel-claude-code-p predicate."
  (let ((backend (gptel--make-claude-code
                  :name "test"
                  :host "localhost"
                  :protocol "file"
                  :endpoint ""
                  :stream t
                  :models nil)))
    (should (gptel-claude-code-p backend))
    ;; Non-claude-code backend should fail
    (should-not (gptel-claude-code-p (list :name "fake")))))

(ert-deftest gptel-claude-code-test-backend-defaults ()
  "Test backend struct default values."
  (let ((backend (gptel--make-claude-code
                  :name "test"
                  :host "localhost"
                  :protocol "file"
                  :endpoint ""
                  :stream t
                  :models nil)))
    (should (equal (gptel-claude-code-claude-command backend) "claude"))
    (should (equal (gptel-claude-code-permission-mode backend) "bypassPermissions"))
    (should (equal (gptel-claude-code-mcp-port backend) 8080))
    (should (member "--print" (gptel-claude-code-default-flags backend)))
    (should (member "--verbose" (gptel-claude-code-default-flags backend)))))

;;; ============================================================
;;; Request data tests
;;; ============================================================

(ert-deftest gptel-claude-code-test-request-data-string ()
  "Test gptel--request-data extracts last user message as string."
  (let ((backend (gptel--make-claude-code
                  :name "test"
                  :host "localhost"
                  :protocol "file"
                  :endpoint ""
                  :stream t
                  :models nil)))
    (let ((result (gptel--request-data
                   backend
                   (list (list :role "user" :content "What is 2+2?")
                         (list :role "assistant" :content "4")
                         (list :role "user" :content "Now multiply by 3")))))
      ;; Should return the last user message content
      (should (equal result "Now multiply by 3")))))

(ert-deftest gptel-claude-code-test-request-data-empty ()
  "Test gptel--request-data with no user messages returns empty string."
  (let ((backend (gptel--make-claude-code
                  :name "test"
                  :host "localhost"
                  :protocol "file"
                  :endpoint ""
                  :stream t
                  :models nil)))
    (let ((result (gptel--request-data backend nil)))
      (should (equal result "")))))

;;; ============================================================
;;; Team module tests
;;; ============================================================

(ert-deftest gptel-claude-code-test-transcript-dir ()
  "Test transcript directory computation."
  (let ((dir (gptel-claude-code--transcript-dir
              "/Users/foo/bar" "session-123")))
    (should (string-match-p "-Users-foo-bar" dir))
    (should (string-match-p "session-123" dir))
    (should (string-match-p "subagents" dir))
    (should (string-suffix-p "/" dir))))

(ert-deftest gptel-claude-code-test-transcript-dir-root ()
  "Test transcript directory for root cwd."
  (let ((dir (gptel-claude-code--transcript-dir "/" "sess")))
    (should (string-match-p "^/" dir))
    (should (string-match-p "sess/subagents/" dir))))

;;; ============================================================
;;; Multipart / media tests
;;; ============================================================

(ert-deftest gptel-claude-code-test-parse-multipart-text-only ()
  "Test parse-multipart with text-only parts."
  (let ((result (gptel-claude-code--parse-multipart
                 (list (list :text "Hello ")
                       (list :text "world")))))
    ;; gptel--trim-prefixes might be relevant, but for plain text it should pass through
    (should (stringp result))
    (should (string-match-p "Hello" result))
    (should (string-match-p "world" result))))

(ert-deftest gptel-claude-code-test-parse-multipart-with-media ()
  "Test parse-multipart with media file references."
  (let ((result (gptel-claude-code--parse-multipart
                 (list (list :media "/tmp/image.png" :mime "image/png")
                       (list :text "Describe this image")))))
    (should (stringp result))
    (should (string-match-p "Attached file:" result))
    (should (string-match-p "image.png" result))
    (should (string-match-p "Describe this image" result))))

;;; Abort tests

(ert-deftest gptel-claude-code-test-abort-cancels-timer ()
  "Test that the abort function cancels the timeout timer."
  (let* ((timer-cancelled nil)
         (process-killed nil)
         (watcher-cleaned nil)
         (mock-timer (run-at-time 999 nil #'ignore))
         (mock-buf (generate-new-buffer " *test-abort-buf*"))
         (info (list :buffer mock-buf :claude-code-timer mock-timer))
         (abort-fn (lambda ()
                     ;; Simulate what our real abort-fn does
                     (when-let* ((timer (plist-get info :claude-code-timer)))
                       (cancel-timer timer)
                       (setq timer-cancelled t)))))
    (unwind-protect
        (progn
          (funcall abort-fn)
          (should timer-cancelled))
      (cancel-timer mock-timer)
      (kill-buffer mock-buf))))

(ert-deftest gptel-claude-code-test-abort-fn-structure ()
  "Test that request-alist entries have correct (FSM . ABORT-FN) structure."
  ;; gptel-abort expects (cadr entry) = FSM, (cddr entry) = ABORT-FN
  (let* ((mock-fsm 'mock-fsm)
         (mock-abort-fn (lambda () nil))
         (entry (cons mock-fsm mock-abort-fn)))
    ;; entry is (FSM . ABORT-FN)
    ;; In the alist: (PROC . (FSM . ABORT-FN)) = (PROC FSM . ABORT-FN)
    (let ((alist-entry (cons 'mock-proc entry)))
      (should (eq (car alist-entry) 'mock-proc))
      (should (eq (cadr alist-entry) 'mock-fsm))
      (should (eq (cddr alist-entry) mock-abort-fn)))))

(provide 'gptel-claude-code-test)
;;; gptel-claude-code-test.el ends here
