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
(require 'gptel-claude-code-team)

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

(ert-deftest gptel-claude-code-test-parallel-tool-result-not-nested ()
  "Test that tool results arriving during an open tool_use block are queued.
When Claude Code calls multiple tools in parallel, tool 1's result can
arrive while tool 2 is still streaming.  The result must NOT be inserted
inside tool 2's #+begin_tool/#+end_tool block."
  (let ((info (list :buffer nil
                    :claude-code-current-block "tool_use"
                    :claude-code-tools (list (list :id "tool-2" :name "Grep")
                                             (list :id "tool-1" :name "Grep")))))
    ;; Simulate tool 1 result arriving while tool 2 block is open
    (let ((result (gptel-claude-code--handle-user
                   (list :type "user"
                         :message
                         (list :content
                               (vector
                                (list :type "tool_result"
                                      :tool_use_id "tool-1"
                                      :content "file.el:10:match"))))
                   info)))
      ;; Should return nil (queued, not inserted immediately)
      (should (null result))
      ;; Should be queued in :claude-code-pending-results
      (should (stringp (plist-get info :claude-code-pending-results)))
      (should (string-match-p "begin_result Grep :id tool-1"
                              (plist-get info :claude-code-pending-results)))
      (should (string-match-p "file\\.el:10:match"
                              (plist-get info :claude-code-pending-results))))))

(ert-deftest gptel-claude-code-test-parallel-tool-result-flushed-on-block-stop ()
  "Test that queued tool results are flushed when a tool_use block closes."
  (let* ((pending-result "\n#+begin_result Grep :id tool-1\nfile.el:10:match\n#+end_result\n")
         (info (list :buffer nil
                     :claude-code-current-block "tool_use"
                     :partial_json (list "ern\": \"foo\"}" "{\"patt")
                     :claude-code-tools (list (list :id "tool-2" :name "Grep"))
                     :claude-code-pending-results pending-result)))
    (let ((result (gptel-claude-code--handle-stream-event
                   (list :type "stream_event"
                         :event (list :type "content_block_stop"))
                   info)))
      ;; Should include both the tool footer AND the flushed pending result
      (should (stringp result))
      (should (string-match-p "end_tool" result))
      (should (string-match-p "begin_result Grep :id tool-1" result))
      ;; Pending results should be cleared
      (should (null (plist-get info :claude-code-pending-results))))))

(ert-deftest gptel-claude-code-test-tool-result-not-queued-when-no-open-block ()
  "Test that tool results are returned immediately when no tool block is open."
  (let ((info (list :buffer nil
                    :claude-code-current-block nil
                    :claude-code-tools (list (list :id "tool-1" :name "Read")))))
    (let ((result (gptel-claude-code--handle-user
                   (list :type "user"
                         :message
                         (list :content
                               (vector
                                (list :type "tool_result"
                                      :tool_use_id "tool-1"
                                      :content "file contents"))))
                   info)))
      ;; Should return the result immediately (not queued)
      (should (stringp result))
      (should (string-match-p "begin_result Read :id tool-1" result))
      ;; No pending results
      (should (null (plist-get info :claude-code-pending-results))))))

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

(ert-deftest gptel-claude-code-test-handle-assistant-records-subagent-tools ()
  "Test assistant handler records tool_use blocks in :claude-code-tools.
Subagent tool calls arrive as full assistant messages (not stream_events).
The handler must record them so handle-user can match tool_result by ID."
  (let ((info (list :buffer nil :data nil))
        (msg (list :type "assistant"
                   :message (list :content
                                  (vector
                                   (list :type "tool_use"
                                         :id "tu-sub-1"
                                         :name "Grep"
                                         :input (list :pattern "foo" :path "/tmp"))
                                   (list :type "tool_use"
                                         :id "tu-sub-2"
                                         :name "Read"
                                         :input (list :file_path "/tmp/bar.txt")))))))
    (let ((result (gptel-claude-code--handle-assistant msg info)))
      ;; Should record both tool entries
      (let ((tools (plist-get info :claude-code-tools)))
        (should (= 2 (length tools)))
        ;; Most recent push is first (Read was pushed last)
        (should (equal "Read" (plist-get (car tools) :name)))
        (should (equal "tu-sub-2" (plist-get (car tools) :id)))
        (should (equal "Grep" (plist-get (cadr tools) :name)))
        (should (equal "tu-sub-1" (plist-get (cadr tools) :id))))
      ;; Should return formatted display string with tool blocks
      (should (stringp result))
      (should (cl-search "#+begin_tool Grep" result))
      (should (cl-search "#+begin_tool Read" result))
      (should (cl-search "#+end_tool" result)))))

(ert-deftest gptel-claude-code-test-subagent-tool-result-resolves-name ()
  "Test that tool_result from subagent resolves name via assistant-recorded tools.
Simulates the full flow: assistant message records tool_use, then user message
with tool_result looks up the name by tool_use_id."
  (let ((info (list :buffer nil :data nil)))
    ;; Step 1: assistant message with subagent's tool_use
    (gptel-claude-code--handle-assistant
     (list :type "assistant"
           :message (list :content
                          (vector
                           (list :type "tool_use"
                                 :id "tu-grep-99"
                                 :name "Grep"
                                 :input (list :pattern "test")))))
     info)
    ;; Step 2: user message with tool_result referencing that id
    (let ((result (gptel-claude-code--handle-user
                   (list :type "user"
                         :message (list :content
                                        (vector
                                         (list :type "tool_result"
                                               :tool_use_id "tu-grep-99"
                                               :content "found 3 matches"))))
                   info)))
      ;; Should resolve to "Grep", not "unknown"
      (should (stringp result))
      (should (cl-search "#+begin_result Grep" result))
      (should-not (cl-search "unknown" result)))))

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

(ert-deftest gptel-claude-code-test-format-tool-input-agent ()
  "Test Agent tool input renders as key-value pairs, not JSON."
  (let ((result (gptel-claude-code--format-tool-input
                 "Agent"
                 (list :subagent_type "general-purpose"
                       :description "Answer math question"
                       :prompt "What is 2+2?"))))
    (should (string-match-p "subagent_type: general-purpose" result))
    (should (string-match-p "description: Answer math question" result))
    (should (string-match-p "prompt: What is 2\\+2\\?" result))
    ;; Should NOT look like JSON
    (should-not (string-match-p "{" result))))

(ert-deftest gptel-claude-code-test-format-tool-input-agent-with-optional ()
  "Test Agent tool input includes optional fields when present."
  (let ((result (gptel-claude-code--format-tool-input
                 "Agent"
                 (list :subagent_type "Explore"
                       :description "Find files"
                       :prompt "Search for tests"
                       :model "haiku"
                       :name "researcher"))))
    (should (string-match-p "subagent_type: Explore" result))
    (should (string-match-p "model: haiku" result))
    (should (string-match-p "name: researcher" result))))

(ert-deftest gptel-claude-code-test-format-tool-input-skill ()
  "Test Skill tool input extracts skill name."
  (should (equal (gptel-claude-code--format-tool-input
                  "Skill" (list :skill "superpowers:systematic-debugging"))
                 "superpowers:systematic-debugging"))
  (should (equal (gptel-claude-code--format-tool-input
                  "Skill" (list :skill "commit"))
                 "commit"))
  (should (equal (gptel-claude-code--format-tool-input "Skill" nil) "")))

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
                   :mcp-port 18684
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
                   :mcp-port 18684
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
                   :mcp-port 18684
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
                   :mcp-port 18684
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
                   :mcp-port 18684
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

(ert-deftest gptel-claude-code-test-build-args-skip-permissions ()
  "Test --dangerously-skip-permissions flag injection."
  (let* ((backend (gptel--make-claude-code
                   :name "test"
                   :host "localhost"
                   :models (gptel--process-models '(claude-sonnet-4-20250514))
                   :default-flags '("--print" "--output-format" "stream-json")))
         (gptel-model 'claude-sonnet-4-20250514)
         (gptel--system-message nil))
    (cl-letf (((symbol-function 'gptel-claude-code--session-state)
               (lambda (_b) (list :state :new :session-id "test-123")))
              ((symbol-function 'gptel-claude-code--mcp-config-json)
               (lambda (sid port)
                 (format "{\"sid\":\"%s\",\"port\":%d}" sid port))))
      ;; When nil, flag should NOT be present
      (let ((gptel-claude-code-skip-permissions nil))
        (let ((args (gptel-claude-code--build-args nil backend)))
          (should-not (member "--dangerously-skip-permissions" args))))
      ;; When non-nil, flag should be present
      (let ((gptel-claude-code-skip-permissions t))
        (let ((args (gptel-claude-code--build-args nil backend)))
          (should (member "--dangerously-skip-permissions" args)))))))

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
                   :mcp-port 18684
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
  (let ((json (gptel-claude-code--mcp-config-json "sess-123" 18684)))
    ;; Should be valid JSON-like string
    (should (stringp json))
    ;; Should contain mcpServers
    (should (string-match-p "mcpServers" json))
    ;; Should contain the session id in the URL
    (should (string-match-p "sess-123" json))
    ;; Should contain the port
    (should (string-match-p "18684" json))
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

(ert-deftest gptel-claude-code-test-session-cwd ()
  "Test querying cwd by session ID."
  (let ((gptel-claude-code--session-map (make-hash-table :test 'equal))
        (buf (generate-new-buffer " *test-session-cwd*")))
    (unwind-protect
        (progn
          ;; Set cwd in the buffer
          (with-current-buffer buf
            (setq-local gptel-claude-code--team-cwd "/tmp/test-project"))
          ;; Register session
          (gptel-claude-code--register-session "sess-cwd-001" buf)
          ;; Should return the cwd
          (should (equal (gptel-claude-code--session-cwd "sess-cwd-001")
                         "/tmp/test-project"))
          ;; Unknown session should return nil
          (should (null (gptel-claude-code--session-cwd "unknown"))))
      (kill-buffer buf))))

(ert-deftest gptel-claude-code-test-session-cwd-nil ()
  "Test session-cwd returns nil when cwd not set."
  (let ((gptel-claude-code--session-map (make-hash-table :test 'equal))
        (buf (generate-new-buffer " *test-session-cwd-nil*")))
    (unwind-protect
        (progn
          (gptel-claude-code--register-session "sess-cwd-002" buf)
          ;; cwd not set, should return nil
          (should (null (gptel-claude-code--session-cwd "sess-cwd-002"))))
      (kill-buffer buf))))

(ert-deftest gptel-claude-code-test-session-cwd-dead-buffer ()
  "Test session-cwd returns nil when buffer is dead."
  (let ((gptel-claude-code--session-map (make-hash-table :test 'equal))
        (buf (generate-new-buffer " *test-session-cwd-dead*")))
    (with-current-buffer buf
      (setq-local gptel-claude-code--team-cwd "/tmp/dead"))
    (gptel-claude-code--register-session "sess-cwd-003" buf)
    (kill-buffer buf)
    ;; Buffer is dead, should return nil
    (should (null (gptel-claude-code--session-cwd "sess-cwd-003")))))

;;; ============================================================
;;; MCP default-directory function tests
;;; ============================================================

(ert-deftest gptel-claude-code-test-mcp-default-directory ()
  "Test mcp-default-directory returns session cwd for given session-id."
  (let ((gptel-claude-code--session-map (make-hash-table :test 'equal))
        (buf (generate-new-buffer " *test-mcp-dir*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local gptel-claude-code--team-cwd "/tmp/project-a"))
          (gptel-claude-code--register-session "sess-dir-001" buf)
          (should (equal "/tmp/project-a"
                         (gptel-claude-code--mcp-default-directory "sess-dir-001"))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest gptel-claude-code-test-mcp-default-directory-no-session ()
  "Test mcp-default-directory returns nil when session-id is nil."
  (should (null (gptel-claude-code--mcp-default-directory nil))))

(ert-deftest gptel-claude-code-test-mcp-default-directory-no-cwd ()
  "Test mcp-default-directory returns nil when session has no cwd."
  (let ((gptel-claude-code--session-map (make-hash-table :test 'equal))
        (buf (generate-new-buffer " *test-mcp-dir-nocwd*")))
    (unwind-protect
        (progn
          (gptel-claude-code--register-session "sess-dir-002" buf)
          ;; No cwd set on buffer
          (should (null (gptel-claude-code--mcp-default-directory "sess-dir-002"))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

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
    (should (equal (gptel-claude-code-mcp-port backend) 18684))
    (should (member "--print" (gptel-claude-code-default-flags backend)))
    (should (member "--verbose" (gptel-claude-code-default-flags backend)))))

;;; ============================================================
;;; Request data tests
;;; ============================================================

(ert-deftest gptel-claude-code-test-request-data-string ()
  "Test gptel--request-data extracts last user message as plist with :prompt."
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
      ;; Should return a plist with :prompt containing the last user message
      (should (listp result))
      (should (equal (plist-get result :prompt) "Now multiply by 3"))
      (should (plist-member result :system-message)))))

(ert-deftest gptel-claude-code-test-request-data-empty ()
  "Test gptel--request-data with no user messages returns plist with empty prompt."
  (let ((backend (gptel--make-claude-code
                  :name "test"
                  :host "localhost"
                  :protocol "file"
                  :endpoint ""
                  :stream t
                  :models nil)))
    (let ((result (gptel--request-data backend nil)))
      (should (listp result))
      (should (equal (plist-get result :prompt) ""))
      (should (plist-member result :system-message)))))

(ert-deftest gptel-claude-code-test-request-data-captures-system-message ()
  "Test gptel--request-data captures augmented system message from data buffer."
  (let ((backend (gptel--make-claude-code
                  :name "test"
                  :host "localhost"
                  :protocol "file"
                  :endpoint ""
                  :stream t
                  :models nil))
        ;; Simulate context-augmented system message in data buffer
        (gptel--system-message "Context: file.txt\n\nYou are helpful."))
    (let ((result (gptel--request-data
                   backend
                   (list (list :role "user" :content "hello")))))
      (should (equal (plist-get result :system-message)
                     "Context: file.txt\n\nYou are helpful.")))))

(ert-deftest gptel-claude-code-test-build-args-uses-data-system-message ()
  "Test build-args prefers system message from info :data over chat buffer."
  (let* ((backend (gptel--make-claude-code
                   :name "test"
                   :host "localhost"
                   :protocol "file"
                   :endpoint ""
                   :stream t
                   :models '("haiku")))
         (gptel-model 'haiku)
         (gptel--system-message "original system msg")
         (info (list :buffer (current-buffer)
                     :model 'haiku
                     :data (list :prompt "hello"
                                 :system-message "Context injected\n\noriginal system msg"))))
    (let ((args (gptel-claude-code--build-args info backend)))
      ;; Should use the augmented system message from :data
      (let ((sys-idx (cl-position "--append-system-prompt" args :test #'equal)))
        (should sys-idx)
        (should (equal (nth (1+ sys-idx) args)
                       "Context injected\n\noriginal system msg"))))))

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

;;; ============================================================
;;; Permission prompt / MCP tool tests
;;; ============================================================

(ert-deftest gptel-claude-code-test-format-tool-input-summary-bash ()
  "Test format-tool-input-summary for Bash tool."
  (should (equal (gptel-claude-code--format-tool-input-summary
                  "Bash" '(:command "echo hello"))
                 "$ echo hello")))

(ert-deftest gptel-claude-code-test-format-tool-input-summary-write ()
  "Test format-tool-input-summary for Write tool with file_path."
  (should (equal (gptel-claude-code--format-tool-input-summary
                  "Write" '(:file_path "/tmp/foo.txt"))
                 "/tmp/foo.txt")))

(ert-deftest gptel-claude-code-test-format-tool-input-summary-read ()
  "Test format-tool-input-summary for Read tool with file_path."
  (should (equal (gptel-claude-code--format-tool-input-summary
                  "Read" '(:file_path "/home/user/bar.el"))
                 "/home/user/bar.el")))

(ert-deftest gptel-claude-code-test-format-tool-input-summary-edit ()
  "Test format-tool-input-summary for Edit tool with file_path."
  (should (equal (gptel-claude-code--format-tool-input-summary
                  "Edit" '(:file_path "/tmp/edit.el"))
                 "/tmp/edit.el")))

(ert-deftest gptel-claude-code-test-format-tool-input-summary-nil ()
  "Test format-tool-input-summary with nil input."
  (should (equal (gptel-claude-code--format-tool-input-summary
                  "Unknown" nil)
                 "")))

(ert-deftest gptel-claude-code-test-format-tool-input-summary-agent ()
  "Test format-tool-input-summary for Agent tool shows type and description."
  (should (equal (gptel-claude-code--format-tool-input-summary
                  "Agent" '(:subagent_type "general-purpose"
                             :description "Answer math question"
                             :prompt "What is 2+2?"))
                 "[general-purpose] Answer math question")))

(ert-deftest gptel-claude-code-test-format-tool-input-summary-other ()
  "Test format-tool-input-summary for unknown tool falls back to JSON."
  (let ((result (gptel-claude-code--format-tool-input-summary
                 "Grep" '(:pattern "foo" :path "/tmp"))))
    (should (stringp result))
    (should (string-match-p "foo" result))
    (should (string-match-p "/tmp" result))))

(ert-deftest gptel-claude-code-test-permission-build-response-allow ()
  "Test permission-build-response for allow includes updatedInput.
Response is flat: {behavior, updatedInput} — no hookSpecificOutput wrapping."
  (let* ((tool-input '((command . "echo hi")))
         (result (gptel-claude-code--permission-build-response "allow" tool-input))
         (parsed (json-read-from-string result)))
    (should (equal (alist-get 'behavior parsed) "allow"))
    ;; updatedInput must be present for allow
    (should (alist-get 'updatedInput parsed))
    (should (equal (alist-get 'command (alist-get 'updatedInput parsed))
                   "echo hi"))
    ;; No hookSpecificOutput wrapping
    (should-not (alist-get 'hookSpecificOutput parsed))))

(ert-deftest gptel-claude-code-test-permission-build-response-deny ()
  "Test permission-build-response for deny includes message.
Response is flat: {behavior, message} — no hookSpecificOutput wrapping."
  (let* ((result (gptel-claude-code--permission-build-response "deny"))
         (parsed (json-read-from-string result)))
    (should (equal (alist-get 'behavior parsed) "deny"))
    ;; message must be present for deny
    (should (stringp (alist-get 'message parsed)))
    ;; updatedInput should NOT be present for deny
    (should-not (alist-get 'updatedInput parsed))
    ;; No hookSpecificOutput wrapping
    (should-not (alist-get 'hookSpecificOutput parsed))))

(ert-deftest gptel-claude-code-test-permission-param-mapping ()
  "Test that positional args are correctly mapped to named parameters."
  (let* ((param-names '(session_id transcript_path cwd permission_mode
                                    hook_event_name tool_name tool_input
                                    permission_suggestions))
         (args '("sess-1" "/tmp/t" "/tmp" "default"
                 "PermissionRequest" "Bash"
                 ((command . "echo hi")) nil))
         (params (cl-mapcar #'cons param-names args)))
    (should (equal (cdr (assq 'session_id params)) "sess-1"))
    (should (equal (cdr (assq 'tool_name params)) "Bash"))
    (should (equal (cdr (assq 'hook_event_name params)) "PermissionRequest"))
    (should (equal (cdr (assq 'tool_input params)) '((command . "echo hi"))))
    (should (null (cdr (assq 'permission_suggestions params))))))

(ert-deftest gptel-claude-code-test-permission-handler-stores-state ()
  "Test that the handler stores state, shows popup, and enters recursive-edit.
Mock recursive-edit to simulate immediate user response."
  (let* ((captured-result nil)
         (callback (lambda (result) (setq captured-result result)))
         (gptel-claude-code--permission-pending nil)
         (gptel-claude-code--permission-in-recursive-edit nil)
         (popup-shown nil))
    ;; Mock recursive-edit: simulate user pressing 'a' (allow) immediately.
    ;; Also mock show-permission-popup to avoid buffer creation.
    (cl-letf (((symbol-function 'recursive-edit)
               (lambda ()
                 ;; State should already be stored before recursive-edit
                 (should gptel-claude-code--permission-pending)
                 (should (equal (plist-get gptel-claude-code--permission-pending :tool-name) "Bash"))
                 (should (equal (plist-get gptel-claude-code--permission-pending :input-summary) "$ echo test"))
                 ;; Simulate user pressing 'a'
                 (cl-letf (((symbol-function 'gptel-claude-code--cleanup-permission-buffer) #'ignore)
                           ((symbol-function 'exit-recursive-edit) #'ignore))
                   (gptel-claude-code--permission-respond t))))
              ((symbol-function 'gptel-claude-code--show-permission-popup)
               (lambda (_tn _is) (setq popup-shown t))))
      (gptel-claude-code--handle-permission-prompt
       callback
       "sess-1" "/tmp/t" "/tmp" "default"
       "PermissionRequest" "Bash"
       '((command . "echo test")) nil))
    ;; Popup was shown
    (should popup-shown)
    ;; Callback should have been called with allow + updatedInput (flat format)
    (should captured-result)
    (let* ((parsed (json-read-from-string captured-result)))
      (should (equal (alist-get 'behavior parsed) "allow"))
      (should (alist-get 'updatedInput parsed))
      (should (equal (alist-get 'command (alist-get 'updatedInput parsed))
                     "echo test")))
    (should-not gptel-claude-code--permission-pending)))

(ert-deftest gptel-claude-code-test-permission-handler-deny ()
  "Test that deny sends the correct response."
  (let* ((captured-result nil)
         (callback (lambda (result) (setq captured-result result)))
         (gptel-claude-code--permission-pending nil)
         (gptel-claude-code--permission-in-recursive-edit nil))
    ;; Mock recursive-edit: simulate user pressing 'd' (deny)
    (cl-letf (((symbol-function 'recursive-edit)
               (lambda ()
                 (cl-letf (((symbol-function 'gptel-claude-code--cleanup-permission-buffer) #'ignore)
                           ((symbol-function 'exit-recursive-edit) #'ignore))
                   (gptel-claude-code--permission-respond nil))))
              ((symbol-function 'gptel-claude-code--show-permission-popup)
               (lambda (_tn _is) nil)))
      (gptel-claude-code--handle-permission-prompt
       callback
       "sess-1" "/tmp/t" "/tmp" "default"
       "PermissionRequest" "Bash"
       '((command . "rm -rf /")) nil))
    (should captured-result)
    (let* ((parsed (json-read-from-string captured-result)))
      (should (equal (alist-get 'behavior parsed) "deny"))
      (should (stringp (alist-get 'message parsed))))
    (should-not gptel-claude-code--permission-pending)))

(ert-deftest gptel-claude-code-test-permission-handler-quit-denies ()
  "Test that C-g (quit) during recursive-edit sends deny."
  (let* ((captured-result nil)
         (callback (lambda (result) (setq captured-result result)))
         (gptel-claude-code--permission-pending nil)
         (gptel-claude-code--permission-in-recursive-edit nil))
    ;; Mock recursive-edit to signal quit (C-g)
    (cl-letf (((symbol-function 'recursive-edit)
               (lambda () (signal 'quit nil)))
              ((symbol-function 'gptel-claude-code--show-permission-popup)
               (lambda (_tn _is) nil))
              ((symbol-function 'gptel-claude-code--cleanup-permission-buffer)
               #'ignore))
      ;; quit propagates after unwind-protect cleanup, so catch it
      (condition-case nil
          (gptel-claude-code--handle-permission-prompt
           callback
           "sess-1" "/tmp/t" "/tmp" "default"
           "PermissionRequest" "Bash"
           '((command . "echo test")) nil)
        (quit nil)))
    ;; unwind-protect should have sent deny (flat format)
    (should captured-result)
    (let* ((parsed (json-read-from-string captured-result)))
      (should (equal (alist-get 'behavior parsed) "deny"))
      (should (stringp (alist-get 'message parsed))))
    (should-not gptel-claude-code--permission-pending)))

(ert-deftest gptel-claude-code-test-permission-respond-function ()
  "Test gptel-claude-code--permission-respond directly."
  (let* ((captured-result nil)
         (gptel-claude-code--permission-pending
          (list :tool-name "Bash"
                :tool-input '((command . "echo hi"))
                :input-summary "$ echo hi"
                :callback (lambda (result) (setq captured-result result))))
         ;; Not inside recursive-edit, so exit-recursive-edit won't be called
         (gptel-claude-code--permission-in-recursive-edit nil))
    (cl-letf (((symbol-function 'gptel-claude-code--cleanup-permission-buffer) #'ignore))
      (gptel-claude-code--permission-respond t))
    (should captured-result)
    (let* ((parsed (json-read-from-string captured-result)))
      (should (equal (alist-get 'behavior parsed) "allow"))
      (should (alist-get 'updatedInput parsed))
      (should (equal (alist-get 'command (alist-get 'updatedInput parsed))
                     "echo hi")))
    (should-not gptel-claude-code--permission-pending)))

;;; ============================================================
;;; Team transcript-dir slug tests
;;; ============================================================

(ert-deftest gptel-claude-code-test-transcript-dir-basic ()
  "Test transcript dir computation with basic path."
  (should (equal (gptel-claude-code--transcript-dir "/Users/foo/bar" "sess-123")
                 (expand-file-name
                  "-Users-foo-bar/sess-123/subagents/"
                  "~/.claude/projects/"))))

(ert-deftest gptel-claude-code-test-transcript-dir-dot-in-path ()
  "Test transcript dir replaces dots with dashes (matches Claude Code)."
  (should (equal (gptel-claude-code--transcript-dir
                  "/Users/yqrashawn/.nixpkgs" "sess-abc")
                 (expand-file-name
                  "-Users-yqrashawn--nixpkgs/sess-abc/subagents/"
                  "~/.claude/projects/"))))

(ert-deftest gptel-claude-code-test-transcript-dir-trailing-slash ()
  "Test transcript dir strips trailing slash before slugifying."
  (should (equal (gptel-claude-code--transcript-dir
                  "/Users/yqrashawn/.nixpkgs/" "sess-abc")
                 (gptel-claude-code--transcript-dir
                  "/Users/yqrashawn/.nixpkgs" "sess-abc"))))

(ert-deftest gptel-claude-code-test-transcript-dir-multiple-dots ()
  "Test transcript dir with multiple dot-prefixed components."
  (should (equal (gptel-claude-code--transcript-dir
                  "/Users/yqrashawn/.emacs.d/.local/straight/repos/mcp-server-lib.el"
                  "sess-xyz")
                 (expand-file-name
                  "-Users-yqrashawn--emacs-d--local-straight-repos-mcp-server-lib-el/sess-xyz/subagents/"
                  "~/.claude/projects/"))))

;;; ============================================================
;;; Sentinel cleanup tests
;;; ============================================================

(ert-deftest gptel-claude-code-test-sentinel-cleanup-on-fsm-error ()
  "Sentinel must clean up request alist even if FSM transition errors.
This prevents stale entries from accumulating and 'Typing...' from
persisting forever."
  (let* ((gptel--request-alist nil)
         (proc-buf (generate-new-buffer " *test-proc-buf*"))
         ;; Create a mock process
         (proc (start-process "test-sentinel" proc-buf "true"))
         ;; Create a minimal FSM-like structure
         (test-buf (generate-new-buffer " *test-chat*"))
         (info (list :http-status "200"
                     :buffer test-buf
                     :callback #'ignore
                     :position (with-current-buffer test-buf (point-min-marker))))
         (fsm (record 'gptel-fsm 'TYPE
                      ;; Transition table: TYPE always goes to BADSTATE
                      '((TYPE (t . BADSTATE))
                        (BADSTATE))
                      ;; Handlers: BADSTATE handler errors
                      `((BADSTATE ,(lambda (_fsm) (error "Test FSM error"))))
                      info)))
    ;; Register in the request alist
    (setf (alist-get proc gptel--request-alist) (cons fsm #'ignore))
    (should (= 1 (length gptel--request-alist)))
    ;; Wait for process to exit (use proc to avoid triggering other process filters)
    (while (process-live-p proc) (accept-process-output proc 0.01))
    ;; Call sentinel -- FSM handler errors, but with-demoted-errors +
    ;; unwind-protect ensures cleanup still happens.
    (gptel-claude-code--stream-cleanup proc "finished\n")
    ;; Entry must be removed despite the error
    (should (= 0 (length gptel--request-alist)))
    ;; Process buffer should be killed
    (should-not (buffer-live-p proc-buf))
    ;; Clean up test buffer
    (when (buffer-live-p test-buf) (kill-buffer test-buf))))

(ert-deftest gptel-claude-code-test-sentinel-cleanup-normal ()
  "Sentinel cleans up properly on normal completion."
  (let* ((gptel--request-alist nil)
         (proc-buf (generate-new-buffer " *test-proc-buf*"))
         (proc (start-process "test-sentinel" proc-buf "true"))
         (callback-called nil)
         (test-buf (generate-new-buffer " *test-chat*"))
         (info (list :http-status "200"
                     :buffer test-buf
                     :callback (lambda (_response _info) (setq callback-called t))
                     :position (with-current-buffer test-buf (point-min-marker))
                     :stop-reason "end_turn"))
         (fsm (record 'gptel-fsm 'TYPE
                      '((TYPE (t . DONE))
                        (DONE))
                      `((DONE ,(lambda (_fsm) nil)))
                      info)))
    (setf (alist-get proc gptel--request-alist) (cons fsm #'ignore))
    (while (process-live-p proc) (accept-process-output proc 0.01))
    (gptel-claude-code--stream-cleanup proc "finished\n")
    (should callback-called)
    (should (= 0 (length gptel--request-alist)))
    (should-not (buffer-live-p proc-buf))
    (when (buffer-live-p test-buf) (kill-buffer test-buf))))

;;; ============================================================
;;; Block folding tests
;;; ============================================================

(ert-deftest gptel-claude-code-test-fold-tool-block ()
  "Test that fold-blocks-in-region folds #+begin_tool blocks."
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_tool Bash\n$ ls -la\n#+end_tool\n")
    (let* ((marker (copy-marker (point)))
           (info (list :buffer (current-buffer)
                       :tracking-marker marker)))
      (gptel-claude-code--fold-blocks-in-region info (point-min))
      ;; After folding, the block content should be invisible
      (goto-char (point-min))
      (forward-line 1)
      (should (invisible-p (point))))))

(ert-deftest gptel-claude-code-test-fold-result-block ()
  "Test that fold-blocks-in-region folds #+begin_result blocks."
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_result Read\nfile contents here\n#+end_result\n")
    (let* ((marker (copy-marker (point)))
           (info (list :buffer (current-buffer)
                       :tracking-marker marker)))
      (gptel-claude-code--fold-blocks-in-region info (point-min))
      (goto-char (point-min))
      (forward-line 1)
      (should (invisible-p (point))))))

(ert-deftest gptel-claude-code-test-fold-multiple-blocks ()
  "Test that fold-blocks-in-region folds multiple blocks."
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_tool Bash\n$ ls\n#+end_tool\n")
    (insert "#+begin_result Bash\noutput\n#+end_result\n")
    (let* ((marker (copy-marker (point)))
           (info (list :buffer (current-buffer)
                       :tracking-marker marker)))
      (gptel-claude-code--fold-blocks-in-region info (point-min))
      ;; Both blocks should be folded
      (goto-char (point-min))
      (forward-line 1)
      (should (invisible-p (point)))
      (goto-char (point-min))
      (search-forward "#+begin_result")
      (forward-line 1)
      (should (invisible-p (point))))))

(ert-deftest gptel-claude-code-test-fold-respects-start-boundary ()
  "Test that fold only affects blocks after START position."
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_tool Read\nold block\n#+end_tool\n")
    (let ((mid (point)))
      (insert "#+begin_tool Bash\nnew block\n#+end_tool\n")
      (let* ((marker (copy-marker (point)))
             (info (list :buffer (current-buffer)
                         :tracking-marker marker)))
        (gptel-claude-code--fold-blocks-in-region info mid)
        ;; First block should NOT be folded (before start)
        (goto-char (point-min))
        (forward-line 1)
        (should-not (invisible-p (point)))
        ;; Second block SHOULD be folded
        (goto-char mid)
        (search-forward "#+begin_tool")
        (forward-line 1)
        (should (invisible-p (point)))))))

(ert-deftest gptel-claude-code-test-fold-skips-non-org ()
  "Test that fold is a no-op in non-org-mode buffers."
  (with-temp-buffer
    (insert "#+begin_tool Bash\n$ ls\n#+end_tool\n")
    (let* ((marker (copy-marker (point)))
           (info (list :buffer (current-buffer)
                       :tracking-marker marker)))
      (gptel-claude-code--fold-blocks-in-region info (point-min))
      ;; Should not error and content should remain visible
      (goto-char (point-min))
      (forward-line 1)
      (should-not (invisible-p (point))))))

;;; ============================================================
;;; Logging tests
;;; ============================================================

(ert-deftest gptel-claude-code-test-stream-filter-logs-json-at-debug ()
  "Stream filter logs each JSON line to *gptel-log* at debug level."
  (let* ((gptel-log-level 'debug)
         (gptel--request-alist nil)
         (log-buf (get-buffer-create gptel--log-buffer-name))
         (proc-buf (generate-new-buffer " *test-proc-buf*"))
         (proc (start-process "test-log" proc-buf "true"))
         (test-buf (generate-new-buffer " *test-chat-log*"))
         (info (list :buffer test-buf
                     :callback #'ignore
                     :position (with-current-buffer test-buf (point-min-marker))))
         (fsm (record 'gptel-fsm 'TYPE
                      '((TYPE (t . DONE)) (DONE))
                      `((DONE ,(lambda (_fsm) nil)))
                      info)))
    (setf (alist-get proc gptel--request-alist) (cons fsm #'ignore))
    ;; Clear log buffer
    (with-current-buffer log-buf (erase-buffer))
    ;; Feed a system init JSON line through the stream filter
    (let ((json-line "{\"type\":\"system\",\"subtype\":\"init\",\"session_id\":\"test-log-sid\",\"model\":\"claude-sonnet-4-20250514\"}\n"))
      (gptel-claude-code--stream-filter proc json-line))
    ;; Check log buffer contains the JSON
    (with-current-buffer log-buf
      (should (string-match-p "test-log-sid" (buffer-string)))
      (should (string-match-p "Claude Code stream" (buffer-string))))
    ;; Cleanup
    (when (process-live-p proc) (delete-process proc))
    (when (buffer-live-p proc-buf) (kill-buffer proc-buf))
    (when (buffer-live-p test-buf) (kill-buffer test-buf))))

(ert-deftest gptel-claude-code-test-stream-filter-no-log-without-debug ()
  "Stream filter does NOT log JSON lines when gptel-log-level is nil."
  (let* ((gptel-log-level nil)
         (gptel--request-alist nil)
         (log-buf (get-buffer-create gptel--log-buffer-name))
         (proc-buf (generate-new-buffer " *test-proc-buf*"))
         (proc (start-process "test-nolog" proc-buf "true"))
         (test-buf (generate-new-buffer " *test-chat-nolog*"))
         (info (list :buffer test-buf
                     :callback #'ignore
                     :position (with-current-buffer test-buf (point-min-marker))))
         (fsm (record 'gptel-fsm 'TYPE
                      '((TYPE (t . DONE)) (DONE))
                      `((DONE ,(lambda (_fsm) nil)))
                      info)))
    (setf (alist-get proc gptel--request-alist) (cons fsm #'ignore))
    ;; Clear log buffer and record starting content
    (with-current-buffer log-buf (erase-buffer))
    ;; Feed a text delta JSON line
    (let ((json-line "{\"type\":\"stream_event\",\"event\":{\"type\":\"content_block_delta\",\"delta\":{\"type\":\"text_delta\",\"text\":\"hello\"}}}\n"))
      (gptel-claude-code--stream-filter proc json-line))
    ;; Log buffer should NOT contain "Claude Code stream"
    (with-current-buffer log-buf
      (should-not (string-match-p "Claude Code stream" (buffer-string))))
    ;; Cleanup
    (when (process-live-p proc) (delete-process proc))
    (when (buffer-live-p proc-buf) (kill-buffer proc-buf))
    (when (buffer-live-p test-buf) (kill-buffer test-buf))))

(ert-deftest gptel-claude-code-test-sentinel-logs-response-body ()
  "Sentinel logs full response body at info level."
  (let* ((gptel-log-level 'info)
         (gptel--request-alist nil)
         (log-buf (get-buffer-create gptel--log-buffer-name))
         (proc-buf (generate-new-buffer " *test-proc-resp*"))
         (proc (start-process "test-resp-log" proc-buf "true"))
         (test-buf (generate-new-buffer " *test-chat-resp*"))
         (info (list :http-status "200"
                     :buffer test-buf
                     :callback (lambda (_r _i &rest _) nil)
                     :position (with-current-buffer test-buf (point-min-marker))
                     :stop-reason "end_turn"))
         (fsm (record 'gptel-fsm 'TYPE
                      '((TYPE (t . DONE)) (DONE))
                      `((DONE ,(lambda (_fsm) nil)))
                      info)))
    (setf (alist-get proc gptel--request-alist) (cons fsm #'ignore))
    ;; Insert some content into the process buffer as if streamed
    (with-current-buffer proc-buf
      (insert "{\"type\":\"system\",\"subtype\":\"init\",\"session_id\":\"resp-log-test\"}\n")
      (insert "{\"type\":\"result\",\"subtype\":\"success\"}\n"))
    ;; Clear log buffer
    (with-current-buffer log-buf (erase-buffer))
    ;; Wait for process to exit
    (while (process-live-p proc) (accept-process-output proc 0.01))
    ;; Call sentinel
    (gptel-claude-code--stream-cleanup proc "finished\n")
    ;; Check that response body was logged
    (with-current-buffer log-buf
      (should (string-match-p "response Claude Code body" (buffer-string)))
      (should (string-match-p "resp-log-test" (buffer-string))))
    ;; Cleanup
    (when (buffer-live-p test-buf) (kill-buffer test-buf))))

(ert-deftest gptel-claude-code-test-sentinel-no-log-when-nil ()
  "Sentinel does NOT log response body when gptel-log-level is nil."
  (let* ((gptel-log-level nil)
         (gptel--request-alist nil)
         (log-buf (get-buffer-create gptel--log-buffer-name))
         (proc-buf (generate-new-buffer " *test-proc-noresp*"))
         (proc (start-process "test-noresp-log" proc-buf "true"))
         (test-buf (generate-new-buffer " *test-chat-noresp*"))
         (info (list :http-status "200"
                     :buffer test-buf
                     :callback (lambda (_r _i &rest _) nil)
                     :position (with-current-buffer test-buf (point-min-marker))
                     :stop-reason "end_turn"))
         (fsm (record 'gptel-fsm 'TYPE
                      '((TYPE (t . DONE)) (DONE))
                      `((DONE ,(lambda (_fsm) nil)))
                      info)))
    (setf (alist-get proc gptel--request-alist) (cons fsm #'ignore))
    (with-current-buffer proc-buf
      (insert "{\"type\":\"result\",\"subtype\":\"success\"}\n"))
    ;; Clear log buffer
    (with-current-buffer log-buf (erase-buffer))
    (while (process-live-p proc) (accept-process-output proc 0.01))
    (gptel-claude-code--stream-cleanup proc "finished\n")
    ;; Should NOT contain Claude Code body log
    (with-current-buffer log-buf
      (should-not (string-match-p "response Claude Code body" (buffer-string))))
    (when (buffer-live-p test-buf) (kill-buffer test-buf))))

(ert-deftest gptel-claude-code-test-sentinel-sets-status-on-failure ()
  "Sentinel sets :status when process exits without init message.
Without :status, gptel--handle-error skips its body and the
header-line stays stuck on 'Typing...' forever."
  (let* ((gptel-log-level nil)
         (gptel--request-alist nil)
         (proc-buf (generate-new-buffer " *test-proc-status*"))
         (proc (start-process "test-status" proc-buf "true"))
         (test-buf (generate-new-buffer " *test-chat-status*"))
         (callback-called nil)
         (info (list :buffer test-buf
                     :callback (lambda (_r _i &rest _)
                                 (setq callback-called t))
                     :position (with-current-buffer test-buf (point-min-marker))))
         (fsm (record 'gptel-fsm 'TYPE
                      '((TYPE (t . DONE)) (DONE))
                      `((DONE ,(lambda (_fsm) nil)))
                      info)))
    (setf (alist-get proc gptel--request-alist) (cons fsm #'ignore))
    ;; No content in proc buffer -- simulates process dying before init
    ;; Use process-specific accept to avoid triggering filters on other processes
    (while (process-live-p proc) (accept-process-output proc 0.01))
    (gptel-claude-code--stream-cleanup proc "finished\n")
    ;; :status must be set for gptel--handle-error to display the error
    (should (stringp (plist-get info :status)))
    (should (stringp (plist-get info :error)))
    (should callback-called)
    (when (buffer-live-p test-buf) (kill-buffer test-buf))))

(ert-deftest gptel-claude-code-test-sentinel-status-on-timeout ()
  "Timeout handler sets :status so ERRS state can display the error."
  (let* ((gptel-log-level nil)
         (gptel--request-alist nil)
         (proc-buf (generate-new-buffer " *test-proc-timeout*"))
         (proc (start-process "test-timeout" proc-buf "sleep" "10"))
         (test-buf (generate-new-buffer " *test-chat-timeout*"))
         (callback-called nil)
         (info (list :buffer test-buf
                     :http-status "200"
                     :callback (lambda (_r _i &rest _)
                                 (setq callback-called t))
                     :position (with-current-buffer test-buf (point-min-marker))))
         (fsm (record 'gptel-fsm 'TYPE
                      '((TYPE (t . DONE)) (DONE))
                      `((DONE ,(lambda (_fsm) nil)))
                      info)))
    (setf (alist-get proc gptel--request-alist) (cons fsm #'ignore))
    ;; Simulate what the timeout timer does
    (plist-put info :error "Claude Code query timed out")
    (plist-put info :status "timeout")
    (delete-process proc)
    ;; Wait for sentinel
    (while (process-live-p proc) (accept-process-output nil 0.01))
    (gptel-claude-code--stream-cleanup proc "killed\n")
    ;; :status must be "timeout" for gptel--handle-error
    (should (equal (plist-get info :status) "timeout"))
    (should (equal (plist-get info :error) "Claude Code query timed out"))
    (when (buffer-live-p test-buf) (kill-buffer test-buf))
    (when (buffer-live-p proc-buf) (kill-buffer proc-buf))))

;;; Unhandled type logging tests

(ert-deftest gptel-claude-code-test-log-unhandled-writes-to-file ()
  "Logging an unhandled type appends an entry to the log file."
  (let* ((tmp (make-temp-file "gptel-unhandled-" nil ".log"))
         (gptel-claude-code-unhandled-log-file tmp))
    (unwind-protect
        (progn
          (gptel-claude-code--log-unhandled "msg-type" "new_thing" "{\"type\":\"new_thing\"}")
          (let ((content (with-temp-buffer
                           (insert-file-contents tmp)
                           (buffer-string))))
            (should (string-match-p "msg-type" content))
            (should (string-match-p "new_thing" content))))
      (delete-file tmp))))

(ert-deftest gptel-claude-code-test-log-unhandled-creates-directory ()
  "Logging creates parent directory if it doesn't exist."
  (let* ((tmp-dir (make-temp-file "gptel-unhandled-dir-" t))
         (sub-dir (expand-file-name "sub/dir" tmp-dir))
         (gptel-claude-code-unhandled-log-file
          (expand-file-name "test.log" sub-dir)))
    (unwind-protect
        (progn
          (gptel-claude-code--log-unhandled "event-type" "foo" "data")
          (should (file-exists-p gptel-claude-code-unhandled-log-file)))
      (delete-directory tmp-dir t))))

(ert-deftest gptel-claude-code-test-log-unhandled-appends ()
  "Multiple log entries are appended, not overwritten."
  (let* ((tmp (make-temp-file "gptel-unhandled-" nil ".log"))
         (gptel-claude-code-unhandled-log-file tmp))
    (unwind-protect
        (progn
          (gptel-claude-code--log-unhandled "msg-type" "alpha" "data1")
          (gptel-claude-code--log-unhandled "delta-type" "beta" "data2")
          (let ((content (with-temp-buffer
                           (insert-file-contents tmp)
                           (buffer-string))))
            (should (string-match-p "alpha" content))
            (should (string-match-p "beta" content))))
      (delete-file tmp))))

(ert-deftest gptel-claude-code-test-stream-filter-logs-unhandled-msg-type ()
  "Stream filter logs unhandled top-level message types to file."
  (let* ((tmp (make-temp-file "gptel-unhandled-" nil ".log"))
         (gptel-claude-code-unhandled-log-file tmp)
         (gptel-log-level nil)
         (gptel--request-alist nil)
         (proc-buf (generate-new-buffer " *test-unhandled-msg*" t))
         (test-buf (generate-new-buffer "*test-unhandled-msg-buf*" t))
         (info (list :buffer test-buf
                     :callback #'ignore
                     :position (with-current-buffer test-buf (point-min-marker))))
         (fsm (record 'gptel-fsm 'TYPE
                      '((TYPE (t . DONE)) (DONE))
                      `((DONE ,(lambda (_fsm) nil)))
                      info))
         (proc (start-process "test-unhandled" proc-buf "true")))
    (unwind-protect
        (progn
          (setf (alist-get proc gptel--request-alist) (cons fsm #'ignore))
          ;; Send an unknown message type
          (gptel-claude-code--stream-filter
           proc "{\"type\":\"brand_new_type\",\"data\":\"hello\"}\n")
          ;; Should be logged to file
          (let ((content (with-temp-buffer
                           (insert-file-contents tmp)
                           (buffer-string))))
            (should (string-match-p "msg-type" content))
            (should (string-match-p "brand_new_type" content))))
      (when (process-live-p proc) (delete-process proc))
      (when (buffer-live-p proc-buf) (kill-buffer proc-buf))
      (when (buffer-live-p test-buf) (kill-buffer test-buf))
      (delete-file tmp))))

(ert-deftest gptel-claude-code-test-stream-event-logs-unhandled-event-type ()
  "handle-stream-event logs unhandled event types."
  (let* ((tmp (make-temp-file "gptel-unhandled-" nil ".log"))
         (gptel-claude-code-unhandled-log-file tmp)
         (info (list :buffer (current-buffer)))
         (msg `(:type "stream_event"
                :event (:type "completely_new_event" :data "test"))))
    (unwind-protect
        (progn
          (gptel-claude-code--handle-stream-event msg info)
          (let ((content (with-temp-buffer
                           (insert-file-contents tmp)
                           (buffer-string))))
            (should (string-match-p "event-type" content))
            (should (string-match-p "completely_new_event" content))))
      (delete-file tmp))))

(ert-deftest gptel-claude-code-test-stream-event-logs-unhandled-block-type ()
  "handle-stream-event logs unhandled content_block_start block types."
  (let* ((tmp (make-temp-file "gptel-unhandled-" nil ".log"))
         (gptel-claude-code-unhandled-log-file tmp)
         (info (list :buffer (current-buffer)))
         (msg `(:type "stream_event"
                :event (:type "content_block_start"
                        :content_block (:type "new_block_type" :data "x")))))
    (unwind-protect
        (progn
          (gptel-claude-code--handle-stream-event msg info)
          (let ((content (with-temp-buffer
                           (insert-file-contents tmp)
                           (buffer-string))))
            (should (string-match-p "block-type" content))
            (should (string-match-p "new_block_type" content))))
      (delete-file tmp))))

(ert-deftest gptel-claude-code-test-stream-event-logs-unhandled-delta-type ()
  "handle-stream-event logs unhandled content_block_delta delta types."
  (let* ((tmp (make-temp-file "gptel-unhandled-" nil ".log"))
         (gptel-claude-code-unhandled-log-file tmp)
         (info (list :buffer (current-buffer)))
         (msg `(:type "stream_event"
                :event (:type "content_block_delta"
                        :delta (:type "new_delta_type" :data "y")))))
    (unwind-protect
        (progn
          (gptel-claude-code--handle-stream-event msg info)
          (let ((content (with-temp-buffer
                           (insert-file-contents tmp)
                           (buffer-string))))
            (should (string-match-p "delta-type" content))
            (should (string-match-p "new_delta_type" content))))
      (delete-file tmp))))

(ert-deftest gptel-claude-code-test-known-event-types-not-logged ()
  "Known event types (message_start etc.) are NOT logged as unhandled."
  (let* ((tmp (make-temp-file "gptel-unhandled-" nil ".log"))
         (gptel-claude-code-unhandled-log-file tmp)
         (info (list :buffer (current-buffer))))
    (unwind-protect
        (progn
          ;; These are all known types that should NOT be logged
          (dolist (event-type '("content_block_start" "content_block_delta"
                                "content_block_stop" "message_start"
                                "message_delta" "message_stop"))
            (let ((msg `(:type "stream_event"
                         :event (:type ,event-type
                                 :content_block (:type "text")
                                 :delta (:type "text_delta" :text "hi")))))
              (gptel-claude-code--handle-stream-event msg info)))
          ;; File should not exist or be empty (no unhandled types logged)
          (should (or (not (file-exists-p tmp))
                      (= 0 (file-attribute-size (file-attributes tmp))))))
      (when (file-exists-p tmp) (delete-file tmp)))))

(provide 'gptel-claude-code-test)
;;; gptel-claude-code-test.el ends here
