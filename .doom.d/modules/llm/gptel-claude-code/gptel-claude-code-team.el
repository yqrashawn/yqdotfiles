;;; gptel-claude-code-team.el --- Teammate transcript watching for Claude Code  -*- lexical-binding: t; -*-

;; Author: yqrashawn
;; Keywords: tools, convenience

;;; Commentary:

;; When Claude Code spawns team teammates, their output goes to
;; transcript .jsonl files on disk (not stdout).  This module watches
;; those files with `file-notify' and incrementally parses new JSONL
;; content into dedicated Emacs buffers.
;;
;; Transcript file paths follow this pattern:
;;   ~/.claude/projects/{project-slug}/{session-id}/subagents/agent-{agent-id}.jsonl
;;
;; Where project-slug = cwd with "/" replaced by "-"
;; (e.g., /Users/foo/bar -> -Users-foo-bar)
;;
;; Teammate spawns are detected from `user' type messages in the
;; stream-json output that contain a `toolUseResult' with
;; `status: "teammate_spawned"'.

;;; Code:

(require 'cl-lib)
(require 'filenotify)

;; Forward declarations -- gptel core
(defvar gptel-log-level)
(declare-function gptel--log "gptel-request")
;; gptel--json-read-string is a macro from gptel, available at runtime
;; via the parent file's (require 'gptel)

;; Forward declarations -- stream module buffer-local vars
(defvar gptel-claude-code--session-id)

;;; Buffer-local state
;;
;; These variables are buffer-local in the gptel chat buffer.

(defvar-local gptel-claude-code--teammates nil
  "Alist of (AGENT-ID . INFO-PLIST) for tracked teammates.
Each INFO-PLIST contains at least :name, :agent-id, :teammate-id,
:agent-type, :model, :team-name.")

(defvar-local gptel-claude-code--file-watchers nil
  "List of file-notify descriptors for transcript watching.")

(defvar-local gptel-claude-code--transcript-bytes-read nil
  "Hash-table tracking bytes read per transcript file path.
Keys are absolute file paths, values are byte offsets.")

(defvar-local gptel-claude-code--team-cwd nil
  "Working directory for the current Claude Code process.
Used to compute the transcript directory.")

;;; Transcript directory computation

(defun gptel-claude-code--transcript-dir (cwd session-id)
  "Compute the transcript directory for subagents.
CWD is the working directory of the Claude Code process.
SESSION-ID is the session ID from the init message.
Returns an absolute path to the subagents directory."
  (let ((slug (replace-regexp-in-string "/" "-" cwd)))
    (expand-file-name
     (concat session-id "/subagents/")
     (expand-file-name slug "~/.claude/projects/"))))

;;; Teammate buffer management

(defun gptel-claude-code--get-teammate-buffer (name)
  "Return or create a buffer for teammate NAME.
Buffer is named `*gptel-teammate-NAME*' and uses `special-mode'."
  (let* ((buf-name (format "*gptel-teammate-%s*" name))
         (buf (get-buffer buf-name)))
    (unless buf
      (setq buf (get-buffer-create buf-name))
      (with-current-buffer buf
        (special-mode)
        (setq-local buffer-read-only t)))
    buf))

;;; JSONL incremental parser

(defun gptel-claude-code--parse-transcript-update (file-path gptel-buffer)
  "Incrementally parse new content from FILE-PATH.
GPTEL-BUFFER is the chat buffer that owns the teammate state.
Reads from the last known byte offset and processes new JSON lines."
  (when (and (file-exists-p file-path)
             (buffer-live-p gptel-buffer))
    (let* ((bytes-table (buffer-local-value
                         'gptel-claude-code--transcript-bytes-read
                         gptel-buffer))
           (bytes-read (or (and bytes-table (gethash file-path bytes-table)) 0))
           (file-size (file-attribute-size (file-attributes file-path)))
           ;; Extract agent-id from the file name
           ;; Pattern: agent-{agent-id}.jsonl
           (agent-id (when (string-match "agent-\\(.+\\)\\.jsonl\\'" file-path)
                       (match-string 1 file-path)))
           ;; Find teammate name from the teammates alist
           ;; Since transcript files use short hex IDs that don't match
           ;; spawn event agent_ids, we do a substring/fuzzy match
           (teammate-info (gptel-claude-code--find-teammate-by-file
                           agent-id gptel-buffer))
           (teammate-name (or (plist-get teammate-info :name)
                              (or agent-id "unknown"))))
      (when (and file-size (> file-size bytes-read))
        ;; Read new bytes from file
        (let ((new-content (with-temp-buffer
                             (set-buffer-multibyte nil)
                             (insert-file-contents-literally
                              file-path nil bytes-read file-size)
                             (buffer-string))))
          ;; Update bytes read
          (when (buffer-live-p gptel-buffer)
            (with-current-buffer gptel-buffer
              (unless gptel-claude-code--transcript-bytes-read
                (setq gptel-claude-code--transcript-bytes-read
                      (make-hash-table :test 'equal)))
              (puthash file-path file-size
                       gptel-claude-code--transcript-bytes-read)))
          ;; Parse each complete line
          (let ((lines (split-string new-content "\n" t))
                (teammate-buf (gptel-claude-code--get-teammate-buffer
                               teammate-name)))
            (dolist (line lines)
              (let ((trimmed (string-trim line)))
                (when (and (not (string-empty-p trimmed))
                           (eq (aref trimmed 0) ?{))
                  (condition-case err
                      (let* ((msg (gptel--json-read-string trimmed))
                             (msg-type (plist-get msg :type)))
                        (gptel-claude-code--handle-transcript-message
                         msg msg-type teammate-name teammate-buf))
                    (error
                     (when (eq gptel-log-level 'debug)
                       (gptel--log
                        (format "Team transcript parse error: %S\nLine: %s"
                                err trimmed)
                        "team parse error" 'no-json)))))))))))))

(defun gptel-claude-code--handle-transcript-message (msg msg-type name buf)
  "Handle a single parsed transcript message MSG.
MSG-TYPE is the message type string.
NAME is the teammate name for display.
BUF is the teammate display buffer."
  (when (buffer-live-p buf)
    (pcase msg-type
      ("assistant"
       ;; Extract text from message.content array
       (when-let* ((message (plist-get msg :message))
                   (content (plist-get message :content)))
         (when (vectorp content)
           (cl-loop for cblock across content
                    for ctype = (plist-get cblock :type)
                    do (when (equal ctype "text")
                         (let ((text (plist-get cblock :text)))
                           (when (and text (not (string-empty-p text)))
                             (gptel-claude-code--insert-in-teammate-buffer
                              buf text))))))))
      ("result"
       ;; Show completion summary
       (let* ((subtype (or (plist-get msg :subtype) "done"))
              (usage (plist-get msg :usage))
              (summary (format "\n--- %s finished (%s) ---\n"
                               name subtype)))
         (when usage
           (setq summary
                 (format "\n--- %s finished (%s) | tokens: %s in / %s out ---\n"
                         name subtype
                         (or (plist-get usage :input_tokens) "?")
                         (or (plist-get usage :output_tokens) "?"))))
         (gptel-claude-code--insert-in-teammate-buffer buf summary))))))

(defun gptel-claude-code--insert-in-teammate-buffer (buf text)
  "Insert TEXT into teammate buffer BUF, respecting read-only mode."
  (when (and (buffer-live-p buf) text)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert text)))))

(defun gptel-claude-code--find-teammate-by-file (agent-id gptel-buffer)
  "Find teammate info for AGENT-ID in GPTEL-BUFFER's teammates alist.
AGENT-ID is the short hex ID from the transcript filename.
Since transcript files use short hex IDs that don't match the spawn
event's agent_id, we try multiple matching strategies:
1. Exact match on agent-id
2. Exact match on teammate-id
3. Substring match on either"
  (when (and agent-id (buffer-live-p gptel-buffer))
    (let ((teammates (buffer-local-value
                      'gptel-claude-code--teammates gptel-buffer)))
      (or
       ;; 1. Exact match on agent-id
       (cdr (assoc agent-id teammates))
       ;; 2. Match on teammate-id field
       (cl-loop for (_id . info) in teammates
                when (equal agent-id (plist-get info :teammate-id))
                return info)
       ;; 3. Substring match (agent-id in teammate-id or vice versa)
       (cl-loop for (_id . info) in teammates
                for tid = (plist-get info :teammate-id)
                when (and tid
                          (or (string-match-p (regexp-quote agent-id) tid)
                              (string-match-p (regexp-quote tid) agent-id)))
                return info)))))

;;; File watcher setup

(defun gptel-claude-code--watch-transcripts (cwd session-id gptel-buffer)
  "Set up file watching for teammate transcripts.
CWD is the working directory of the Claude Code process.
SESSION-ID is the session ID from the init message.
GPTEL-BUFFER is the chat buffer to receive updates.

Creates the transcript directory if needed and installs a
`file-notify-add-watch' for create and change events."
  (when (and cwd session-id (buffer-live-p gptel-buffer))
    (let ((dir (gptel-claude-code--transcript-dir cwd session-id)))
      ;; Create directory if needed (mkdir -p equivalent)
      (unless (file-directory-p dir)
        (make-directory dir t))
      ;; Set up file watcher
      (condition-case err
          (let ((descriptor
                 (file-notify-add-watch
                  dir '(change)
                  (lambda (event)
                    (gptel-claude-code--transcript-notify-handler
                     event gptel-buffer)))))
            ;; Store watcher descriptor in gptel buffer
            (when (buffer-live-p gptel-buffer)
              (with-current-buffer gptel-buffer
                (push descriptor gptel-claude-code--file-watchers)))
            (when (eq gptel-log-level 'debug)
              (gptel--log (format "Watching transcript dir: %s" dir)
                          "team watcher" 'no-json)))
        (error
         (when (eq gptel-log-level 'debug)
           (gptel--log (format "Failed to watch %s: %S" dir err)
                       "team watcher error" 'no-json))))
      ;; Also process any existing files (in case teammates started
      ;; before we set up the watcher)
      (when (file-directory-p dir)
        (dolist (file (directory-files dir t "\\.jsonl\\'"))
          (gptel-claude-code--parse-transcript-update
           file gptel-buffer))))))

(defun gptel-claude-code--transcript-notify-handler (event gptel-buffer)
  "Handle a file-notify EVENT for transcript files.
GPTEL-BUFFER is the chat buffer owning this watcher."
  (when (and event (buffer-live-p gptel-buffer))
    (let ((action (cadr event))
          (file (caddr event)))
      (when (and file
                 (member action '(created changed))
                 (string-suffix-p ".jsonl" file))
        (gptel-claude-code--parse-transcript-update
         file gptel-buffer)))))

;;; Teammate detection from user messages

(defun gptel-claude-code--detect-teammate-spawn (msg info)
  "Detect teammate spawn events from a user-type message MSG.
INFO is the request info plist.

In Claude Code stream-json, teammate spawns appear as `user' type
messages with a top-level `toolUseResult' field having
`status: \"teammate_spawned\"'.

When detected: registers the teammate, starts file watcher if
this is the first teammate, and returns a display string.
Returns nil if MSG is not a teammate spawn."
  (when-let* ((tool-result (or (plist-get msg :toolUseResult)
                               (plist-get msg :tool_use_result)))
              (status (plist-get tool-result :status))
              (_ (equal status "teammate_spawned"))
              (name (plist-get tool-result :name))
              (agent-id (or (plist-get tool-result :agent_id) ""))
              (buf (plist-get info :buffer)))
    (let ((teammate-id (or (plist-get tool-result :teammate_id) ""))
          (agent-type (or (plist-get tool-result :agent_type) ""))
          (model (or (plist-get tool-result :model) ""))
          (team-name (or (plist-get tool-result :team_name) ""))
          (session-id nil)
          (cwd nil))
      ;; Build teammate info plist
      (let ((teammate-info (list :name name
                                 :agent-id agent-id
                                 :teammate-id teammate-id
                                 :agent-type agent-type
                                 :model model
                                 :team-name team-name)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            ;; Register teammate
            (push (cons agent-id teammate-info)
                  gptel-claude-code--teammates)
            ;; Get session-id and cwd for transcript watching
            (setq session-id gptel-claude-code--session-id)
            (setq cwd gptel-claude-code--team-cwd))
          ;; Start watching transcripts if we have session info
          ;; Only set up watcher once (on first teammate)
          (when (and session-id cwd
                     (= 1 (length (buffer-local-value
                                   'gptel-claude-code--teammates buf))))
            (gptel-claude-code--watch-transcripts cwd session-id buf))))
      ;; Return display string
      (format "\n#+team-spawned %s [%s %s]\n" name agent-type model))))

;;; Cleanup

(defun gptel-claude-code--cleanup-watchers (buffer)
  "Stop all file-notify watchers for BUFFER.
Removes all watcher descriptors and clears the watcher list.
Does not kill teammate buffers (user may want to keep them)."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((n-watchers (length gptel-claude-code--file-watchers)))
        ;; Remove file watchers
        (dolist (desc gptel-claude-code--file-watchers)
          (condition-case nil
              (file-notify-rm-watch desc)
            (error nil)))
        (setq gptel-claude-code--file-watchers nil)
        ;; Clear bytes-read tracking
        (when gptel-claude-code--transcript-bytes-read
          (clrhash gptel-claude-code--transcript-bytes-read))
        ;; Log cleanup (teammates list kept for reference)
        (when (and (eq gptel-log-level 'debug) (> n-watchers 0))
          (gptel--log (format "Cleaned up %d teammate watchers for %s"
                              n-watchers (buffer-name buffer))
                      "team cleanup" 'no-json))))))

(provide 'gptel-claude-code-team)
;;; gptel-claude-code-team.el ends here
