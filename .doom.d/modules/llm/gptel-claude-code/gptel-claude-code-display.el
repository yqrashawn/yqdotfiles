;;; gptel-claude-code-display.el --- Display formatting for Claude Code tool use  -*- lexical-binding: t; -*-

;; Author: yqrashawn
;; Keywords: tools, convenience

;;; Commentary:

;; Functions to format Claude Code's tool usage and thinking blocks as
;; org-mode markup in the gptel buffer.  When Claude Code uses built-in
;; tools (Bash, Edit, Read, etc.), we display them as read-only org
;; blocks so the user can see what the agent is doing.
;;
;; Tool use is displayed as:
;;
;;   #+begin_tool NAME
;;   FORMATTED_INPUT
;;   #+end_tool
;;
;; Tool results are displayed as:
;;
;;   #+begin_result NAME
;;   CONTENT
;;   #+end_result

;;; Code:

(require 'cl-lib)

;; Forward declarations
(declare-function json-encode "json")
(defvar json-encoding-pretty-print)

;;; Tool input formatting

(defun gptel-claude-code--format-tool-input (name input)
  "Format tool INPUT for display based on tool NAME.

Returns a human-readable string representation of the tool's
arguments.  Common tools get special formatting; others fall back
to JSON encoding."
  (unless input (setq input nil))
  (pcase name
    ("Bash"
     (let ((cmd (plist-get input :command)))
       (if cmd (format "$ %s" cmd) "")))

    ("Read"
     (let ((path (plist-get input :file_path)))
       (if path (format "%s" path) "")))

    ("Edit"
     (let ((path (plist-get input :file_path))
           (old (plist-get input :old_string))
           (new (plist-get input :new_string)))
       (concat
        (or path "")
        (when (and old new)
          (format "\n%s\n  ->\n%s"
                  (truncate-string-to-width old 200 nil nil "...")
                  (truncate-string-to-width new 200 nil nil "..."))))))

    ("MultiEdit"
     (let ((path (plist-get input :file_path)))
       (or path "")))

    ("Write"
     (let ((path (plist-get input :file_path)))
       (if path (format "%s" path) "")))

    ("Grep"
     (let ((pattern (plist-get input :pattern))
           (path (plist-get input :path)))
       (concat (or pattern "")
               (when path (format " in %s" path)))))

    ("Glob"
     (let ((pattern (plist-get input :pattern))
           (path (plist-get input :path)))
       (concat (or pattern "")
               (when path (format " in %s" path)))))

    ("TodoRead"
     "Reading task list")

    ("TodoWrite"
     (let ((todos (plist-get input :todos)))
       (if todos
           (format "%d task(s)" (length todos))
         "")))

    ("WebFetch"
     (let ((url (plist-get input :url)))
       (or url "")))

    ("WebSearch"
     (let ((query (plist-get input :query)))
       (or query "")))

    ("Skill"
     (let ((skill (plist-get input :skill)))
       (or skill "")))

    ("Agent"
     (let ((type (plist-get input :subagent_type))
           (desc (plist-get input :description))
           (prompt (plist-get input :prompt))
           (model (plist-get input :model))
           (name (plist-get input :name))
           (resume (plist-get input :resume)))
       (concat
        (when type (format "subagent_type: %s" type))
        (when desc (format "\ndescription: %s" desc))
        (when model (format "\nmodel: %s" model))
        (when name (format "\nname: %s" name))
        (when resume (format "\nresume: %s" resume))
        (when prompt (format "\nprompt: %s" prompt)))))

    (_
     ;; Default: JSON-encode the input
     (if input
         (condition-case nil
             (let ((json-encoding-pretty-print nil))
               (json-encode input))
           (error (format "%S" input)))
       ""))))

;;; Block formatting

(defun gptel-claude-code--format-tool-use (name input &optional id)
  "Format a tool_use block as an org drawer.

NAME is the tool name (e.g. \"Bash\", \"Read\").
INPUT is the parsed tool input plist, or nil if not yet available.
ID is the optional tool_use_id string.

Returns a string with org markup."
  (let ((formatted-input (gptel-claude-code--format-tool-input name input)))
    (concat "\n#+begin_tool " name
            (when id (concat " :id " id))
            "\n"
            (unless (string-empty-p formatted-input)
              (concat formatted-input "\n"))
            "#+end_tool\n")))

(defun gptel-claude-code--format-tool-result (name content is-error &optional id)
  "Format a tool result block as an org drawer.

NAME is the tool name.
CONTENT is the result text.
IS-ERROR is non-nil if the result represents an error.
ID is the optional tool_use_id string.

Returns a string with org markup."
  (let ((display-content (or content "")))
    (when is-error
      (setq display-content (concat "ERROR: " display-content)))
    ;; Truncate long results to keep the buffer manageable
    (when (> (length display-content) 2000)
      (setq display-content
            (concat (substring display-content 0 2000)
                    "\n... (truncated)")))
    (concat "\n#+begin_result " name
            (when id (concat " :id " id))
            "\n"
            display-content
            (unless (string-suffix-p "\n" display-content) "\n")
            "#+end_result\n")))

(defun gptel-claude-code--format-tool-use-header (name &optional id)
  "Return the opening line for a tool_use org block with NAME.

When ID is non-nil, include the tool_use_id as a property.
Used during streaming when the tool block starts but input
is not yet available."
  (concat "\n#+begin_tool " name
          (when id (concat " :id " id))
          "\n"))

(defun gptel-claude-code--format-tool-use-footer ()
  "Return the closing line for a tool_use org block."
  "#+end_tool\n")

(provide 'gptel-claude-code-display)
;;; gptel-claude-code-display.el ends here
