;;; .nixpkgs/.doom.d/gptel-tools/ask-user-question.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Interactive question-asking tool for LLMs via Emacs completion system.
;; Implements Claude Code's AskUserQuestion tool specification.
;;
;; Uses :async t so the MCP server's accept-process-output loop keeps
;; Emacs responsive.  When the user hits C-g, the question is deferred
;; and they can call M-x gptelt-auq-resume to answer later.

;;; Code:

;;; Debug Configuration
(defvar gptelt-auq-debug nil
  "When non-nil, log ask-user-question tool operations for debugging.")

;;; Constants
(defconst gptelt-auq-max-questions 5
  "Maximum number of questions per tool call.")

(defconst gptelt-auq-max-options 4
  "Maximum number of options per question.")

(defconst gptelt-auq-min-options 2
  "Minimum number of options per question.")

;;; Parameter Validation

(defun gptelt-auq--validate-input-parameters (questions-input)
  "Validate top-level QUESTIONS-INPUT parameter.
Returns (valid-questions . error-message) where error-message is nil on success."
  (when gptelt-auq-debug
    (message "[ASK-USER-DEBUG] Validating input: type=%s, length=%s"
             (type-of questions-input)
             (if (or (listp questions-input) (vectorp questions-input))
                 (length questions-input)
               "N/A")))

  (cond
   ((null questions-input)
    (cons nil "Error: Parameter 'questions' is required and must be a non-empty array"))

   ((not (or (listp questions-input) (vectorp questions-input)))
    (cons nil (format "Error: Parameter 'questions' must be an array, got: %S"
                      (type-of questions-input))))

   (t
    ;; Convert vector to list if needed
    (let ((questions (if (vectorp questions-input)
                         (append questions-input nil)
                       questions-input)))

      (when (zerop (length questions))
        (setq questions nil))

      (cond
       ((null questions)
        (cons nil "Error: Parameter 'questions' must contain at least one question"))

       ((< (length questions) 1)
        (cons nil "Error: No questions provided"))

       ((> (length questions) gptelt-auq-max-questions)
        (cons nil (format "Error: Too many questions: %d (maximum %d)"
                          (length questions) gptelt-auq-max-questions)))

       ;; All validation passed
       (t
        (cons questions nil)))))))

;;; Validation Helpers

(defun gptelt-auq--plist-to-alist (plist)
  "Convert PLIST to alist for uniform handling."
  (when plist
    (let (result)
      (while plist
        (push (cons (substring (symbol-name (car plist)) 1)
                    (cadr plist))
              result)
        (setq plist (cddr plist)))
      (nreverse result))))

(defun gptelt-auq--normalize-question (q)
  "Normalize question Q to alist format from plist, alist, or vector.
  Handles JSON arrays (vectors) by converting them to lists first."
  (let ((q-as-list (if (vectorp q) (append q nil) q)))
    (cond
     ;; Already an alist
     ((and (listp q-as-list) (consp (car q-as-list)))
      q-as-list)
     ;; Plist format
     ((and (listp q-as-list) (keywordp (car q-as-list)))
      (gptelt-auq--plist-to-alist q-as-list))
     (t
      (error "Invalid question format: %S" q)))))

(defun gptelt-auq--get-prop (alist key &optional default)
  "Get property KEY from ALIST, return DEFAULT if not found."
  (or (cdr (assoc key alist))
      (cdr (assoc (intern (concat ":" key)) alist))
      default))

(defun gptelt-auq--validate-question (q)
  "Validate question Q structure and return normalized alist.
Returns (normalized-question . error-message) where error-message is nil on success."
  (condition-case err
      (let* ((norm-q (gptelt-auq--normalize-question q))
             (question-text (gptelt-auq--get-prop norm-q "question"))
             (options-raw (gptelt-auq--get-prop norm-q "options"))
             (options (if (vectorp options-raw) (append options-raw nil) options-raw))
             (num-options (length options)))

        ;; Validate question text
        (unless (and question-text (stringp question-text) (> (length question-text) 0))
          (error "Missing or invalid question text"))

        ;; Validate options
        (unless (and options (listp options))
          (error "Missing or invalid options list"))

        (when (< num-options gptelt-auq-min-options)
          (error "Too few options: %d (minimum %d)" num-options gptelt-auq-min-options))

        (when (> num-options gptelt-auq-max-options)
          (error "Too many options: %d (maximum %d)" num-options gptelt-auq-max-options))

        ;; Validate each option has label
        (dolist (opt options)
          (let* ((norm-opt (gptelt-auq--normalize-question opt))
                 (label (gptelt-auq--get-prop norm-opt "label")))
            (unless (and label (stringp label) (> (length label) 0))
              (error "Option missing label: %S" opt))))

        ;; Return normalized question
        (cons norm-q nil))
    (error
     (cons nil (error-message-string err)))))

;;; UI Helpers

(defun gptelt-auq--format-option (opt)
  "Format option OPT for display in completion UI.
Returns a cons of (label . description) for use with annotation."
  (let* ((norm-opt (gptelt-auq--normalize-question opt))
         (label (gptelt-auq--get-prop norm-opt "label"))
         (desc (gptelt-auq--get-prop norm-opt "description" "")))
    (cons label desc)))

;;; Deferred/Resumable Question State

(defvar gptelt-auq--pending nil
  "Pending question state for deferred answering.
When non-nil, a plist with:
  :all-numbered   - list of numbered label strings
  :numbered-to-orig - alist mapping numbered labels to original labels
  :prompt         - minibuffer prompt string
  :multi-select   - whether multi-select is enabled
  :question-text  - the question text
  :option-pairs   - list of (label . description) pairs
  :header         - header string
  :callback       - the MCP async callback to call with the final JSON result
  :remaining-questions - remaining questions to ask after this one
  :results-so-far - results collected from previous questions")

(defun gptelt-auq-resume ()
  "Resume a previously deferred question prompt.
Call this after dismissing the completing-read with C-g to
bring back the selection prompt."
  (interactive)
  (unless gptelt-auq--pending
    (user-error "No pending question to resume"))
  (let* ((state gptelt-auq--pending)
         (all-numbered (plist-get state :all-numbered))
         (numbered-to-orig (plist-get state :numbered-to-orig))
         (prompt (plist-get state :prompt))
         (multi-select (plist-get state :multi-select))
         (question-text (plist-get state :question-text))
         (option-pairs (plist-get state :option-pairs))
         (header (plist-get state :header))
         (callback (plist-get state :callback))
         (remaining-questions (plist-get state :remaining-questions))
         (results-so-far (plist-get state :results-so-far))
         result)
    ;; Re-show help buffer
    (gptelt-auq--show-help-buffer question-text header option-pairs)
    (condition-case _err
        (progn
          (setq result
                (if multi-select
                    (completing-read-multiple prompt all-numbered nil t)
                  (completing-read prompt all-numbered nil t)))
          ;; Map numbered labels back to original labels
          (setq result
                (if (listp result)
                    (mapcar (lambda (r)
                              (or (cdr (assoc r numbered-to-orig)) r))
                            result)
                  (or (cdr (assoc result numbered-to-orig)) result)))
          ;; Handle "Other" custom input
          (when (if (listp result)
                    (member "Other (custom input)" result)
                  (string= result "Other (custom input)"))
            (let ((custom (read-string "Enter custom answer: ")))
              (if (listp result)
                  (setq result
                        (mapcar (lambda (s)
                                  (if (string= s "Other (custom input)")
                                      custom
                                    s))
                                result))
                (setq result custom))))
          ;; Answer recorded - continue with remaining questions or finish
          (let* ((answer (if (listp result)
                             (mapconcat #'identity result ", ")
                           result))
                 (this-result (cons question-text answer))
                 (all-results (append results-so-far (list this-result))))
            (setq gptelt-auq--pending nil)
            (gptelt-auq--cleanup-help-buffer)
            (message "Answer recorded: %s" answer)
            ;; Continue asking remaining questions or call callback with final results
            (if remaining-questions
                (gptelt-auq--ask-remaining-questions
                 remaining-questions all-results callback)
              (gptelt-auq--finish-with-results all-results callback))))
      (quit
       (message "Dismissed again. Use M-x gptelt-auq-resume to answer later.")))))

;;; Single Question UI

(defconst gptelt-auq--help-buffer-name "*AUQ Question*"
  "Buffer name for displaying question details.")

(defun gptelt-auq--show-help-buffer (question-text header option-pairs)
  "Display a help buffer with QUESTION-TEXT, HEADER, and OPTION-PAIRS.
OPTION-PAIRS is a list of (label . description) cons cells.
The buffer uses word-wrap so long text is readable."
  (let ((buf (get-buffer-create gptelt-auq--help-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (when (and header (> (length header) 0))
          (insert (propertize (format "[%s]\n" header)
                              'face 'font-lock-keyword-face)))
        (insert (propertize "Question:\n" 'face 'font-lock-function-name-face))
        (insert question-text "\n\n")
        (insert (propertize "Options:\n" 'face 'font-lock-function-name-face))
        (let ((idx 1))
          (dolist (pair option-pairs)
            (insert (propertize (format "  %d. %s" idx (car pair))
                                'face 'font-lock-constant-face))
            (when (and (cdr pair) (> (length (cdr pair)) 0))
              (insert "\n     " (cdr pair)))
            (insert "\n")
            (cl-incf idx)))
        (insert (propertize (format "  %d. Other (custom input)\n" (1+ (length option-pairs)))
                            'face 'font-lock-comment-face))
        (setq buffer-read-only t)
        (goto-char (point-min))
        (visual-line-mode 1)))
    (display-buffer buf
                    '((display-buffer-reuse-window display-buffer-below-selected)
                      (window-height . fit-window-to-buffer)))))

(defun gptelt-auq--cleanup-help-buffer ()
  "Kill the help buffer and its window if they exist."
  (when-let* ((buf (get-buffer gptelt-auq--help-buffer-name)))
    (when-let* ((win (get-buffer-window buf)))
      (delete-window win))
    (kill-buffer buf)))

(defun gptelt-auq--ask-single-question (norm-q callback remaining-questions results-so-far)
  "Ask single question NORM-Q interactively.
CALLBACK is the MCP async callback for the final JSON result.
REMAINING-QUESTIONS is the list of questions still to ask after this one.
RESULTS-SO-FAR is the list of (question . answer) pairs already collected.

On normal completion, either continues to the next question or calls
CALLBACK with the final JSON result.

On C-g, stores state in `gptelt-auq--pending' so the user can do research
and call `gptelt-auq-resume' later.  The MCP server's own polling loop
keeps Emacs responsive in the meantime."
  (let* ((question-text (gptelt-auq--get-prop norm-q "question"))
         (header (gptelt-auq--get-prop norm-q "header" ""))
         (options-raw (gptelt-auq--get-prop norm-q "options"))
         (options (if (vectorp options-raw) (append options-raw nil) options-raw))
         (multi-select (gptelt-auq--get-prop norm-q "multiSelect" nil))
         ;; Build label->description pairs
         (option-pairs (mapcar #'gptelt-auq--format-option options))
         (labels (mapcar #'car option-pairs))
         ;; Build numbered short labels for vertico: "1. Label"
         (numbered-labels
          (let ((idx 0))
            (mapcar (lambda (label)
                      (cl-incf idx)
                      (format "%d. %s" idx label))
                    labels)))
         ;; Add "Other" option
         (other-label (format "%d. Other (custom input)" (1+ (length labels))))
         (all-numbered (append numbered-labels (list other-label)))
         ;; Map numbered->original label
         (numbered-to-orig
          (append (cl-mapcar #'cons numbered-labels labels)
                  (list (cons other-label "Other (custom input)"))))
         ;; Short prompt for minibuffer
         (prompt (if (and header (> (length header) 0))
                     (format "[%s] Select: " header)
                   "Select: "))
         result)

    ;; Show help buffer with full question and descriptions
    (gptelt-auq--show-help-buffer question-text header option-pairs)

    (condition-case _err
        (progn
          (setq result
                (if multi-select
                    (completing-read-multiple
                     prompt all-numbered nil t)
                  (completing-read
                   prompt all-numbered nil t)))

          ;; Map numbered labels back to original labels
          (setq result
                (if (listp result)
                    (mapcar (lambda (r)
                              (or (cdr (assoc r numbered-to-orig)) r))
                            result)
                  (or (cdr (assoc result numbered-to-orig)) result)))

          ;; Handle "Other" custom input
          (when (if (listp result)
                    (member "Other (custom input)" result)
                  (string= result "Other (custom input)"))
            (let ((custom (read-string "Enter custom answer: ")))
              (if (listp result)
                  (setq result
                        (mapcar (lambda (s)
                                  (if (string= s "Other (custom input)")
                                      custom
                                    s))
                                result))
                (setq result custom))))

          ;; Clean up help buffer
          (gptelt-auq--cleanup-help-buffer)
          (let* ((answer (if (listp result)
                             (mapconcat #'identity result ", ")
                           result))
                 (this-result (cons question-text answer))
                 (all-results (append results-so-far (list this-result))))
            ;; Continue with remaining questions or finish
            (if remaining-questions
                (gptelt-auq--ask-remaining-questions
                 remaining-questions all-results callback)
              (gptelt-auq--finish-with-results all-results callback))))

      (quit
       ;; User hit C-g -- defer the question.
       ;; Keep help buffer open so they can read it.
       ;; Store state so gptelt-auq-resume can bring back the prompt.
       ;; Do NOT block here -- just return.  The MCP server's own
       ;; accept-process-output loop keeps Emacs responsive, so the
       ;; user can freely interact and call M-x gptelt-auq-resume.
       (setq gptelt-auq--pending
             (list :all-numbered all-numbered
                   :numbered-to-orig numbered-to-orig
                   :prompt prompt
                   :multi-select multi-select
                   :question-text question-text
                   :option-pairs option-pairs
                   :header header
                   :callback callback
                   :remaining-questions remaining-questions
                   :results-so-far results-so-far))
       (message "Question deferred. Do your research, then M-x gptelt-auq-resume to answer."))

      (error
       (gptelt-auq--cleanup-help-buffer)
       (message "Question failed: %s" (error-message-string _err))
       ;; On error, call callback with error result
       (funcall callback
                (json-encode
                 `((isError . t)
                   (content . [((type . "text")
                                (text . ,(format "Error: %s"
                                                 (error-message-string _err))))]))))))))

;;; Result Helpers

(defun gptelt-auq--build-result-json (results)
  "Build the final JSON result string from RESULTS alist.
RESULTS is a list of (question-text . answer) pairs."
  (let ((has-errors (seq-some (lambda (pair)
                                (string-match-p "error\\|cancelled"
                                                (cdr pair)))
                              results)))
    (if has-errors
        (json-encode
         `((isError . t)
           (content . [((type . "text")
                        (text . ,(format "Errors occurred: %S" results)))])))
      (json-encode
       `((answers . ,(mapcar (lambda (pair)
                               (cons (car pair) (cdr pair)))
                             results)))))))

(defun gptelt-auq--finish-with-results (results callback)
  "Finalize RESULTS and call CALLBACK with JSON response.
RESULTS is a list of (question-text . answer) pairs."
  (when gptelt-auq-debug
    (message "[ASK-USER-DEBUG] Finishing with %d result(s)" (length results)))
  (funcall callback (gptelt-auq--build-result-json results)))

(defun gptelt-auq--ask-remaining-questions (questions results-so-far callback)
  "Ask QUESTIONS sequentially, accumulating RESULTS-SO-FAR.
CALLBACK is the MCP async callback for the final JSON result.
Each question may defer (user hits C-g), in which case the state
is stored in `gptelt-auq--pending' and CALLBACK will be called
later from `gptelt-auq-resume'."
  (if (null questions)
      ;; All questions answered
      (gptelt-auq--finish-with-results results-so-far callback)
    (let* ((q (car questions))
           (rest (cdr questions))
           (validation (gptelt-auq--validate-question q))
           (norm-q (car validation))
           (error-msg (cdr validation)))
      (if error-msg
          ;; Validation failed - record error and continue
          (let* ((question-text (when norm-q
                                  (gptelt-auq--get-prop norm-q "question")))
                 (error-result (cons (or question-text "unknown")
                                     (format "Validation error: %s" error-msg)))
                 (new-results (append results-so-far (list error-result))))
            (gptelt-auq--ask-remaining-questions rest new-results callback))
        ;; Ask this question
        (gptelt-auq--ask-single-question norm-q callback rest results-so-far)))))

;;; Main Tool Function

(defun gptelt-auq-ask-user-question (callback questions-input)
  "Ask user QUESTIONS-INPUT interactively and return results via CALLBACK.
CALLBACK is the MCP async callback (first arg due to :async t).
QUESTIONS-INPUT should be a list of question plists/alists.

Calls CALLBACK with JSON string containing answers or error information.
This is the main entry point called by the LLM."
  (when gptelt-auq-debug
    (message "[ASK-USER-DEBUG] Called with %d question(s)"
             (if (or (listp questions-input) (vectorp questions-input))
                 (length questions-input)
               0)))

  ;; Validate input parameters first
  (let* ((validation (gptelt-auq--validate-input-parameters questions-input))
         (questions (car validation))
         (error-msg (cdr validation)))

    (if error-msg
        ;; Return parameter validation error immediately
        (progn
          (when gptelt-auq-debug
            (message "[ASK-USER-DEBUG] Validation failed: %s" error-msg))
          (funcall callback
                   (json-encode
                    `((isError . t)
                      (content . [((type . "text")
                                   (text . ,error-msg))])))))

      ;; Parameters valid - proceed with asking questions
      (condition-case err
          (progn
            (when gptelt-auq-debug
              (message "[ASK-USER-DEBUG] Asking %d validated question(s)"
                       (length questions)))

            ;; Notify via pushover
            (ignore-errors
              (let ((msg (mapconcat
                          (lambda (q)
                            (let* ((norm-q (gptelt-auq--normalize-question q))
                                   (question-text (gptelt-auq--get-prop norm-q "question"))
                                   (options-raw (gptelt-auq--get-prop norm-q "options"))
                                   (options (if (vectorp options-raw) (append options-raw nil) options-raw))
                                   (opts-str (mapconcat
                                              (lambda (opt)
                                                (let ((pair (gptelt-auq--format-option opt)))
                                                  (if (and (cdr pair) (> (length (cdr pair)) 0))
                                                      (format "%s - %s" (car pair) (cdr pair))
                                                    (car pair))))
                                              options "\n  ")))
                              (format "Q: %s\n  %s" question-text opts-str)))
                          questions
                          "\n\n")))
                (pushover-send "GPTEL Question" msg :sound "magic")))

            ;; Start asking questions -- the first call either completes all
            ;; questions synchronously or defers on C-g.  When deferred, the
            ;; callback will be called later from gptelt-auq-resume.
            (gptelt-auq--ask-remaining-questions questions nil callback))

        (error
         ;; Top-level error handling
         (when gptelt-auq-debug
           (message "[ASK-USER-DEBUG] Caught error: %s" (error-message-string err)))
         (funcall callback
                  (json-encode
                   `((isError . t)
                     (content . [((type . "text")
                                  (text . ,(format "Error: %s"
                                                   (error-message-string err))))])))))))))

;;; Tool Registration

(gptelt-make-tool
 :name "ask_user_question"
 :function #'gptelt-auq-ask-user-question
 :async t
 :description
 (concat "Ask user structured questions interactively via Emacs completion UI. "
         "Supports single-select and multi-select questions with 2-4 options each. "
         "Maximum 4 questions per call. User can provide custom 'Other' responses."
         "\n\nPARAMETER STRUCTURE:\n"
         "{\n"
         "  \"questions\": array (required) - Array of 1-4 question objects\n"
         "}\n\n"
         "QUESTION OBJECT STRUCTURE:\n"
         "{\n"
         "  \"question\": \"string\" (required) - The question text to display\n"
         "  \"header\": \"string\" (optional) - Header text shown before question\n"
         "  \"options\": array (required) - Array of 2-4 option objects\n"
         "  \"multiSelect\": boolean (optional, default: false) - Allow multiple selections\n"
         "}\n\n"
         "OPTION OBJECT STRUCTURE:\n"
         "{\n"
         "  \"label\": \"string\" (required) - The option label shown to user\n"
         "  \"description\": \"string\" (optional) - Additional description of the option\n"
         "}\n\n"
         "IMPORTANT CONSTRAINTS:\n"
         "- 1-4 questions per call\n"
         "- 2-4 options per question\n"
         "- No emojis in text (breaks TTY)\n"
         "- Cannot be used by subagents (main thread only)\n"
         "- Blocks execution until user responds\n\n"
         "QUESTION FORMAT EXAMPLE:\n"
         "{\n"
         "  \"question\": \"What type of task is this?\",\n"
         "  \"header\": \"Task Type\",  // optional\n"
         "  \"options\": [\n"
         "    {\"label\": \"Build Feature\", \"description\": \"Create new functionality\"},\n"
         "    {\"label\": \"Fix Bug\", \"description\": \"Debug and resolve issue\"}\n"
         "  ],\n"
         "  \"multiSelect\": false  // true for multiple selections\n"
         "}\n\n"
         "SUCCESS RESPONSE FORMAT:\n"
         "{\n"
         "  \"answers\": {\n"
         "    \"What type of task is this?\": \"Build Feature\"\n"
         "  }\n"
         "}\n\n"
         "ERROR RESPONSE FORMAT:\n"
         "{\n"
         "  \"isError\": true,\n"
         "  \"content\": [{\"type\": \"text\", \"text\": \"Error message\"}]\n"
         "}\n\n"
         "USAGE EXAMPLES:\n"
         "1. Single question, single-select:\n"
         "   [{\"question\": \"Auth method?\", \n"
         "     \"options\": [{\"label\": \"JWT\"}, {\"label\": \"OAuth\"}]}]\n\n"
         "2. Multiple questions:\n"
         "   [{\"question\": \"Task type?\", ...}, \n"
         "    {\"question\": \"Output format?\", ...}]\n\n"
         "3. Multi-select:\n"
         "   [{\"question\": \"Features?\", \n"
         "     \"multiSelect\": true,\n"
         "     \"options\": [...]}]")

 :args '((:name "questions"
          :type array
          :description "Array of question objects (1-4 questions, 2-4 options each)"
          :items
          ((:type object
            :description "A question object"
            :required ["question" "options"]
            :properties
            ((:name "question"
              :type string
              :description "The question text to display")
             (:name "header"
              :type string
              :description "Header text shown before question")
             (:name "options"
              :type array
              :description "Array of 2-4 option objects"
              :minItems 2
              :maxItems 4
              :items
              ((:type object
                :description "Option object"
                :required ["label"]
                :properties
                ((:name "label"
                  :type string
                  :description "The option label shown to user")
                 (:name "description"
                  :type string
                  :description "Option description")))))
             (:name "multiSelect"
              :type boolean
              :description "Allow multiple selections"))))))
 :category "interaction"
 :confirm nil
 :include t)

;;; Test Examples

(comment
  ;; Test 1: Single question, single-select (async - callback as first arg)
  (gptelt-auq-ask-user-question
   (lambda (result) (message "Result: %s" result))
   '(((:question . "What type of task is this?")
      (:header . "Task Type")
      (:options . (((:label . "Build Feature")
                    (:description . "Create new functionality"))
                   ((:label . "Fix Bug")
                    (:description . "Debug and resolve issue"))))
      (:multiSelect . nil))))

  ;; Test 2: Multiple questions
  (gptelt-auq-ask-user-question
   (lambda (result) (message "Result: %s" result))
   '(((:question . "Auth method?")
      (:options . (((:label . "JWT"))
                   ((:label . "OAuth")))))
     ((:question . "Output format?")
      (:options . (((:label . "XML"))
                   ((:label . "Markdown")))))))

  ;; Test 3: Multi-select
  (gptelt-auq-ask-user-question
   (lambda (result) (message "Result: %s" result))
   '(((:question . "Select features to include")
      (:multiSelect . t)
      (:options . (((:label . "Auth"))
                   ((:label . "Logging"))
                   ((:label . "Testing"))))))))

;;; ask-user-question.el ends here
