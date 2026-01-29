;;; .nixpkgs/.doom.d/gptel-tools/ask-user-question.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Interactive question-asking tool for LLMs via Emacs completion system.
;; Implements Claude Code's AskUserQuestion tool specification.

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

(defconst gptelt-auq-max-attempts 3
  "Maximum number of attempts (initial + 2 retries).")

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
             (options (gptelt-auq--get-prop norm-q "options"))
             (num-options (length options)))
        
        ;; Validate question text
        (unless (and question-text (stringp question-text) (> (length question-text) 0))
          (error "Missing or invalid question text"))
        
        ;; Validate options - accept both lists and vectors (JSON arrays)
        (unless (and options (or (listp options) (vectorp options)))
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
  "Format option OPT for display in completion UI."
  (let* ((norm-opt (gptelt-auq--normalize-question opt))
         (label (gptelt-auq--get-prop norm-opt "label"))
         (desc (gptelt-auq--get-prop norm-opt "description" "")))
    (if (and desc (> (length desc) 0))
        (format "%s - %s" label desc)
      label)))

(defun gptelt-auq--build-prompt (question-text header options)
  "Build completion prompt from QUESTION-TEXT, HEADER, and OPTIONS."
  (let ((header-str (if (and header (> (length header) 0))
                        (format "[%s] " header)
                      "")))
    (concat header-str question-text)))

;;; Single Question UI

(defun gptelt-auq--ask-single-question (norm-q)
  "Ask single question NORM-Q and return (question-text . selected-label).
NORM-Q is a normalized alist. Returns nil on error/cancel."
  (let* ((question-text (gptelt-auq--get-prop norm-q "question"))
         (header (gptelt-auq--get-prop norm-q "header" ""))
         (options (gptelt-auq--get-prop norm-q "options"))
         (multi-select (gptelt-auq--get-prop norm-q "multiSelect" nil))
         (formatted-options (mapcar #'gptelt-auq--format-option options))
         ;; Add "Other" option at the end
         (all-options (append formatted-options '("Other (custom input)")))
         (prompt (gptelt-auq--build-prompt question-text header options))
         result)
    
    (condition-case err
        (progn
          (setq result
                (if multi-select
                    ;; Multi-select: returns comma-separated string
                    (completing-read-multiple 
                     (concat prompt " (select multiple, separate with commas): ")
                     all-options nil nil)
                  ;; Single-select: returns single string
                  (completing-read 
                   (concat prompt ": ")
                   all-options nil nil)))
          
          ;; Handle "Other" custom input
          (when (if (listp result)
                    (member "Other (custom input)" result)
                  (string= result "Other (custom input)"))
            (let ((custom (read-string "Enter custom answer: ")))
              (if (listp result)
                  ;; Replace "Other" with custom text in list
                  (setq result 
                        (mapcar (lambda (s)
                                  (if (string= s "Other (custom input)")
                                      custom
                                    s))
                                result))
                ;; Replace "Other" with custom text
                (setq result custom))))
          
          ;; Return (question . answer)
          (cons question-text
                (if (listp result)
                    (mapconcat #'identity result ", ")
                  result)))
      
      ((quit error)
       ;; User cancelled or error occurred
       (message "Question cancelled or failed: %s" (error-message-string err))
       nil))))

;;; Multi-Question Coordinator

(defun gptelt-auq--ask-multiple-questions (questions)
  "Ask multiple QUESTIONS and return alist of (question . answer).
Retries failed questions up to max attempts. Returns list of results."
  (let ((results nil)
        (attempt-count (make-hash-table :test 'equal)))
    
    (dolist (q questions)
      (let* ((validation (gptelt-auq--validate-question q))
             (norm-q (car validation))
             (error-msg (cdr validation))
             (question-text (when norm-q (gptelt-auq--get-prop norm-q "question")))
             (current-attempt 0)
             answer-pair)
        
        (if error-msg
            ;; Validation failed - return error for this question
            (push (cons question-text 
                        (format "Validation error: %s" error-msg))
                  results)
          
          ;; Try to ask question with retry logic
          (while (and (not answer-pair)
                      (< current-attempt gptelt-auq-max-attempts))
            (setq current-attempt (1+ current-attempt))
            (setq answer-pair (gptelt-auq--ask-single-question norm-q))
            
            (unless answer-pair
              (when (< current-attempt gptelt-auq-max-attempts)
                (message "Retrying question (attempt %d/%d)..." 
                         current-attempt gptelt-auq-max-attempts)
                (sit-for 0.5))))
          
          ;; Store result or error
          (if answer-pair
              (push answer-pair results)
            (push (cons question-text "User cancelled after retries")
                  results)))))
    
    (nreverse results)))

;;; Main Tool Function

(defun gptelt-auq-ask-user-question (questions-input)
  "Ask user QUESTIONS-INPUT interactively and return results.
QUESTIONS-INPUT should be a list of question plists/alists.

Returns JSON string with answers or error information.
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
        ;; Return parameter validation error
        (progn
          (when gptelt-auq-debug
            (message "[ASK-USER-DEBUG] Validation failed: %s" error-msg))
          (json-encode 
           `((isError . t)
             (content . [((type . "text")
                          (text . ,error-msg))]))))
      
      ;; Parameters valid - proceed with asking questions
      (condition-case err
          (let ((num-questions (length questions)))
            (when gptelt-auq-debug
              (message "[ASK-USER-DEBUG] Asking %d validated question(s)" num-questions))
            
            ;; Ask questions and collect answers
            (let* ((results (gptelt-auq--ask-multiple-questions questions))
                   (has-errors (seq-some (lambda (pair)
                                           (string-match-p "error\\|cancelled" 
                                                           (cdr pair)))
                                         results)))
              
              (when gptelt-auq-debug
                (message "[ASK-USER-DEBUG] Collected %d answer(s), has-errors: %s" 
                         (length results) has-errors))
              
              (if has-errors
                  ;; Return error JSON
                  (progn
                    (when gptelt-auq-debug
                      (message "[ASK-USER-DEBUG] Returning error results"))
                    (json-encode 
                     `((isError . t)
                       (content . [((type . "text")
                                    (text . ,(format "Errors occurred: %S" results)))]))))
                
                ;; Return success - convert results to JSON-friendly format
                (when gptelt-auq-debug
                  (message "[ASK-USER-DEBUG] Returning success results"))
                (json-encode 
                 `((answers . ,(mapcar (lambda (pair)
                                         (cons (car pair) (cdr pair)))
                                       results)))))))
        
        (error
         ;; Top-level error handling
         (when gptelt-auq-debug
           (message "[ASK-USER-DEBUG] Caught error: %s" (error-message-string err)))
         (json-encode 
          `((isError . t)
            (content . [((type . "text")
                         (text . ,(format "Error: %s" 
                                          (error-message-string err))))])))))))) 

;;; Tool Registration

(when (fboundp 'gptelt-make-tool)
  (gptelt-make-tool
   :name "ask_user_question"
   :function #'gptelt-auq-ask-user-question
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
   :include t))

;;; Test Examples

(comment
  ;; Test 1: Single question, single-select
  (gptelt-auq-ask-user-question
   '(((:question . "What type of task is this?")
      (:header . "Task Type")
      (:options . (((:label . "Build Feature")
                    (:description . "Create new functionality"))
                   ((:label . "Fix Bug")
                    (:description . "Debug and resolve issue"))))
      (:multiSelect . nil)))
   (lambda (result) (message "Result: %s" result)))
  
  ;; Test 2: Multiple questions
  (gptelt-auq-ask-user-question
   '(((:question . "Auth method?")
      (:options . (((:label . "JWT"))
                   ((:label . "OAuth")))))
     ((:question . "Output format?")
      (:options . (((:label . "XML"))
                   ((:label . "Markdown"))))))
   (lambda (result) (message "Result: %s" result)))
  
  ;; Test 3: Multi-select
  (gptelt-auq-ask-user-question
   '(((:question . "Select features to include")
      (:multiSelect . t)
      (:options . (((:label . "Auth"))
                   ((:label . "Logging"))
                   ((:label . "Testing"))))))
   (lambda (result) (message "Result: %s" result)))
  
  ;; Test 4: With "Other" custom input
  ;; Just select "Other (custom input)" and provide custom text
  
  ;; Test 5: Error case - too many questions
  (gptelt-auq-ask-user-question
   (make-list 5 '((:question . "Test?")
                  (:options . (((:label . "A")) ((:label . "B"))))))
   (lambda (result) (message "Result: %s" result))))

;;; ask-user-question.el ends here
