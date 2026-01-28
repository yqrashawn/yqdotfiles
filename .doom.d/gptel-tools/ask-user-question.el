;;; .nixpkgs/.doom.d/gptel-tools/ask-user-question.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Interactive question-asking tool for LLMs via Emacs completion system.
;; Implements Claude Code's AskUserQuestion tool specification.

;;; Code:

;;; Constants
(defconst gptelt-auq-max-questions 5
  "Maximum number of questions per tool call.")

(defconst gptelt-auq-max-options 4
  "Maximum number of options per question.")

(defconst gptelt-auq-min-options 2
  "Minimum number of options per question.")

(defconst gptelt-auq-max-attempts 3
  "Maximum number of attempts (initial + 2 retries).")

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
  "Normalize question Q to alist format from plist or alist."
  (cond
   ;; Already an alist
   ((and (listp q) (consp (car q)))
    q)
   ;; Plist format
   ((and (listp q) (keywordp (car q)))
    (gptelt-auq--plist-to-alist q))
   (t
    (error "Invalid question format: %S" q))))

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
  (condition-case err
      (let* ((questions (if (vectorp questions-input)
                            (append questions-input nil)
                          questions-input))
             (num-questions (length questions)))
        
        ;; Validate number of questions
        (when (< num-questions 1)
          (error "No questions provided"))
        
        (when (> num-questions gptelt-auq-max-questions)
          (error "Too many questions: %d (maximum %d)" 
                 num-questions gptelt-auq-max-questions))
        
        ;; Ask questions and collect answers
        (let* ((results (gptelt-auq--ask-multiple-questions questions))
               (has-errors (seq-some (lambda (pair)
                                       (string-match-p "error\\|cancelled" 
                                                       (cdr pair)))
                                     results)))
          
          (if has-errors
              ;; Return error JSON
              (json-encode 
               `((isError . t)
                 (content . [((type . "text")
                              (text . ,(format "Errors occurred: %S" results)))])))
            
            ;; Return success - convert results to JSON-friendly format
            (json-encode 
             `((answers . ,(mapcar (lambda (pair)
                                     (cons (car pair) (cdr pair)))
                                   results)))))))
    
    (error
     ;; Top-level error handling
     (json-encode 
      `((isError . t)
        (content . [((type . "text")
                     (text . ,(format "Error: %s" 
                                      (error-message-string err))))]))))))

;;; Tool Registration

(when (fboundp 'gptelt-make-tool)
  (gptelt-make-tool
   :name "ask_user_question"
   :function #'gptelt-auq-ask-user-question
   :description
   "Ask user structured questions interactively via Emacs completion UI.
Supports single-select and multi-select questions with 2-4 options each.
Maximum 4 questions per call. User can provide custom 'Other' responses.

IMPORTANT CONSTRAINTS:
- 1-4 questions per call
- 2-4 options per question
- No emojis in text (breaks TTY)
- Cannot be used by subagents (main thread only)
- Blocks execution until user responds

QUESTION FORMAT:
{
  \"question\": \"What type of task is this?\",
  \"header\": \"Task Type\",  // optional
  \"options\": [
    {\"label\": \"Build Feature\", \"description\": \"Create new functionality\"},
    {\"label\": \"Fix Bug\", \"description\": \"Debug and resolve issue\"}
  ],
  \"multiSelect\": false  // true for multiple selections
}

RETURNS:
{
  \"answers\": {
    \"What type of task is this?\": \"Build Feature\"
  }
}

ERROR FORMAT:
{
  \"isError\": true,
  \"content\": [{\"type\": \"text\", \"text\": \"Error message\"}]
}

EXAMPLES:
1. Single question, single-select:
   [{\"question\": \"Auth method?\", 
     \"options\": [{\"label\": \"JWT\"}, {\"label\": \"OAuth\"}]}]

2. Multiple questions:
   [{\"question\": \"Task type?\", ...}, 
    {\"question\": \"Output format?\", ...}]

3. Multi-select:
   [{\"question\": \"Features?\", 
     \"multiSelect\": true,
     \"options\": [...]}]"
   
   :args '((:name "questions"
            :type array
            :description "Array of question objects (1-4 questions, 2-4 options each)"))
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
