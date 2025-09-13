;;; .nixpkgs/.doom.d/gptel-tools/elisp.el -*- lexical-binding: t; -*-

;;; Tool: Evaluate Elisp Buffer (ask for confirmation, show buffer if not visible)
(defun gptelt-evaluate-elisp-buffer (buffer-name)
  "Evaluate elisp BUFFER-NAME after confirming with user. Show buffer if not visible. Bury if not shown before."
  (interactive)
  (let* ((buf (get-buffer buffer-name))
         (buf-file (buffer-file-name buf))
         (shown-before (get-buffer-window buf))
         (eval-ok? nil)
         (eval-err nil)
         win)
    (unless buf (error "Buffer not found: %s" buffer-name))
    (unless (eq (buffer-local-value 'major-mode buf) 'emacs-lisp-mode)
      (error "Buffer %s is not in emacs-lisp-mode" buffer-name))
    (setq win (unless shown-before (display-buffer buf)))
    (when (y-or-n-p (format "Evaluate buffer %s? " buffer-name))
      (setq eval-err
            (condition-case err
                (progn
                  (eval-buffer buf t buf-file nil t)
                  (setq eval-ok? t))
              (error (format "Error evaluate buffer `%s`: %s"
                             buf
                             (error-message-string err)))))
      (message "Evaluated buffer: %s" buffer-name))
    (when (and win (window-live-p win))
      (quit-restore-window win 'bury))
    (if eval-ok?
        (format "Successfully evaluated buffer `%s`" buffer-name)
      eval-err)))

;;; Tool: Evaluate Elisp File (load file into buffer, then evaluate, with confirmation)
(defun gptelt-evaluate-elisp-file (file-path)
  "Load FILE-PATH into buffer and evaluate after confirming with user. Show buffer if not visible. Bury if not shown before."
  (interactive)
  (unless (file-name-absolute-p file-path) (error "file-path must be absolute"))
  (unless (string-suffix-p ".el" file-path)
    (error "File %s is not an .el file" file-path))
  (gptelt-evaluate-elisp-buffer (find-file-noselect file-path)))

;;; Tool: Evaluate Elisp String (put string in temp buffer, eval after confirmation, return result)
(defun gptelt-evaluate-elisp-string (elisp-string)
  "Put ELISP-STRING into a temp buffer for confirmation, then eval using (eval (read ...)), return result."
  (interactive)
  (let* ((buf (generate-new-buffer " *gptel-eval-elisp*"))
         (eval-result nil)
         (eval-err nil)
         win)
    (with-current-buffer buf
      (emacs-lisp-mode)
      (insert elisp-string)
      (goto-char (point-min)))
    (setq win (display-buffer buf))
    (when (y-or-n-p "Evaluate elisp string in temp buffer? ")
      (condition-case err
          (setq eval-result (eval (read elisp-string)))
        (error (setq eval-err (format "Error evaluating elisp string: %s" (error-message-string err))))))
    (when (and win (window-live-p win))
      (quit-restore-window win 'bury))
    (when (buffer-live-p buf) (kill-buffer buf))
    (if eval-err
        eval-err
      (format "Result: %S" eval-result))))

;;; Tool: Run ERT test by selector regex and return summary report
(defun gptelt-run-ert-test (test_selector_regex)
  "Run ERT tests matching TEST_SELECTOR_REGEX, return summary report as string. Handles 'test-started and 'test-ended events."
  (let* ((output-string "")
         (listener
          (lambda (event-type &rest event-args)
            (cl-ecase event-type
              (run-started
               (cl-destructuring-bind (stats) event-args
                 (setq output-string (format "Running %s tests (selector: %S)\n"
                                             (length (ert--stats-tests stats))
                                             test_selector_regex))))
              (test-started
               (cl-destructuring-bind (stats test) event-args
                 (setq output-string (concat output-string
                                             (format "Started: %S\n" (ert-test-name test))))))
              (test-ended
               (cl-destructuring-bind (stats test result) event-args
                 (let ((expectedp (ert-test-result-expected-p test result)))
                   (setq output-string (concat output-string
                                               (format "Ended:   %S\n%9s  %S%s\n"
                                                       (ert-test-name test)
                                                       (ert-string-for-test-result result expectedp)
                                                       (ert-test-name test)
                                                       (ert-reason-for-test-result result)))))))
              (run-ended
               (cl-destructuring-bind (stats abortedp) event-args
                 (setq output-string (concat output-string
                                             (format "\nRan %s tests, %s results as expected, %s unexpected, %s skipped%s\n"
                                                     (ert-stats-total stats)
                                                     (ert-stats-completed-expected stats)
                                                     (ert-stats-completed-unexpected stats)
                                                     (ert-stats-skipped stats)
                                                     (if abortedp ", ABORTED" "")))))))))
         (stats (ert-run-tests test_selector_regex listener)))
    output-string))

(comment
  (gptelt-run-ert-test "list-buffers-test"))

;;; gptel tool registration
(when (fboundp 'gptel-make-tool)
  (gptel-make-tool
   :name "evaluate_elisp_buffer"
   :function #'gptelt-evaluate-elisp-buffer
   :description "Evaluate emacs-lisp-mode a buffer by buffer name."
   :args '((:name "buffer_name" :type string :description "The name of the buffer to evaluate"))
   :category "elisp"
   :confirm nil
   :include t)

  (gptel-make-tool
   :name "evaluate_elisp_file"
   :function #'gptelt-evaluate-elisp-file
   :description "Evaluate a \".el\" file by loading it as a buffer and evaluating."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the elisp file to evaluate"))
   :category "elisp"
   :confirm nil
   :include t)

  (gptel-make-tool
   :name "run_ert_test"
   :function #'gptelt-run-ert-test
   :description "Run Emacs ERT test(s) by selector regex, return summary report."
   :args '((:name "test_selector_regex" :type string :description "Regex selector for tests to run."))
   :category "elisp"
   :confirm nil
   :include t)

  (gptel-make-tool
   :name "evaluate_elisp_string"
   :function #'gptelt-evaluate-elisp-string
   :description "Evaluate emacs-lisp code from a string in a temp buffer, after user confirmation. Returns the result string."
   :args '((:name "elisp_string" :type string :description "The elisp code string to evaluate."))
   :category "elisp"
   :confirm nil
   :include t))
