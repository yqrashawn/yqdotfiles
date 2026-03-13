;;; clj-test.el --- Tests for clj.el async functions -*- lexical-binding: t; -*-

(require 'ert)

;;; Helper macro to reduce boilerplate for nREPL mocking

(defmacro clj-test--with-mock-nrepl (mock-repl-buf &rest body)
  "Execute BODY with nREPL and CIDER REPL functions mocked.
MOCK-REPL-BUF is a symbol bound to the mock REPL buffer."
  (declare (indent 1))
  `(let ((,mock-repl-buf (generate-new-buffer " *mock-clj-repl*")))
     (unwind-protect
         (cl-letf (((symbol-function 'gptelt-clojure--ensure-workspace) #'ignore)
                   ((symbol-function 'gptelt-clj-ensure-helper-loaded) #'ignore)
                   ((symbol-function 'gptelt-clj--get-clj-repl) (lambda () ,mock-repl-buf))
                   ((symbol-function 'cider-repl--replace-input) #'ignore)
                   ((symbol-function 'cider-repl-reset-markers) #'ignore)
                   ((symbol-function 'cider-repl-emit-stdout) #'ignore)
                   ((symbol-function 'cider-repl-emit-result) #'ignore)
                   ((symbol-function 'cider-repl-emit-stderr) #'ignore)
                   ((symbol-function 'cider-repl-emit-prompt) #'ignore)
                   ((symbol-function 'nrepl--mark-id-completed) #'ignore)
                   ((symbol-function 'format!) (lambda (fmt &rest args) (apply #'format fmt args))))
           ,@body)
       (when (buffer-live-p ,mock-repl-buf)
         (kill-buffer ,mock-repl-buf)))))

;;; Tests for gptelt-eval--clj-string-async

(ert-deftest gptelt-eval-clj-string-async-success-test ()
  "Test async CLJ eval calls callback with value on success."
  (let ((result nil))
    (clj-test--with-mock-nrepl buf
      (cl-letf (((symbol-function 'nrepl-request:eval)
                 (lambda (input callback connection &optional ns &rest _)
                   (funcall callback (nrepl-dict "value" "42" "ns" "user"))
                   (funcall callback (nrepl-dict "status" '("done") "id" "test-1")))))
        (gptelt-eval--clj-string-async
         (lambda (r) (setq result r))
         "(+ 1 2)" "user")))
    (should (equal result "42"))))

(ert-deftest gptelt-eval-clj-string-async-error-output-test ()
  "Test async CLJ eval calls callback with error output."
  (let ((result nil))
    (clj-test--with-mock-nrepl buf
      (cl-letf (((symbol-function 'nrepl-request:eval)
                 (lambda (input callback connection &optional ns &rest _)
                   (funcall callback (nrepl-dict "err" "CompilerException: Unable to resolve symbol: bad"))
                   (funcall callback (nrepl-dict "status" '("done") "id" "test-2")))))
        (gptelt-eval--clj-string-async
         (lambda (r) (setq result r))
         "(bad-code)" "user")))
    (should (string-match-p "Unable to resolve symbol" result))))

(ert-deftest gptelt-eval-clj-string-async-no-output-test ()
  "Test async CLJ eval with no value or error gives fallback message."
  (let ((result nil))
    (clj-test--with-mock-nrepl buf
      (cl-letf (((symbol-function 'nrepl-request:eval)
                 (lambda (input callback connection &optional ns &rest _)
                   (funcall callback (nrepl-dict "status" '("done") "id" "test-3")))))
        (gptelt-eval--clj-string-async
         (lambda (r) (setq result r))
         "(do nil)" "user")))
    (should (equal result "Evaluation completed with no output"))))

(ert-deftest gptelt-eval-clj-string-async-with-stdout-test ()
  "Test async CLJ eval with stdout output returns value, not stdout."
  (let ((result nil))
    (clj-test--with-mock-nrepl buf
      (cl-letf (((symbol-function 'nrepl-request:eval)
                 (lambda (input callback connection &optional ns &rest _)
                   (funcall callback (nrepl-dict "out" "hello from println\n"))
                   (funcall callback (nrepl-dict "value" "nil" "ns" "user"))
                   (funcall callback (nrepl-dict "status" '("done") "id" "test-4")))))
        (gptelt-eval--clj-string-async
         (lambda (r) (setq result r))
         "(println \"hello\")" "user")))
    (should (equal result "nil"))))

(ert-deftest gptelt-eval-clj-string-async-uses-namespace-test ()
  "Test async CLJ eval passes the correct namespace to nrepl-request:eval."
  (let ((captured-ns nil))
    (clj-test--with-mock-nrepl buf
      (cl-letf (((symbol-function 'nrepl-request:eval)
                 (lambda (input callback connection &optional ns &rest _)
                   (setq captured-ns ns)
                   (funcall callback (nrepl-dict "value" "ok"))
                   (funcall callback (nrepl-dict "status" '("done") "id" "test-5")))))
        (gptelt-eval--clj-string-async
         #'ignore "(+ 1 2)" "my.custom.ns")))
    (should (equal captured-ns "my.custom.ns"))))

(ert-deftest gptelt-eval-clj-string-async-default-namespace-test ()
  "Test async CLJ eval defaults to 'user' namespace."
  (let ((captured-ns nil))
    (clj-test--with-mock-nrepl buf
      (cl-letf (((symbol-function 'nrepl-request:eval)
                 (lambda (input callback connection &optional ns &rest _)
                   (setq captured-ns ns)
                   (funcall callback (nrepl-dict "value" "ok"))
                   (funcall callback (nrepl-dict "status" '("done") "id" "test-6")))))
        (gptelt-eval--clj-string-async
         #'ignore "(+ 1 2)")))
    (should (equal captured-ns "user"))))

;;; Tests for gptelt-clj-get-symbol-source-code-async

(ert-deftest gptelt-clj-get-symbol-source-code-async-success-test ()
  "Test async source code retrieval calls callback with source."
  (let ((result nil))
    (cl-letf (((symbol-function 'gptelt-clojure--ensure-workspace) #'ignore)
              ((symbol-function 'gptelt-eval--clj-string-async)
               (lambda (callback clj-string &optional ns)
                 (funcall callback "\"(defn foo [x] (+ x 1))\""))))
      (gptelt-clj-get-symbol-source-code-async
       (lambda (r) (setq result r))
       "foo"))
    (should (stringp result))
    (should (string-match-p "defn foo" result))))

(ert-deftest gptelt-clj-get-symbol-source-code-async-with-ns-test ()
  "Test async source code retrieval passes qualified symbol."
  (let ((captured-code nil))
    (cl-letf (((symbol-function 'gptelt-clojure--ensure-workspace) #'ignore)
              ((symbol-function 'gptelt-eval--clj-string-async)
               (lambda (callback clj-string &optional ns)
                 (setq captured-code clj-string)
                 (funcall callback "\"(defn bar [] :ok)\""))))
      (gptelt-clj-get-symbol-source-code-async
       #'ignore "bar" "my.ns"))
    (should (string-match-p "my.ns/bar" captured-code))))

(ert-deftest gptelt-clj-get-symbol-source-code-async-not-found-test ()
  "Test async source code for nonexistent symbol."
  (let ((result nil))
    (cl-letf (((symbol-function 'gptelt-clojure--ensure-workspace) #'ignore)
              ((symbol-function 'gptelt-eval--clj-string-async)
               (lambda (callback clj-string &optional ns)
                 (funcall callback "nil"))))
      (gptelt-clj-get-symbol-source-code-async
       (lambda (r) (setq result r))
       "nonexistent-sym"))
    (should (string-match-p "not found" result))))

;;; Tests for gptelt-clj-read-file-url-async

(ert-deftest gptelt-clj-read-file-url-async-success-test ()
  "Test async file URL reading calls callback with content."
  (let ((result nil))
    (cl-letf (((symbol-function 'gptelt-clojure--ensure-workspace) #'ignore)
              ((symbol-function 'gptelt-clj-ensure-helper-loaded) #'ignore)
              ((symbol-function 'gptelt-eval--clj-string-async)
               (lambda (callback clj-string &optional ns)
                 (funcall callback "\"file content here\""))))
      (gptelt-clj-read-file-url-async
       (lambda (r) (setq result r))
       "file:///some/path.clj"))
    (should (equal result "file content here"))))

(ert-deftest gptelt-clj-read-file-url-async-with-limit-offset-test ()
  "Test async file URL reading passes limit and offset in clj string."
  (let ((captured-code nil))
    (cl-letf (((symbol-function 'gptelt-clojure--ensure-workspace) #'ignore)
              ((symbol-function 'gptelt-clj-ensure-helper-loaded) #'ignore)
              ((symbol-function 'gptelt-eval--clj-string-async)
               (lambda (callback clj-string &optional ns)
                 (setq captured-code clj-string)
                 (funcall callback "\"content\""))))
      (gptelt-clj-read-file-url-async
       #'ignore "file:///path" 100 50))
    (should (string-match-p ":limit 100" captured-code))
    (should (string-match-p ":offset 50" captured-code))))

(ert-deftest gptelt-clj-read-file-url-async-error-test ()
  "Test async file URL reading with read-file-url failure."
  (let ((result nil))
    (cl-letf (((symbol-function 'gptelt-clojure--ensure-workspace) #'ignore)
              ((symbol-function 'gptelt-clj-ensure-helper-loaded) #'ignore)
              ((symbol-function 'gptelt-eval--clj-string-async)
               (lambda (callback clj-string &optional ns)
                 (funcall callback "\"Failed to read file url: not found\""))))
      (gptelt-clj-read-file-url-async
       (lambda (r) (setq result r))
       "file:///nonexistent"))
    (should (string-match-p "Error" result))))

(ert-deftest gptelt-clj-read-file-url-async-malformed-result-test ()
  "Test async file URL reading with unreadable result."
  (let ((result nil))
    (cl-letf (((symbol-function 'gptelt-clojure--ensure-workspace) #'ignore)
              ((symbol-function 'gptelt-clj-ensure-helper-loaded) #'ignore)
              ((symbol-function 'gptelt-eval--clj-string-async)
               (lambda (callback clj-string &optional ns)
                 (funcall callback "not-a-readable-form [["))))
      (gptelt-clj-read-file-url-async
       (lambda (r) (setq result r))
       "file:///path"))
    (should (string-match-p "Error reading file" result))))

;;; clj-test.el ends here
