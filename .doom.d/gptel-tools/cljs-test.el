;;; cljs-test.el --- Tests for cljs.el async functions -*- lexical-binding: t; -*-

(require 'ert)

;;; Tests for gptelt-cljs-list-ns-async

(ert-deftest gptelt-cljs-list-ns-async-test ()
  "Test async CLJS namespace listing calls callback with result."
  (let ((result nil))
    (cl-letf (((symbol-function 'gptelt-cljs-ensure-helper-loaded) #'ignore)
              ((symbol-function 'gptelt-eval--clj-string-async)
               (lambda (callback clj-string &optional ns)
                 (funcall callback "[\"cljs.core\" \"app.core\" \"reagent.core\"]"))))
      (gptelt-cljs-list-ns-async
       (lambda (r) (setq result r))
       ":app" 1))
    (should (string-match-p "cljs.core" result))))

(ert-deftest gptelt-cljs-list-ns-async-with-filter-test ()
  "Test async CLJS namespace listing passes regex filter."
  (let ((captured-code nil))
    (cl-letf (((symbol-function 'gptelt-cljs-ensure-helper-loaded) #'ignore)
              ((symbol-function 'gptelt-eval--clj-string-async)
               (lambda (callback clj-string &optional ns)
                 (setq captured-code clj-string)
                 (funcall callback "[\"app.core\"]"))))
      (gptelt-cljs-list-ns-async
       #'ignore ":app" 1 "app\\."))
    (should (string-match-p "\"app\\\\." captured-code))))

(ert-deftest gptelt-cljs-list-ns-async-no-filter-test ()
  "Test async CLJS namespace listing without filter omits regex param."
  (let ((captured-code nil))
    (cl-letf (((symbol-function 'gptelt-cljs-ensure-helper-loaded) #'ignore)
              ((symbol-function 'gptelt-eval--clj-string-async)
               (lambda (callback clj-string &optional ns)
                 (setq captured-code clj-string)
                 (funcall callback "[]"))))
      (gptelt-cljs-list-ns-async
       #'ignore ":app" 1))
    (should (string-match-p "(get-build-namespaces :app)" captured-code))
    (should-not (string-match-p "\"\"" captured-code))))

;;; Tests for gptelt-cljs-get-project-states-async

(ert-deftest gptelt-cljs-get-project-states-async-test ()
  "Test async project states calls callback with shadow-cljs info."
  (let ((result nil))
    (cl-letf (((symbol-function 'gptelt-cljs-ensure-helper-loaded) #'ignore)
              ((symbol-function 'gptelt-eval--clj-string-async)
               (lambda (callback clj-string &optional ns)
                 (funcall callback "{:builds [{:id :app :status :completed}]}"))))
      (gptelt-cljs-get-project-states-async
       (lambda (r) (setq result r))))
    (should (string-match-p ":app" result))
    (should (string-match-p ":completed" result))))

(ert-deftest gptelt-cljs-get-project-states-async-uses-cljs-helper-ns-test ()
  "Test async project states evaluates in cljs-helper namespace."
  (let ((captured-ns nil))
    (cl-letf (((symbol-function 'gptelt-cljs-ensure-helper-loaded) #'ignore)
              ((symbol-function 'gptelt-eval--clj-string-async)
               (lambda (callback clj-string &optional ns)
                 (setq captured-ns ns)
                 (funcall callback "{}"))))
      (gptelt-cljs-get-project-states-async #'ignore))
    (should (equal captured-ns "cljs-helper"))))

;;; Tests for gptelt-cljs-get-symbol-doc-async

(ert-deftest gptelt-cljs-get-symbol-doc-async-test ()
  "Test async CLJS symbol doc calls callback with documentation."
  (let ((result nil))
    (cl-letf (((symbol-function 'gptelt-cljs-ensure-helper-loaded) #'ignore)
              ((symbol-function 'gptelt-eval--clj-string-async)
               (lambda (callback clj-string &optional ns)
                 (funcall callback "\"Returns a map with docs for the symbol\""))))
      (gptelt-cljs-get-symbol-doc-async
       (lambda (r) (setq result r))
       "map" ":app" 1))
    (should (string-match-p "Returns a map" result))))

(ert-deftest gptelt-cljs-get-symbol-doc-async-with-namespace-test ()
  "Test async CLJS symbol doc passes namespace."
  (let ((captured-code nil))
    (cl-letf (((symbol-function 'gptelt-cljs-ensure-helper-loaded) #'ignore)
              ((symbol-function 'gptelt-eval--clj-string-async)
               (lambda (callback clj-string &optional ns)
                 (setq captured-code clj-string)
                 (funcall callback "\"doc here\""))))
      (gptelt-cljs-get-symbol-doc-async
       #'ignore "map" ":app" 1 "cljs.core"))
    (should (string-match-p "cljs.core" captured-code))))

;;; Tests for gptelt-cljs-get-symbol-source-code-async

(ert-deftest gptelt-cljs-get-symbol-source-code-async-test ()
  "Test async CLJS symbol source code retrieval."
  (let ((result nil))
    (cl-letf (((symbol-function 'gptelt-cljs-ensure-helper-loaded) #'ignore)
              ((symbol-function 'gptelt-eval--clj-string-async)
               (lambda (callback clj-string &optional ns)
                 (funcall callback "\"(defn my-fn [x] (inc x))\""))))
      (gptelt-cljs-get-symbol-source-code-async
       (lambda (r) (setq result r))
       "my-fn" ":app" 1))
    (should (string-match-p "defn my-fn" result))))

(ert-deftest gptelt-cljs-get-symbol-source-code-async-with-ns-test ()
  "Test async CLJS source code passes namespace in eval string."
  (let ((captured-code nil))
    (cl-letf (((symbol-function 'gptelt-cljs-ensure-helper-loaded) #'ignore)
              ((symbol-function 'gptelt-eval--clj-string-async)
               (lambda (callback clj-string &optional ns)
                 (setq captured-code clj-string)
                 (funcall callback "\"source\""))))
      (gptelt-cljs-get-symbol-source-code-async
       #'ignore "foo" ":app" 1 "my.ns"))
    (should (string-match-p "my.ns" captured-code))))

;;; Tests for gptelt-cljs-get-build-status-async

(ert-deftest gptelt-cljs-get-build-status-async-test ()
  "Test async build status calls callback with status info."
  (let ((result nil))
    (cl-letf (((symbol-function 'gptelt-cljs-ensure-helper-loaded) #'ignore)
              ((symbol-function 'gptelt-eval--clj-string-async)
               (lambda (callback clj-string &optional ns)
                 (funcall callback "{:status :completed :duration 1234}"))))
      (gptelt-cljs-get-build-status-async
       (lambda (r) (setq result r))))
    (should (string-match-p ":completed" result))))

(ert-deftest gptelt-cljs-get-build-status-async-with-build-id-test ()
  "Test async build status passes build ID in eval string."
  (let ((captured-code nil))
    (cl-letf (((symbol-function 'gptelt-cljs-ensure-helper-loaded) #'ignore)
              ((symbol-function 'gptelt-eval--clj-string-async)
               (lambda (callback clj-string &optional ns)
                 (setq captured-code clj-string)
                 (funcall callback "{}"))))
      (gptelt-cljs-get-build-status-async
       #'ignore ":app"))
    (should (string-match-p "(get-build-status :app)" captured-code))))

(ert-deftest gptelt-cljs-get-build-status-async-no-build-id-test ()
  "Test async build status without build ID gets all builds."
  (let ((captured-code nil))
    (cl-letf (((symbol-function 'gptelt-cljs-ensure-helper-loaded) #'ignore)
              ((symbol-function 'gptelt-eval--clj-string-async)
               (lambda (callback clj-string &optional ns)
                 (setq captured-code clj-string)
                 (funcall callback "{}"))))
      (gptelt-cljs-get-build-status-async
       #'ignore))
    (should (equal captured-code "(get-build-status)"))))

;;; Tests for gptel-cljs-eval-string-async

(ert-deftest gptel-cljs-eval-string-async-test ()
  "Test async CLJS eval calls gptelt-eval--clj-string-async with correct format."
  (let ((captured-code nil)
        (result nil))
    (cl-letf (((symbol-function 'gptelt-cljs-ensure-helper-loaded) #'ignore)
              ((symbol-function 'gptelt-eval--clj-string-async)
               (lambda (callback clj-string &optional ns)
                 (setq captured-code clj-string)
                 (funcall callback "\"hello from cljs\""))))
      (gptel-cljs-eval-string-async
       (lambda (r) (setq result r))
       ":app" 1 "js/document.title"))
    (should (string-match-p "cljs-eval" captured-code))
    (should (string-match-p ":app" captured-code))
    (should (equal result "\"hello from cljs\""))))

;;; Tests for error handling in async wrappers

(ert-deftest gptelt-cljs-list-ns-async-error-test ()
  "Test async CLJS list ns calls callback with error when helper loading fails."
  (let ((result nil))
    (cl-letf (((symbol-function 'gptelt-cljs-ensure-helper-loaded)
               (lambda () (error "nREPL not connected"))))
      (gptelt-cljs-list-ns-async
       (lambda (r) (setq result r))
       ":app" 1))
    (should (stringp result))
    (should (string-match-p "nREPL not connected" result))))

(ert-deftest gptelt-cljs-get-project-states-async-error-test ()
  "Test async project states calls callback with error when helper loading fails."
  (let ((result nil))
    (cl-letf (((symbol-function 'gptelt-cljs-ensure-helper-loaded)
               (lambda () (error "nREPL not connected"))))
      (gptelt-cljs-get-project-states-async
       (lambda (r) (setq result r))))
    (should (stringp result))
    (should (string-match-p "nREPL not connected" result))))

(ert-deftest gptel-cljs-eval-string-async-error-test ()
  "Test async CLJS eval calls callback with error when helper loading fails."
  (let ((result nil))
    (cl-letf (((symbol-function 'gptelt-cljs-ensure-helper-loaded)
               (lambda () (error "nREPL not connected"))))
      (gptel-cljs-eval-string-async
       (lambda (r) (setq result r))
       ":app" 1 "js/document.title"))
    (should (stringp result))
    (should (string-match-p "nREPL not connected" result))))

(ert-deftest gptelt-cljs-get-symbol-doc-async-error-test ()
  "Test async CLJS symbol doc calls callback with error when helper loading fails."
  (let ((result nil))
    (cl-letf (((symbol-function 'gptelt-cljs-ensure-helper-loaded)
               (lambda () (error "nREPL not connected"))))
      (gptelt-cljs-get-symbol-doc-async
       (lambda (r) (setq result r))
       "map" ":app" 1))
    (should (stringp result))
    (should (string-match-p "nREPL not connected" result))))

(ert-deftest gptelt-cljs-get-symbol-source-code-async-error-test ()
  "Test async CLJS source code calls callback with error when helper loading fails."
  (let ((result nil))
    (cl-letf (((symbol-function 'gptelt-cljs-ensure-helper-loaded)
               (lambda () (error "nREPL not connected"))))
      (gptelt-cljs-get-symbol-source-code-async
       (lambda (r) (setq result r))
       "map" ":app" 1))
    (should (stringp result))
    (should (string-match-p "nREPL not connected" result))))

(ert-deftest gptelt-cljs-get-build-status-async-error-test ()
  "Test async build status calls callback with error when helper loading fails."
  (let ((result nil))
    (cl-letf (((symbol-function 'gptelt-cljs-ensure-helper-loaded)
               (lambda () (error "nREPL not connected"))))
      (gptelt-cljs-get-build-status-async
       (lambda (r) (setq result r))))
    (should (stringp result))
    (should (string-match-p "nREPL not connected" result))))

;;; cljs-test.el ends here
