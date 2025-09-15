;;; .nixpkgs/.doom.d/clj-test.el -*- lexical-binding: t; coding: utf-8 -*-

(require 'ert)
(require 'cl-lib)

(comment
  (ert-run-tests-interactively "clj-test-")
  (ert "clj-test-"))

;;; Tests for basic collection operations

(ert-deftest clj-test-get ()
  "Test clj/get function with various collection types."
  ;; Hash table
  (let ((ht (make-hash-table :test 'equal)))
    (puthash "key" "value" ht)
    (should (equal "value" (clj/get ht "key")))
    (should (equal "default" (clj/get ht "missing" "default")))
    (should (equal nil (clj/get ht "missing"))))

  ;; Array/vector
  (let ((arr [1 2 3 4]))
    (should (equal 1 (clj/get arr 0)))
    (should (equal 3 (clj/get arr 2)))
    (should (equal "default" (clj/get arr 10 "default")))
    (should (equal nil (clj/get arr 10))))

  ;; Plist
  (let ((plist '(:a 1 :b 2 :c 3)))
    (should (equal 1 (clj/get plist :a)))
    (should (equal 2 (clj/get plist :b)))
    (should (equal "default" (clj/get plist :missing "default"))))

  ;; Alist
  (let ((alist '((a . 1) (b . 2) (c . 3))))
    (should (equal 1 (clj/get alist 'a)))
    (should (equal 2 (clj/get alist 'b)))
    (should (equal "default" (clj/get alist 'missing "default")))))

(ert-deftest clj-test-get-in ()
  "Test clj/get-in function with nested structures."
  (let ((nested '((users . ((alice . ((name . "Alice") (age . 30)))
                            (bob . ((name . "Bob") (age . 25))))))))
    (should (equal "Alice" (clj/get-in nested '(users alice name))))
    (should (equal 30 (clj/get-in nested '(users alice age))))
    (should (equal "default" (clj/get-in nested '(users charlie name) "default")))
    (should (equal nil (clj/get-in nested '(missing key))))))

(ert-deftest clj-test-assoc ()
  "Test clj/assoc function with various collection types."
  ;; Hash table
  (let* ((ht (make-hash-table :test 'equal))
         (_ (puthash "a" 1 ht))
         (result (clj/assoc ht "b" 2)))
    (should (equal 1 (gethash "a" result)))
    (should (equal 2 (gethash "b" result)))
    (should (not (gethash "b" ht)))) ; Original unchanged

  ;; Array
  (let* ((arr [1 2 3])
         (result (clj/assoc arr 1 "two")))
    (should (equal [1 "two" 3] result))
    (should (equal [1 2 3] arr))) ; Original unchanged

  ;; Alist
  (let* ((alist '((a . 1) (b . 2)))
         (result (clj/assoc alist 'c 3)))
    (should (equal 3 (cdr (assoc 'c result))))
    (should (equal 1 (cdr (assoc 'a result))))))

(ert-deftest clj-test-assoc-in ()
  "Test clj/assoc-in function with nested structures."
  (let* ((nested '((users . ((alice . ((name . "Alice") (age . 30)))))))
         (result (clj/assoc-in nested '(users alice age) 31)))
    (should (equal 31 (clj/get-in result '(users alice age))))
    (should (equal "Alice" (clj/get-in result '(users alice name))))
    ;; Original unchanged
    (should (equal 30 (clj/get-in nested '(users alice age))))))

(ert-deftest clj-test-update ()
  "Test clj/update function."
  (let* ((alist '((count . 5) (name . "test")))
         (result (clj/update alist 'count '+ 3)))
    (should (equal 8 (cdr (assoc 'count result))))
    (should (equal 5 (cdr (assoc 'count alist)))))) ; Original unchanged

(ert-deftest clj-test-update-in ()
  "Test clj/update-in function with nested structures."
  (let* ((nested '((user . ((stats . ((score . 100)))))))
         (result (clj/update-in nested '(user stats score) '+ 50)))
    (should (equal 150 (clj/get-in result '(user stats score))))
    ;; Original unchanged
    (should (equal 100 (clj/get-in nested '(user stats score))))))

(ert-deftest clj-test-merge ()
  "Test clj/merge function."
  (let* ((map1 '((a . 1) (b . 2)))
         (map2 '((b . 3) (c . 4)))
         (result (clj/merge map1 map2)))
    (should (equal 1 (cdr (assoc 'a result))))
    (should (equal 3 (cdr (assoc 'b result)))) ; map2 wins
    (should (equal 4 (cdr (assoc 'c result))))))

;;; Tests for sequence operations

(ert-deftest clj-test-first ()
  "Test clj/first function."
  (should (equal 1 (clj/first '(1 2 3))))
  (should (equal 'a (clj/first [a b c])))
  (should (equal ?h (clj/first "hello")))
  (should (equal nil (clj/first nil)))
  (should (equal nil (clj/first []))))

(ert-deftest clj-test-second ()
  "Test clj/second function."
  (should (equal 2 (clj/second '(1 2 3))))
  (should (equal 'b (clj/second [a b c])))
  (should (equal ?e (clj/second "hello")))
  (should (equal nil (clj/second nil)))
  (should (equal nil (clj/second [a]))))

(ert-deftest clj-test-conj ()
  "Test clj/conj function."
  ;; Lists - prepend
  (should (equal '(3 1 2) (clj/conj '(1 2) 3)))
  (should (equal '(3 4 1 2) (clj/conj '(1 2) 3 4)))

  ;; Vectors - append
  (should (equal [1 2 3] (clj/conj [1 2] 3)))
  (should (equal [1 2 3 4] (clj/conj [1 2] 3 4)))
  (should (equal [?1 ?2 ?3 4] (clj/conj "123" 4))))

(ert-deftest clj-test-into ()
  "Test clj/into function."
  (should (equal '(1 2 3) (clj/into '() [1 2 3])))
  (should (equal [1 2 3 4] (clj/into [1 2] [3 4])))
  (should (equal '(1 2 3 4) (clj/into '(1 2) '(3 4)))))

;;; Tests for functional operations

(ert-deftest clj-test-identity ()
  "Test clj/identity function."
  (should (equal 42 (clj/identity 42)))
  (should (equal "hello" (clj/identity "hello")))
  (should (equal '(1 2 3) (clj/identity '(1 2 3))))
  (should (equal nil (clj/identity nil))))

(ert-deftest clj-test-constantly ()
  "Test clj/constantly function."
  (let ((f (clj/constantly 42)))
    (should (equal 42 (funcall f)))
    (should (equal 42 (funcall f 1 2 3)))
    (should (equal 42 (funcall f "any" "args")))))

(ert-deftest clj-test-comp ()
  "Test clj/comp function."
  ;; No functions - should return identity
  (let ((f (clj/comp)))
    (should (equal 5 (funcall f 5))))

  ;; Single function
  (let ((f (clj/comp '1+)))
    (should (equal 6 (funcall f 5))))

  ;; Multiple functions - right to left composition
  (let ((f (clj/comp '1+ '1+ '*)))
    (should (equal 6 (funcall f 2 2)))) ; (* 2 2) -> (1+ 4) -> (1+ 5) = 6

  ;; Test with lambda functions
  (let ((f (clj/comp (lambda (x) (* x 2)) (lambda (x) (+ x 1)))))
    (should (equal 6 (funcall f 2))))) ; (+ 2 1) -> (* 3 2) = 6

(ert-deftest clj-test-partial ()
  "Test clj/partial function."
  ;; Partial application with +
  (let ((add5 (clj/partial '+ 5)))
    (should (equal 8 (funcall add5 3)))
    (should (equal 15 (funcall add5 10))))

  ;; Partial application with multiple args
  (let ((add-many (clj/partial '+ 1 2 3)))
    (should (equal 10 (funcall add-many 4)))
    (should (equal 16 (funcall add-many 4 6))))

  ;; Partial with list function
  (let ((prepend-hello (clj/partial 'list "hello")))
    (should (equal '("hello" "world") (funcall prepend-hello "world")))))

(ert-deftest clj-test-map ()
  "Test clj/map function."
  ;; Single collection
  (should (equal '(2 3 4) (clj/map '1+ '(1 2 3))))
  (should (equal '(1 4 9) (clj/map (lambda (x) (* x x)) '(1 2 3))))

  ;; Multiple collections
  (should (equal '(5 7 9) (clj/map '+ '(1 2 3) '(4 5 6))))
  (should (equal '(6 8 10) (clj/map '+ '(1 2 3) '(2 3 4) '(3 3 3)))))

(ert-deftest clj-test-filter ()
  "Test clj/filter function."
  (should (equal '(2 4 6) (clj/filter 'cl-evenp '(1 2 3 4 5 6))))
  (should (equal '(1 3 5) (clj/filter 'cl-oddp '(1 2 3 4 5 6))))
  (should (equal '() (clj/filter 'cl-evenp '(1 3 5))))
  (should (equal '(2 4) (clj/filter 'cl-evenp '(2 4)))))

(ert-deftest clj-test-remove ()
  "Test clj/remove function."
  (should (equal '(1 3 5) (clj/remove 'cl-evenp '(1 2 3 4 5 6))))
  (should (equal '(2 4 6) (clj/remove 'cl-oddp '(1 2 3 4 5 6))))
  (should (equal '(1 3 5) (clj/remove 'cl-evenp '(1 3 5))))
  (should (equal '() (clj/remove (lambda (_) t) '(1 2 3)))))

(ert-deftest clj-test-some ()
  "Test clj/some function."
  (should (equal 4 (clj/some (lambda (x) (when (cl-evenp x) x)) '(1 3 4 5))))
  (should (equal nil (clj/some 'cl-evenp '(1 3 5))))
  (should (equal t (clj/some 'cl-evenp '(2 4 6))))
  (should (equal nil (clj/some 'cl-oddp '()))))

;;; Tests for threading macros

(ert-deftest clj-test-thread-first ()
  "Test clj/-> (thread-first) macro."
  ;; Simple threading
  (should (equal 6 (clj/-> 5 1+)))
  (should (equal 7 (clj/-> 5 1+ 1+)))

  ;; Threading with function calls
  (should (equal 8 (clj/-> 5 (+ 2) 1+)))
  (should (equal 10 (clj/-> 5 (+ 2) (* 2) (- 4))))

  ;; Threading through multiple forms
  (should (equal "6" (clj/-> 5 1+ number-to-string))))

(ert-deftest clj-test-thread-last ()
  "Test clj/->> (thread-last) macro."
  ;; Simple threading
  (should (equal '(6) (clj/->> '(5) (clj/map '1+))))

  ;; Threading with function calls
  (should (equal 7 (clj/->> 5 (+ 2))))
  (should (equal 14 (clj/->> 5 (+ 2) (* 2))))

  ;; Threading through list operations
  (should (equal '(2 3 4) (clj/->> '(1 2 3) (mapcar '1+))))
  (should (equal '(2 4) (clj/->> '(1 2 3 4) (cl-remove-if 'cl-oddp)))))

;;; Tests for doseq macro

(ert-deftest clj-test-doseq ()
  "Test clj/doseq macro."
  ;; Single collection
  (let ((result '()))
    (clj/doseq (x '(1 2 3))
               (push (* x 2) result))
    (should (equal '(6 4 2) result)))

  ;; Multiple collections (nested)
  (let ((result '()))
    (clj/doseq (x '(1 2) y '(a b))
               (push (list x y) result))
    (should (equal '((2 b) (2 a) (1 b) (1 a)) result)))

  ;; Empty collection
  (let ((result '()))
    (clj/doseq (x '())
               (push x result))
    (should (equal '() result))))

;;; Integration tests

(ert-deftest clj-test-integration ()
  "Test combinations of functions working together."
  ;; Map + filter + thread-last
  (should (equal '(4 16 36)
                 (clj/->> '(1 2 3 4 5 6)
                          (clj/filter 'cl-evenp)
                          (clj/map (lambda (x) (* x x))))))

  ;; Complex nested operations
  (let* ((data '((users . ((alice . ((posts . (1 2 3))))
                           (bob . ((posts . (4 5))))))
                 (stats . ((total . 100)))))
         (alice-posts (clj/get-in data '(users alice posts)))
         (result (clj/->> alice-posts
                          (clj/map (clj/partial '* 2))
                          (clj/filter (lambda (x) (> x 4))))))
    (should (equal '(6) result))))

(provide 'clj-test)

;;; clj-test.el ends here
