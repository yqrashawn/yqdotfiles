;;; jina-test.el --- Tests for jina.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for Jina Reranker SDK

;;; Code:

(require 'ert)
(require 'jina)

(ert-deftest jina-reranker--get-api-key-test ()
  "Test API key retrieval."
  (let ((jina-api-key "test-key"))
    (should (string= "test-key" (jina-reranker--get-api-key))))
  
  (let ((jina-api-key (lambda () "lambda-key")))
    (should (string= "lambda-key" (jina-reranker--get-api-key))))
  
  (let ((jina-api-key nil))
    (with-environment-variables (("JINA_API_KEY" "env-key"))
      (should (string= "env-key" (jina-reranker--get-api-key)))))
  
  (let ((jina-api-key nil))
    (with-environment-variables (("JINA_API_KEY" nil))
      (should-error (jina-reranker--get-api-key)))))

(ert-deftest jina-reranker--make-headers-test ()
  "Test header creation."
  (let ((jina-api-key "test-key"))
    (let ((headers (jina-reranker--make-headers)))
      (should (string= "Bearer test-key" (cdr (assoc "Authorization" headers))))
      (should (string= "application/json" (cdr (assoc "Content-Type" headers))))
      (should (string= "application/json" (cdr (assoc "Accept" headers)))))))

(ert-deftest jina-rerank-validation-test ()
  "Test input validation."
  (let ((jina-api-key "test-key"))
    (should-error (jina-rerank-sync :documents '("doc1")))
    (should-error (jina-rerank-sync :query "test"))
    (should-error (jina-rerank-sync
                   :query "test"
                   :documents '("doc1")
                   :model "invalid-model"))))

(ert-deftest jina-rerank-payload-test ()
  "Test payload construction."
  (let ((jina-api-key "test-key"))
    (cl-letf (((symbol-function 'plz)
               (lambda (&rest args)
                 (let ((body (plist-get args :body)))
                   (json-encode `((test . ,body)))))))
      (let* ((result (jina-rerank-sync
                      :query "test query"
                      :documents '("doc1" "doc2")
                      :model "jina-reranker-v2-base-multilingual"
                      :top-n 1))
             (payload (json-read-from-string (alist-get 'test result))))
        (should (string= "test query" (alist-get 'query payload)))
        (should (equal ["doc1" "doc2"] (alist-get 'documents payload)))
        (should (string= "jina-reranker-v2-base-multilingual" (alist-get 'model payload)))
        (should (= 1 (alist-get 'top_n payload)))))))

(provide 'jina-test)
;;; jina-test.el ends here
