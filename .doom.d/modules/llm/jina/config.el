;;; llm/jina/config.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 yqrashawn
;;
;; Author: yqrashawn <namy.19@gmail.com>
;; Maintainer: yqrashawn <namy.19@gmail.com>
;; Created: November 09, 2025
;; Modified: November 09, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/yqrashawn/jina
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Jina AI Reranker SDK for Emacs.
;;
;; Usage:
;;   (jina-rerank
;;    :query "machine learning"
;;    :documents '("Deep learning" "Database optimization" "ML in healthcare")
;;    :model "jina-reranker-v2-base-multilingual"
;;    :top-n 2
;;    :then (lambda (result) (message "Result: %S" result)))
;;
;;; Code:

(use-package! plz
  :commands (plz))

;;;; Variables

(defvar jina-api-key ""
  "Jina AI API key.
Get your Jina AI API key for free: https://jina.ai/?sui=apikey
Can be a string or a function that returns a string.")

(defvar jina-reranker-base-url "https://api.jina.ai/v1"
  "Base URL for Jina AI Reranker API.")

(defvar jina-reranker-models
  '(("jina-reranker-m0" . "2.4B")
    ("jina-reranker-v2-base-multilingual" . "278M")
    ("jina-colbert-v2" . "560M")
    ("jina-reranker-v3" . "0.6B"))
  "Available Jina reranker models.")

;;;; Functions
(defun jina-reranker--get-api-key ()
  "Get the Jina API key."
  (let ((key (or jina-api-key
                 (getenv "JINA_API_KEY"))))
    (when (functionp key)
      (setq key (funcall key)))
    (unless key
      (error "Jina API key not set. Set `jina-api-key' or JINA_API_KEY environment variable"))
    key))

(defun jina-reranker--make-headers ()
  "Make headers for Jina API request."
  (list (cons "Authorization" (format "Bearer %s" (jina-reranker--get-api-key)))
        (cons "Content-Type" "application/json")
        (cons "Accept" "application/json")
        +jina-custom-header))

(cl-defun jina-rerank (&key query documents
                            (model "jina-reranker-v2-base-multilingual")
                            top-n
                            (return-documents t)
                            then else finally)
  "Rerank DOCUMENTS by relevance to QUERY using Jina Reranker API.

Required arguments:
  QUERY - Search query string
  DOCUMENTS - List of document strings to rerank

Optional arguments:
  MODEL - Model to use (default: jina-reranker-v2-base-multilingual)
          Options: jina-reranker-v2-base-multilingual, jina-reranker-m0, jina-colbert-v2
  TOP-N - Number of top results to return (default: all documents)
  RETURN-DOCUMENTS - If t, return documents with results (default: t)

Callback arguments:
  THEN - Success callback function, receives parsed response
  ELSE - Error callback function, receives error
  FINALLY - Function called after THEN or ELSE

Returns:
  For synchronous requests (no THEN): parsed response
  For asynchronous requests (with THEN): the plz process object

Example:
  (jina-rerank
   :query \"machine learning\"
   :documents '(\"Deep learning models\" \"Database optimization\" \"ML in healthcare\")
   :model \"jina-reranker-v2-base-multilingual\"
   :top-n 2
   :then (lambda (result)
           (message \"Top results: %S\" (alist-get 'results result))))"
  (unless query
    (error "Query is required"))
  (unless documents
    (error "Documents are required"))
  (unless (member model (mapcar #'car jina-reranker-models))
    (error "Invalid model: %s. Valid models: %s"
           model (mapconcat #'car jina-reranker-models ", ")))
  (let* ((url (concat jina-reranker-base-url "/reranker"))
         (payload (list (cons "model" model)
                        (cons "query" query)
                        (cons "documents" (vconcat documents))))
         (sync-p (not then)))
    (when top-n
      (push (cons "top_n" top-n) payload))
    (when (not return-documents)
      (push (cons "return_documents" :json-false) payload))
    (if sync-p
        (condition-case err
            (json-read-from-string
             (plz 'post url
               :headers (jina-reranker--make-headers)
               :body (json-encode payload)
               :as 'string))
          (plz-error
           (if else
               (funcall else err)
             (signal (car err) (cdr err)))))
      (plz 'post url
        :headers (jina-reranker--make-headers)
        :body (json-encode payload)
        :as #'json-read
        :then then
        :else else
        :finally finally))))

(cl-defun jina-rerank-sync (&rest args)
  "Synchronous version of `jina-rerank'.
Accepts the same arguments as `jina-rerank' except THEN, ELSE, and FINALLY.
Returns the parsed response directly."
  (let ((filtered-args (cl-loop for (key val) on args by #'cddr
                                unless (memq key '(:then :else :finally))
                                append (list key val))))
    (apply #'jina-rerank filtered-args)))

(comment
  (setq jina-reranker-base-url +jina-reranker-base-url)
  (jina-rerank
   :query "machine learning"
   :documents '("Deep learning" "Database optimization" "ML in healthcare")
   :model "jina-reranker-v2-base-multilingual"
   :top-n 2
   :then (lambda (result) (message "Results: %S" result))
   :else 'message))
