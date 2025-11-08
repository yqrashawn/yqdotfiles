;;; embeddings.el --- GPTEL embeddings tool -*- lexical-binding: t; -*-
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Homepage: https://github.com/yqrashawn/gptel-tools
;; Keywords: tools, embedding

;;; Commentary:
;;
;; Embedding utilities for GPTEL.
;;
;;; Code:

(require 'cl-lib)
(require 'llm)
(require 'llm-provider-utils)
(require 'json)
(require 'plz)


(cl-defstruct (llm-jina (:include llm-standard-full-provider))
  "A structure for holding information needed by Jina AI's API.

KEY is the API key for Jina AI, which is required.

EMBEDDING-MODEL is the model to use for embeddings.  If unset, it
will use jina-embeddings-v4 as default.

TASK is the intended downstream application for optimizing embeddings.
For code models: nl2code.query, nl2code.passage, code2code.query, etc.
For text models: retrieval.query, retrieval.passage, text-matching, etc.

DIMENSIONS can truncate output embeddings to specified size if set.

CUSTOM-HEADERS is an alist of custom headers to add to requests.
For example: '((\"x-auth\" . \"foobar\") (\"x-custom\" . \"value\"))

BASE-URL is the base URL for the API endpoint.  If unset, it will
use the default Jina AI endpoint."
  key
  (embedding-model "jina-embeddings-v4")
  task
  dimensions
  custom-headers
  (base-url "https://api.jina.ai/v1"))

(cl-defmethod llm-provider-embedding-url ((provider llm-jina) &optional _)
  "Return the URL for embeddings for the Jina PROVIDER."
  (concat (llm-jina-base-url provider)
          (unless (string-suffix-p "/" (llm-jina-base-url provider)) "/")
          "embeddings"))

(cl-defmethod llm-provider-request-prelude ((provider llm-jina))
  "Check that the key is set for the Jina PROVIDER."
  (unless (llm-jina-key provider)
    (error "To call Jina AI API, add a key to the `llm-jina' provider")))

(cl-defmethod llm-provider-headers ((provider llm-jina))
  "Return the headers for the Jina PROVIDER."
  (let ((headers '()))
    (when-let ((key (llm-jina-key provider)))
      (when (functionp key)
        (setq key (funcall key)))
      (push (cons "Authorization" (format "Bearer %s" (encode-coding-string key 'utf-8))) headers))
    (when-let ((custom-headers (llm-jina-custom-headers provider)))
      (setq headers (append headers custom-headers)))
    headers))

(cl-defmethod llm-provider-embedding-request ((provider llm-jina) string)
  "Return the request for the Jina PROVIDER for STRING."
  (let ((request `(:model ,(llm-jina-embedding-model provider)
                   :input ,string)))
    (when-let ((task (llm-jina-task provider)))
      (setq request (plist-put request :task task)))
    (when-let ((dimensions (llm-jina-dimensions provider)))
      (setq request (plist-put request :dimensions dimensions)))
    request))

(cl-defmethod llm-provider-batch-embeddings-request ((provider llm-jina) string-list)
  "Return the batch request for the Jina PROVIDER for STRING-LIST."
  (llm-provider-embedding-request provider (apply #'vector string-list)))

(cl-defmethod llm-provider-embedding-extract-result ((_ llm-jina) response)
  "Return the embedding from the server RESPONSE."
  (assoc-default 'embedding (aref (assoc-default 'data response) 0)))

(cl-defmethod llm-provider-batch-embeddings-extract-result ((_ llm-jina) response)
  "Return the embeddings from the server RESPONSE for batch request."
  (let* ((data (assoc-default 'data response))
         (vec (make-vector (length data) nil)))
    (mapc (lambda (d)
            (aset vec (assoc-default 'index d)
                  (assoc-default 'embedding d)))
          data)
    (append vec nil)))

(cl-defmethod llm-provider-embedding-extract-error ((_ llm-jina) err-response)
  "Return an error message from ERR-RESPONSE for Jina provider."
  (if-let ((detail (assoc-default 'detail err-response)))
      (if (vectorp detail)
          (format "Jina AI returned error: %s" (mapconcat (lambda (e) (assoc-default 'message e)) detail ", "))
        (format "Jina AI returned error: %s" detail))
    (when-let ((errdata (assoc-default 'error err-response)))
      (format "Jina AI returned error: %s"
              (or (cdr (assoc 'message errdata)) "unknown error")))))

(cl-defmethod llm-name ((_ llm-jina))
  "Return the name of the Jina provider."
  "Jina AI")

(cl-defmethod llm-capabilities ((_ llm-jina))
  "Return the capabilities of the Jina provider."
  '(embeddings embeddings-batch))

(defun llm-make-jina (&rest args)
  "Create a Jina AI provider with ARGS.
Get your Jina AI API key for free: https://jina.ai/?sui=apikey"
  (apply #'make-llm-jina args))

(comment
  (setq jina-provider
        (llm-make-jina
         :base-url +jina-base-url
         :key "nokey"
         :embedding-model "jina-embeddings-v4"
         :dimensions 2048
         :task "retrieval.passage"
         :custom-headers (list +jina-custom-header)))

  (llm-embedding jina-provider "Hello, world!")

  (llm-batch-embeddings jina-provider '("Hello" "world"))

  (defun +llm-jina-code-embeddings (code-list &optional on-ok on-err)
    "Get code embeddings for CODE-LIST using Jina code model."
    (let ((provider (llm-make-jina
                     :key (getenv "JINA_API_KEY")
                     :embedding-model "jina-code-embeddings-1.5b"
                     :task "nl2code.passage")))
      (llm-batch-embeddings-async
       provider
       code-list
       (or on-ok (lambda (result) (message "[EMBEDDINGS]\n%S" result)))
       (or on-err (lambda (type msg) (message "[ERROR-EMBEDDINGS] %s: %s" type msg))))))

  (defun +llm-jina-query-embeddings (query-list &optional on-ok on-err)
    "Get query embeddings for QUERY-LIST using Jina code model."
    (let ((provider (llm-make-jina
                     :key (getenv "JINA_API_KEY")
                     :embedding-model "jina-code-embeddings-1.5b"
                     :task "nl2code.query")))
      (llm-batch-embeddings-async
       provider
       query-list
       (or on-ok (lambda (result) (message "[EMBEDDINGS]\n%S" result)))
       (or on-err (lambda (type msg) (message "[ERROR-EMBEDDINGS] %s: %s" type msg))))))

  (+llm-jina-code-embeddings '("foo" "bar"))
  (+llm-jina-query-embeddings '("foo" "bar")))

;;; embeddings.el ends here
