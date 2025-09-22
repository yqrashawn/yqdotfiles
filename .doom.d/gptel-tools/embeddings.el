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

(after! llm
  (cl-defstruct (llm-jina (:include llm-standard-full-provider))
    "A structure for holding information needed by Jina AI's API.

API-KEY is the API key for Jina AI, which is optional for some endpoints.

EMBEDDING-MODEL is the model to use for embeddings.  If unset, it
will use jina-code-embeddings-1.5b as default.

BASE-URL is the base URL for the API endpoint.  If unset, it will
use the default Jina AI endpoint."
    (api-key "")
    (embedding-model "jina-code-embeddings-1.5b")
    (embedding-task "nl2code.passage")
    (base-url "https://api.jina.ai/v1"))

  (cl-defmethod llm-provider-embedding-url ((provider llm-jina) &optional _)
    "Return the URL for embeddings for the Jina PROVIDER."
    (concat (llm-jina-base-url provider)
            (unless (string-suffix-p "/" (llm-jina-base-url provider)) "/")
            "embeddings"))

  (cl-defmethod llm-provider-headers ((provider llm-jina))
    "Return the headers for the Jina PROVIDER."
    (when-let ((key (llm-jina-api-key provider)))
      (when (functionp key)
        (setq key (funcall key)))
      `(("Authorization" . ,(format "Bearer %s" (encode-coding-string key 'utf-8)))
        ("Content-Type" . "application/json"))))

  (cl-defmethod llm-provider-embedding-request ((provider llm-jina) string)
    "Return the request for the Jina PROVIDER for STRING."
    (let ((request `(:model ,(llm-jina-embedding-model provider)
                     :input ,string
                     :embedding_type "float")))
      ;; Add task-specific parameters for code embedding models
      (when (string-match-p "code-embeddings" (llm-jina-embedding-model provider))
        (setq request (plist-put request :task "nl2code.passage")))
      (when-let ((task (llm-jina-embedding-task provider)))
        (setq request (plist-put request :task task)))
      request))

  (cl-defmethod llm-provider-batch-embeddings-request ((provider llm-jina) string-list)
    "Return the batch request for the Jina PROVIDER for STRING-LIST."
    (let ((request `(:model ,(llm-jina-embedding-model provider)
                     :input ,(apply #'vector string-list)
                     :embedding_type "float")))
      ;; Add task-specific parameters for code embedding models
      (when (string-match-p "code-embeddings" (llm-jina-embedding-model provider))
        (setq request (plist-put request :task "nl2code.passage")))
      (when-let ((task (llm-jina-embedding-task provider)))
        (setq request (plist-put request :task task)))
      request))

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
    (let ((errdata (assoc-default 'error err-response)))
      (when errdata
        (format "Jina AI returned error: %s message: %s"
                (or (cdr (assoc 'type errdata)) "unknown")
                (or (cdr (assoc 'message errdata)) "unknown error")))))

  (cl-defmethod llm-name ((_ llm-jina))
    "Return the name of the Jina provider."
    "Jina AI")

  (cl-defmethod llm-capabilities ((_ llm-jina))
    "Return the capabilities of the Jina provider."
    '(embeddings embeddings-batch)))

(comment
  (setq jina1 (+llm-make-jina))

  (defun +llm-jina-code-embeddings (code-list &optional on-ok on-err)
    (let* ((provider (make-llm-jina)))
      (llm-batch-embeddings-async
       provider
       code-list
       (or on-ok (clj/partial 'message "[EMBEDDINGS]\n%s"))
       (or on-err (clj/partial 'message "[ERROR-EMBEDDINGS]\n%s")))))

  (defun +llm-jina-query-embeddings (query-list &optional on-ok on-err)
    (let* ((provider (make-llm-jina :embedding-task "nl2code.query")))
      (llm-batch-embeddings-async
       provider
       query-list
       (or on-ok (clj/partial 'message "[EMBEDDINGS]\n%s"))
       (or on-err (clj/partial 'message "[ERROR-EMBEDDINGS]\n%s")))))

  ;; list of 2 vector
  (setq jkjk (+llm-jina-code-embeddings '("foo" "bar")))
  (setq jkjkj (+llm-jina-query-embeddings '("foo" "bar"))))
