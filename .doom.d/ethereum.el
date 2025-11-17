;;; ethereum.el -*- lexical-binding: t; -*-

(defun +ethereum-request (chain method cb &rest params)
  (require 'request)
  (let ((url (alist-get chain +alchemy-end-point))
        (params (if params
                    (seq-into params 'vector)
                  [])))
    (request url
      :type "POST"
      :parser 'json-read
      :encoding 'utf-8
      :headers '(("Content-Type" . "application/json"))
      :data (json-encode
             `(("jsonrpc" . "2.0")
               ("method" . ,method)
               ("params" . ,params)))
      :success
      (cl-function
       (lambda (&key data &allow-other-keys)
         (let ((result (ignore-errors (thread-last data (assoc-default 'result)))))
           (funcall cb result))))
      :error
      (cl-function (lambda (&rest arg &key error-thrown &key data &allow-other-keys)
                     (message "Ethereum Error: %S" (thread-last data
                                                                (assoc-default 'error)
                                                                (assoc-default 'message))))))))
