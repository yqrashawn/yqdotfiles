;;; gpt.el -*- lexical-binding: t; -*-

;; (use-package! llm
;;   :defer t
;;   :init
;;   (setq! llm-warn-on-nonfree nil))

(defvar +gpt-system-message "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")

(defun +gpt-request (message cb &optional system-message)
  (require 'request)
  (require 'seq)
  (request "https://api.openai.com/v1/chat/completions"
    :type "POST"
    :parser 'json-read
    :encoding 'utf-8
    :headers `(("Content-Type" . "application/json")
               ("Authorization" . ,(concat "Bearer " +open-ai-api-key)))
    :data (json-encode
           `(("model" . "gpt-3.5-turbo-16k")
             ("temperature" . 1.0)
             ("messages" . [(("role" . "system")
                             ("content" . ,(or system-message +gpt-system-message)))
                            (("role" . "user")
                             ("content" . ,message))])))
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (let ((msg (ignore-errors (thread-last data
                                              (assoc-default 'choices)
                                              (seq-first)
                                              (assoc-default 'message)
                                              (assoc-default 'content)))))
         (funcall cb msg))))
    :error
    (cl-function (lambda (&rest arg &key error-thrown &key data &allow-other-keys)
                   ;; (message "OpenAI Error: %S" error-thrown)
                   (message "OpenAI Error: %S" (thread-last data
                                                            (assoc-default 'error)
                                                            (assoc-default 'message)))))))

;;;###autoload
(defun +summarize-current-elfeed-show-buffer ()
  (interactive)
  (require 's)
  (when (eq major-mode 'elfeed-show-mode)
    (let* ((buf (current-buffer))
           (title (elfeed-entry-title elfeed-show-entry))
           (link (elfeed-entry-link elfeed-show-entry))
           (date (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
           (nicedate (format-time-string "%a, %e %b %Y %T %Z" date))
           (authors (elfeed-meta elfeed-show-entry :authors))
           (niceauthors (s-join "," (seq-map 'elfeed--show-format-author authors)))
           ;; (content (elfeed-deref (elfeed-entry-content elfeed-show-entry)))
           ;; (type (elfeed-entry-content-type elfeed-show-entry))
           (feed (elfeed-entry-feed elfeed-show-entry))
           (feed-title (elfeed-feed-title feed))
           (txt-content (with-current-buffer (current-buffer) (buffer-string)))
           (gpt-message (format "%s
Artical metadata:
```txt
From feed: %s
Title: %s
Link: %s
Date: %s
Authors: %s
```

Artical content:
```txt
%s
```
"

                                ;; "Kindly provide me with a summary of the following article. The summary should cover the main points of the article and provide me with a clear understanding of the topic discussed. Please ensure that the summary is concise but comprehensive and includes all the essential information from the article. Please response in in multiline markdown format text."
                                ;; "Provide me key takeaways, tldrs and summary of the following article.
                                ;; Your response should cover the main points of the article and provide me with a clear understanding of the topic discussed.
                                ;; Ensure that your response is concise but comprehensive and includes all the essential information from the article."
                                "Generate TLDR for the following article.
Your response should cover the main points of the article and provide me with a clear understanding of the topic discussed.
Ensure that your response is concise but comprehensive and includes all the essential information from the article.
Use HTML <ol> bullet points in your response as much as possible."
                                ;; "Summarize following article, response with markdown."
                                feed-title title link nicedate niceauthors txt-content)))
      (+gpt-request gpt-message (lambda (msg)
                                  ;; (setq kkk msg)
                                  (when msg
                                    (with-temp-buffer
                                      (insert (concat "<article><title>Summary</title>"
                                                      (s-replace-all '(("\n" . "<br/>")) msg)
                                                      "<p>------</p><article/>"))
                                      (let ((dom (libxml-parse-html-region (point-min) (point-max))))
                                        (when (buffer-live-p buf)
                                          (with-current-buffer buf
                                            (goto-char (point-min))
                                            (read-only-mode -1)
                                            (shr-insert-document dom)
                                            ;; (insert (concat "====== Summary ======\n" msg "\n====== End Of Summary ======\n"))
                                            (read-only-mode 1)))))))
                    "You are a large language model living in Emacs and a helpful reading assistant.
Your response should be in HTML format.
You should separate your response into multiple paragraph if they are too long."))))

;;;###autoload
(defun +summarize-current-eww-buffer ()
  (interactive)
  (require 's)
  (when (eq major-mode 'eww-mode)
    (let* ((buf (current-buffer))
           (title (plist-get eww-data :title))
           (link (plist-get eww-data :url))
           (txt-content (with-current-buffer (current-buffer) (buffer-string)))
           (gpt-message (format "%s
Artical metadata:
```txt
Title: %s
Link: %s
```

Artical content:
```txt
%s
```
"

                                ;; "Kindly provide me with a summary of the following article. The summary should cover the main points of the article and provide me with a clear understanding of the topic discussed. Please ensure that the summary is concise but comprehensive and includes all the essential information from the article. Please response in in multiline markdown format text."
                                ;; "Provide me key takeaways, tldrs and summary of the following article.
                                ;; Your response should cover the main points of the article and provide me with a clear understanding of the topic discussed.
                                ;; Ensure that your response is concise but comprehensive and includes all the essential information from the article."
                                "Generate TLDR for the following article.
Your response should cover the main points of the article and provide me with a clear understanding of the topic discussed.
Ensure that your response is concise but comprehensive and includes all the essential information from the article.
Use HTML <ol> bullet points in your response as much as possible."
                                ;; "Summarize following article, response with markdown."
                                title link txt-content)))
      (+gpt-request gpt-message (lambda (msg)
                                  ;; (setq kkk msg)
                                  (when msg
                                    (with-temp-buffer
                                      (insert (concat "<article><title>Summary</title>"
                                                      (s-replace-all '(("\n" . "<br/>")) msg)
                                                      "<p>------</p><article/>"))
                                      (let ((dom (libxml-parse-html-region (point-min) (point-max))))
                                        (when (buffer-live-p buf)
                                          (with-current-buffer buf
                                            (goto-char (point-min))
                                            (read-only-mode -1)
                                            (shr-insert-document dom)
                                            ;; (insert (concat "====== Summary ======\n" msg "\n====== End Of Summary ======\n"))
                                            (read-only-mode 1)))))))
                    "You are a large language model living in Emacs and a helpful reading assistant.
Your response should be in HTML format.
You should separate your response into multiple paragraph if they are too long."))))

;;;###autoload
(defun +gpt-dwim-current-buffer ()
  (interactive)
  (cond
   ((eq major-mode 'eww-mode) (+summarize-current-eww-buffer))
   ((eq major-mode 'elfeed-show-mode) (+summarize-current-elfeed-show-buffer))))



;; (defun xxx ()
;;   (require 'llm-openai)
;;   (require 'llm)
;;   (let ((open-ai (make-llm-openai :key +open-ai-api-key :chat-model "gpt-3.5-turbo-16k" :embedding-model "text-embedding-ada-002"))))
;;   (with-current-buffer (get-buffer "*elfeed-entry*")
;;     (let (((make-llm-chat-prompt :context (buffer-substring))))
;;       (llm-chat-async open-ai))))
