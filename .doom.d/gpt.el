;;; gpt.el -*- lexical-binding: t; -*-

(use-package! llm
  :defer t
  :init
  (setq! llm-warn-on-nonfree nil)
  :config
  (require 's)
  (defadvice! +llm-request-async (orig-fn url &rest args)
    :around #'llm-request-async
    (let ((new-url (if (s-starts-with? "https://api.openai.com" url)
                       (s-replace "https://api.openai.com" "https://openrouter.ai/api" url)
                     url)))
      (apply orig-fn (cl-pushnew new-url args)))))

(defvar +gpt-system-message "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")

(defun +llm-gpt-request (message cb &optional system-message use-16k-model)
  (require 'llm)
  (require 'llm-openai)
  (let* ((model (if (eq use-16k-model 1)
                    "gpt-4-1106-preview"
                  "gpt-3.5-turbo-1106"))
         ;; (provider (make-llm-openai
         ;;            :key +open-ai-api-key
         ;;            :chat-model model))
         (provider (make-llm-openai
                    :key +openrouter-api-key
                    :chat-model "openrouter/auto"))
         (first-message (make-llm-chat-prompt-interaction
                         :role 'user
                         :content message))
         (prompt (make-llm-chat-prompt
                  :context (or system-message +gpt-system-message)
                  :temperature 0.8
                  :interactions (list first-message))))
    (llm-chat-async provider
                    prompt
                    cb
                    (lambda (_err-sym err-msg) (message "OpenAI Error: %S" err-msg)))))

(defun +gpt-request (message cb &optional system-message use-16k-model)
  (require 'request)
  (require 'seq)
  (request "https://api.openai.com/v1/chat/completions"
    :type "POST"
    :parser 'json-read
    :encoding 'utf-8
    :headers `(("Content-Type" . "application/json")
               ("Authorization" . ,(concat "Bearer " +open-ai-api-key)))
    :data (json-encode
           `(,(if (eq use-16k-model 1)
                  '("model" . "gpt-4-1106-preview")
                '("model" . "gpt-3.5-turbo-1106"))
             ("temperature" . 0.8)
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
(defun +summarize-current-elfeed-show-buffer (arg)
  (interactive "P")
  (require 's)
  (when (eq major-mode 'elfeed-show-mode)
    (let* ((buf (current-buffer))
           (use-16k-model (eq arg 1))
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
      (+llm-gpt-request gpt-message (lambda (msg)
                                      ;; (setq kkk msg)
                                      (when msg
                                        (with-temp-buffer
                                          (insert (concat "<article><title>Summary</title>"
                                                          (thread-last msg
                                                                       (s-replace-all '(("\n" . "<br/>")))
                                                                       (s-chop-prefix "```html")
                                                                       (s-chop-suffix "```"))
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
You should separate your response into multiple paragraph if they are too long."
                        use-16k-model))))

;; TODO: split into multiple conversation if token exceeds limit

;;;###autoload
(defun +summarize-current-eww-buffer (arg)
  (interactive "P")
  (require 's)
  (when (eq major-mode 'eww-mode)
    (let* ((buf (current-buffer))
           (use-16k-model (eq arg 1))
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
      (+llm-gpt-request gpt-message (lambda (msg)
                                      ;; (setq kkk msg)
                                      (when msg
                                        (with-temp-buffer
                                          (insert (concat "<article><title>Summary</title>"
                                                          (thread-last msg
                                                                       (s-replace-all '(("\n" . "<br/>")))
                                                                       (s-chop-prefix "```html")
                                                                       (s-chop-suffix "```"))
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
You should separate your response into multiple paragraph if they are too long."
                        use-16k-model))))

;;;###autoload
(defun +gpt-dwim-current-buffer (arg)
  (interactive "P")
  (cond
   ((eq major-mode 'eww-mode) (call-interactively '+summarize-current-eww-buffer arg))
   ((eq major-mode 'elfeed-show-mode) (call-interactively '+summarize-current-elfeed-show-buffer arg))))



;; (defun xxx ()
;;   (require 'llm-openai)
;;   (require 'llm)
;;   (let ((open-ai (make-llm-openai :key +open-ai-api-key :chat-model "gpt-3.5-turbo-16k" :embedding-model "text-embedding-ada-002"))))
;;   (with-current-buffer (get-buffer "*elfeed-entry*")
;;     (let (((make-llm-chat-prompt :context (buffer-substring))))
;;       (llm-chat-async open-ai))))
