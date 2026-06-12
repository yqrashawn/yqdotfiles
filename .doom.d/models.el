;;; .nixpkgs/.doom.d/models.el -*- lexical-binding: t; -*-


(defconst gptel--openrouter-models
  '((openai/gpt-5
     :description "Flagship model for coding, reasoning, and agentic tasks across domains"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 400
     :input-cost 1.25
     :output-cost 10
     :cutoff-date "2024-09")
    (openai/gpt-5-mini
     :description "Faster, more cost-efficient version of GPT-5"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 400
     :input-cost 0.25
     :output-cost 2.0
     :cutoff-date "2024-09")
    (openai/gpt-4o-mini
     :description "Cheap model for fast tasks; cheaper & more capable than GPT-3.5 Turbo"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 0.15
     :output-cost 0.6
     :cutoff-date "2023-10")
    (gemini-2.5-pro
     :description "Most powerful Gemini thinking model with state-of-the-art performance"
     :capabilities (tool-use json media audio video)
     :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
                  "application/pdf" "text/plain" "text/csv" "text/html"
                  "audio/mpeg" "audio/wav" "audio/ogg" "audio/flac" "audio/aac" "audio/mp3"
                  "video/mp4" "video/mpeg" "video/avi" "video/quicktime" "video/webm")
     :context-window 1048             ; 65536 output token limit
     :input-cost 1.25                 ; 2.50 for >200k tokens
     :output-cost 10.00               ; 15 for >200k tokens
     :cutoff-date "2025-01")
    (google/gemini-2.5-flash
     :description "Best in terms of price-performance, with well-rounded capabilities"
     :capabilities (tool-use json media audio video)
     :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
                  "application/pdf" "text/plain" "text/csv" "text/html"
                  "audio/mpeg" "audio/wav" "audio/ogg" "audio/flac" "audio/aac" "audio/mp3"
                  "video/mp4" "video/mpeg" "video/avi" "video/quicktime" "video/webm")
     :context-window 1048             ; 65536 output token limit
     :input-cost 0.3
     :output-cost 2.5
     :cutoff-date "2025-01")))

(defconst gptel--codex-models
  '((gpt-5-codex
     :description "Flagship model for coding, reasoning, and agentic tasks across domains"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 272
     :input-cost 1.25
     :output-cost 10
     :cutoff-date "2024-09")
    (gpt-5
     :description "Flagship model for coding, reasoning, and agentic tasks across domains"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 264
     :input-cost 1.25
     :output-cost 10
     :cutoff-date "2024-09")
    (codex-mini
     :description "Faster, more cost-efficient version of GPT-5"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 0.25
     :output-cost 2.0
     :cutoff-date "2024-09")))

(defconst gptel--gh-b-models
  '((gpt-4o
     :description "Advanced model for complex tasks; cheaper & faster than GPT-Turbo"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg"
                  "image/png"
                  "image/gif"
                  "image/webp")
     :context-window 128
     :input-cost 2.5
     :output-cost 10
     :cutoff-date "2024-11")
    (gpt-4.1
     :description "Flagship model for complex tasks"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 2.0
     :output-cost 8.0
     :cutoff-date "2025-04")
    (gpt-5-mini
     :description "Faster, more cost-efficient version of GPT-5"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 0.25
     :output-cost 2.0
     :cutoff-date "2024-09")
    (claude-sonnet-4
     :description "High-performance model with exceptional reasoning and efficiency"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/webp")
     :context-window 216
     :input-cost 3
     :output-cost 15
     :cutoff-date "2025-03")
    (claude-sonnet-4.5
     :description "High-performance model with exceptional reasoning and efficiency"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/webp")
     :context-window 144
     :input-cost 3
     :output-cost 15
     :cutoff-date "2025-07"
     :request-params (:parallel_tool_calls t))
    (claude-haiku-4.5
     :description "Near-frontier intelligence at blazing speeds with extended thinking"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/webp")
     :context-window 144
     :input-cost 1
     :output-cost 5
     :cutoff-date "2025-02")))

(defconst gptel--claude-code-models
  ;; Generated from (base x effort x channel) instead of hand-writing each
  ;; variant. Each base lists the effort tiers it supports; medium is emitted
  ;; both as the bare name (e.g. `opus') and as `<base>-medium' (the -medium
  ;; alias is kept so llm.el cchp presets keep resolving). Every variant also
  ;; gets a `:rc' release-channel twin. The `0g:*' models are appended
  ;; literally (1M context, custom names).
  (let* ((mime-types '("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf"))
         (effort-budgets '((low . 1024) (medium . 8000) (high . 16000) (xhigh . 24000) (max . 32000)))
         ;; (base (effort...) desc-prefix input-cost output-cost cutoff-date)
         (bases
          '((haiku (low medium high)
             "Near-frontier intelligence at blazing speeds with extended thinking" 1 5 "2025-02")
            (sonnet (low medium high max)
             "High-performance model with exceptional reasoning and efficiency" 3 15 "2025-07")
            (opus (low medium high xhigh max)
             "Most capable model for complex reasoning and advanced coding" 5 25 "2025-03")
            (fable (low medium high xhigh max)
             "Most capable model for complex reasoning and advanced coding" 5 25 "2025-03")))
         (make-entry
          (lambda (name desc ctx in out cutoff budget)
            (list (intern name)
                  :description desc
                  :capabilities '(media tool-use cache)
                  :mime-types mime-types
                  :context-window ctx
                  :input-cost in
                  :output-cost out
                  :cutoff-date cutoff
                  :request-params (list :thinking (list :type "enabled" :budget_tokens budget)))))
         (normal '())
         (rc '()))
    (pcase-dolist (`(,base ,efforts ,desc-prefix ,in ,out ,cutoff) bases)
      (dolist (effort efforts)
        (let* ((budget (alist-get effort effort-budgets))
               (desc (format "%s (%s effort)" desc-prefix effort))
               ;; medium = bare name + `<base>-medium' alias; others = `<base>-<effort>'
               (stems (if (eq effort 'medium)
                          (list (symbol-name base) (format "%s-medium" base))
                        (list (format "%s-%s" base effort)))))
          (dolist (stem stems)
            (push (funcall make-entry stem desc 200 in out cutoff budget) normal)
            (push (funcall make-entry (concat stem ":rc") desc 200 in out cutoff budget) rc)))))
    (append
     (nreverse normal)
     (nreverse rc)
     (mapcar
      (lambda (name)
        (funcall make-entry
                 name
                 "Most capable model for complex reasoning and advanced coding, 1M context (max effort)"
                 1000 5 25 "2025-03" 32000))
      '("0g:glm-5" "0g:glm-5.1" "0g:deepseek-v4-flash"
        "0g:deepseek-v4-pro" "0g:qwen3.7-max")))))
