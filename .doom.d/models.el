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
  `((haiku
     :description "Near-frontier intelligence at blazing speeds with extended thinking"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 1
     :output-cost 5
     :cutoff-date "2025-02")
    (sonnet-low
     :description "High-performance model with exceptional reasoning and efficiency (low effort)"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 3
     :output-cost 15
     :cutoff-date "2025-07"
     :request-params (:thinking (:type "enabled" :budget_tokens 1024)))
    (sonnet-medium
     :description "High-performance model with exceptional reasoning and efficiency (medium effort)"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 3
     :output-cost 15
     :cutoff-date "2025-07"
     :request-params (:thinking (:type "enabled" :budget_tokens 8000)))
    (sonnet-high
     :description "High-performance model with exceptional reasoning and efficiency (high effort)"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 3
     :output-cost 15
     :cutoff-date "2025-07"
     :request-params (:thinking (:type "enabled" :budget_tokens 16000)))
    (sonnet
     :description "High-performance model with exceptional reasoning and efficiency (max effort)"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 3
     :output-cost 15
     :cutoff-date "2025-07"
     :request-params (:thinking (:type "enabled" :budget_tokens 32000)))
    (,(intern "sonnet[1m]-low")
     :description "High-performance model with exceptional reasoning and efficiency, 1M context (low effort)"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 1000
     :input-cost 3
     :output-cost 15
     :cutoff-date "2025-07"
     :request-params (:thinking (:type "enabled" :budget_tokens 1024)))
    (,(intern "sonnet[1m]-medium")
     :description "High-performance model with exceptional reasoning and efficiency, 1M context (medium effort)"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 1000
     :input-cost 3
     :output-cost 15
     :cutoff-date "2025-07"
     :request-params (:thinking (:type "enabled" :budget_tokens 8000)))
    (,(intern "sonnet[1m]-high")
     :description "High-performance model with exceptional reasoning and efficiency, 1M context (high effort)"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 1000
     :input-cost 3
     :output-cost 15
     :cutoff-date "2025-07"
     :request-params (:thinking (:type "enabled" :budget_tokens 16000)))
    (,(intern "sonnet[1m]")
     :description "High-performance model with exceptional reasoning and efficiency, 1M context (max effort)"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 1000
     :input-cost 3
     :output-cost 15
     :cutoff-date "2025-07"
     :request-params (:thinking (:type "enabled" :budget_tokens 32000)))
    (opus-low
     :description "Most capable model for complex reasoning and advanced coding (low effort)"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 5
     :output-cost 25
     :cutoff-date "2025-03"
     :request-params (:thinking (:type "enabled" :budget_tokens 1024)))
    (opus-medium
     :description "Most capable model for complex reasoning and advanced coding (medium effort)"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 5
     :output-cost 25
     :cutoff-date "2025-03"
     :request-params (:thinking (:type "enabled" :budget_tokens 8000)))
    (opus-high
     :description "Most capable model for complex reasoning and advanced coding (high effort)"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 5
     :output-cost 25
     :cutoff-date "2025-03"
     :request-params (:thinking (:type "enabled" :budget_tokens 16000)))
    (opus
     :description "Most capable model for complex reasoning and advanced coding (max effort)"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 5
     :output-cost 25
     :cutoff-date "2025-03"
     :request-params (:thinking (:type "enabled" :budget_tokens 32000)))
    (,(intern "opus[1m]-low")
     :description "Most capable model for complex reasoning and advanced coding, 1M context (low effort)"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 1000
     :input-cost 5
     :output-cost 25
     :cutoff-date "2025-03"
     :request-params (:thinking (:type "enabled" :budget_tokens 1024)))
    (,(intern "opus[1m]-medium")
     :description "Most capable model for complex reasoning and advanced coding, 1M context (medium effort)"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 1000
     :input-cost 5
     :output-cost 25
     :cutoff-date "2025-03"
     :request-params (:thinking (:type "enabled" :budget_tokens 8000)))
    (,(intern "opus[1m]-high")
     :description "Most capable model for complex reasoning and advanced coding, 1M context (high effort)"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 1000
     :input-cost 5
     :output-cost 25
     :cutoff-date "2025-03"
     :request-params (:thinking (:type "enabled" :budget_tokens 16000)))
    (,(intern "opus[1m]")
     :description "Most capable model for complex reasoning and advanced coding, 1M context (max effort)"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 1000
     :input-cost 5
     :output-cost 25
     :cutoff-date "2025-03"
     :request-params (:thinking (:type "enabled" :budget_tokens 32000)))))
