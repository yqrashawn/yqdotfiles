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

(defconst gptel--claude-code-models
  '((sonnet
     :description "High-performance model with exceptional reasoning and efficiency"
     :capabilities (tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 3
     :output-cost 15
     :cutoff-date "2025-03")
    (opus
     :description "Most capable model for complex reasoning and advanced coding"
     :capabilities (tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 15
     :output-cost 75
     :cutoff-date "2025-03")))

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
