;;; .nixpkgs/.doom.d/models.el -*- lexical-binding: t; -*-


(defconst gptel--openrouter-models
  '((openai/o3-mini-high
     :description "High intelligence at the same cost and latency targets of o1-mini"
     :context-window 200
     :input-cost 1.1
     :output-cost 4.4
     :cutoff-date "2023-10"
     :capabilities (reasoning)
     :request-params (:stream :json-false))
    (openai/o3-mini
     :description "High intelligence at the same cost and latency targets of o1-mini"
     :context-window 200
     :input-cost 1.1
     :output-cost 4.4
     :cutoff-date "2023-10"
     :capabilities (reasoning)
     :request-params (:stream :json-false))
    (openai/gpt-4o-search-preview
     :description "Advanced model for complex tasks; cheaper & faster than GPT-Turbo"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 2.5
     :output-cost 10
     :cutoff-date "2023-10")
    (openai/gpt-4o-mini-search-preview
     :description "Cheap model for fast tasks; cheaper & more capable than GPT-3.5 Turbo"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 0.15
     :output-cost 0.6
     :cutoff-date "2023-10")
    (openai/gpt-4o-mini
     :description "Cheap model for fast tasks; cheaper & more capable than GPT-3.5 Turbo"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 0.15
     :output-cost 0.6
     :cutoff-date "2023-10")
    (anthropic/claude-3.7-sonnet:beta
     :description "Hybrid model capable of standard thinking and extended thinking modes"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 3
     :output-cost 15
     :cutoff-date "2025-02")
    (anthropic/claude-3.7-sonnet:thinking
     :description "Hybrid model capable of standard thinking and extended thinking modes"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 3
     :output-cost 15
     :cutoff-date "2025-02")))

(defconst gptel--gh-models
  '((gpt-5-mini
     :description "Flagship model for complex tasks"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 2.0
     :output-cost 8.0
     :cutoff-date "2024-05")
    (gpt-4.1
     :description "Flagship model for complex tasks"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 2.0
     :output-cost 8.0
     :cutoff-date "2024-05")
    (gpt-5
     :description "Flagship model for coding, reasoning, and agentic tasks across domains"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 1.25
     :output-cost 10
     :cutoff-date "2024-09")
    (gpt-5-mini
     :description "Flagship model for coding, reasoning, and agentic tasks across domains"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 1.25
     :output-cost 10
     :cutoff-date "2024-09")
    (o3-mini
     :description "High intelligence at the same cost and latency targets of o1-mini"
     :capabilities (reasoning tool-use)
     :context-window 200
     :input-cost 3
     :output-cost 12
     :cutoff-date "2023-10")
    (o4-mini
     :description "Fast, effective reasoning with efficient performance in coding and visual tasks"
     :capabilities (reasoning media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 200
     :input-cost 1.1
     :output-cost 4.4
     :cutoff-date "2024-05")
    (claude-3.7-sonnet
     :description "Hybrid model capable of standard thinking and extended thinking modes"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 3
     :output-cost 15
     :cutoff-date "2025-02")
    (claude-3.7-sonnet-thought
     :description "Claude 3.7 Sonnet Thinking"
     :capabilities (media cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 3
     :output-cost 15
     :cutoff-date "2025-02")
    (claude-sonnet-4
     :description "High-performance model with exceptional reasoning and efficiency"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 3
     :output-cost 15
     :cutoff-date "2025-03")
    (claude-opus-4
     :description "Most capable model for complex reasoning and advanced coding"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 15
     :output-cost 75
     :cutoff-date "2025-03")
    (gemini-2.5-pro
     :description "Next gen, high speed, multimodal for a diverse variety of tasks"
     :capabilities (tool-use json media)
     :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
                  "application/pdf" "text/plain" "text/csv" "text/html")
     :context-window 1000
     :input-cost 0.1
     :output-cost 0.4
     :cutoff-date "2024-08")))

(defconst gptel--claude-code-models
  '((sonnet
     :description "High-performance model with exceptional reasoning and efficiency"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 3
     :output-cost 15
     :cutoff-date "2025-03")
    (opus
     :description "Most capable model for complex reasoning and advanced coding"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 15
     :output-cost 75
     :cutoff-date "2025-03")))

(defconst gptel--gh-copilot-business-models
  '((gpt-4.1-2025-04-14
     :description "Flagship model for complex tasks"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 2.0
     :output-cost 8.0
     :cutoff-date "2024-05")
    (claude-3.7-sonnet-thought
     :description "Claude 3.7 Sonnet Thinking"
     :capabilities (media cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 3
     :output-cost 15
     :cutoff-date "2025-02")
    (gpt-4o
     :description
     "Advanced model for complex tasks; cheaper & faster than GPT-Turbo"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128 :input-cost 2.5 :output-cost 10 :cutoff-date "2023-10")
    (gpt-4.1
     :description "Flagship model for complex tasks"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 2.0
     :output-cost 8.0
     :cutoff-date "2024-05")
    (o3-mini
     :description "High intelligence at the same cost and latency targets of o1-mini"
     :capabilities (reasoning tool-use)
     :context-window 200
     :input-cost 3
     :output-cost 12
     :cutoff-date "2023-10")
    (o3-mini-2025-01-03
     :description "High intelligence at the same cost and latency targets of o1-mini"
     :capabilities (reasoning tool-use)
     :context-window 200
     :input-cost 3
     :output-cost 12
     :cutoff-date "2023-10")
    (claude-3.5-sonnet
     :description "Highest level of intelligence and capability"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 3
     :output-cost 15
     :cutoff-date "2024-04")
    (claude-3.7-sonnet
     :description "Hybrid model capable of standard thinking and extended thinking modes"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 3
     :output-cost 15
     :cutoff-date "2025-02")
    (gemini-2.0-flash-001
     :description "Next gen, high speed, multimodal for a diverse variety of tasks"
     :capabilities (json media)
     :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
                  "application/pdf" "text/plain" "text/csv" "text/html")
     :context-window 1000
     :input-cost 0.10
     :output-cost 0.40
     :cutoff-date "2024-08")))
