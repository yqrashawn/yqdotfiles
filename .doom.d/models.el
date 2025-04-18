;;; ../.nixpkgs/.doom.d/models.el -*- lexical-binding: t; -*-


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

(defconst gptel--gh-copilot-models
  '((gpt-4o
     :description "Advanced model for complex tasks; cheaper & faster than GPT-Turbo"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 2.5
     :output-cost 10
     :cutoff-date "2024-11-20")
    (o1
     :description "Reasoning model designed to solve hard problems across domains"
     :capabilities (reasoning tool-use)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 200
     :input-cost 15
     :output-cost 60
     :cutoff-date "2024-12"
     :request-params (:stream :json-false))
    (o3-mini
     :description "High intelligence at the same cost and latency targets of o1-mini"
     :context-window 200
     :input-cost 1.1
     :output-cost 4.4
     :cutoff-date "2025-01-31"
     :capabilities (reasoning tool-use json)
     :request-params (:stream :json-false))
    (claude-3.5-sonnet
     :description "Hybrid model capable of standard thinking and extended thinking modes"
     :capabilities (media tool-use json url cache)
     :mime-types ("image/jpeg" "image/png" "image/webp")
     :context-window 200
     :input-cost 3
     :output-cost 15
     :cutoff-date "2025-02")
    (claude-3.7-sonnet
     :description "Hybrid model capable of standard thinking and extended thinking modes"
     :capabilities (media tool-use ;; json
                          url cache)
     :mime-types ("image/jpeg" "image/png" "image/webp")
     :context-window 200
     :input-cost 3
     :output-cost 15
     :cutoff-date "2025-02")
    (claude-3.7-sonnet-thought
     :description "Hybrid model capable of standard thinking and extended thinking modes"
     :capabilities (media cache)
     :mime-types ("image/jpeg" "image/png" "image/webp")
     :context-window 200
     :input-cost 3
     :output-cost 15
     :cutoff-date "2025-02")
    (gemini-2.0-flash-001
     :description "Hybrid model capable of standard thinking and extended thinking modes"
     :capabilities (media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "application/pdf" "image/heic" "image/heif")
     :context-window 200
     :input-cost 3
     :output-cost 15
     :cutoff-date "2025-02")
    (gemini-2.5-pro
     :description "Hybrid model capable of standard thinking and extended thinking modes"
     :capabilities (tool-use media json)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 3
     :output-cost 15
     :cutoff-date "2025-02")
    (o4-mini
     :description "Cheap model for fast tasks; cheaper & more capable than GPT-3.5 Turbo"
     :capabilities (tool-use json url)
     :context-window 128
     :input-cost 0.15
     :output-cost 0.6
     :cutoff-date "2024-07-18")
    (gpt-4.1
     :description "Cheap model for fast tasks; cheaper & more capable than GPT-3.5 Turbo"
     :capabilities (tool-use json url)
     :context-window 128
     :input-cost 0.15
     :output-cost 0.6
     :cutoff-date "2024-07-18")))
