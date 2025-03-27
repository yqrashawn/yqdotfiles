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
  '((o1
     :description "Reasoning model designed to solve hard problems across domains"
     :capabilities (media reasoning)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 200
     :input-cost 15
     :output-cost 60
     :cutoff-date "2023-10"
     :request-params (:stream :json-false))
    (o3-mini-paygo
     :description "High intelligence at the same cost and latency targets of o1-mini"
     :context-window 200
     :input-cost 1.1
     :output-cost 4.4
     :cutoff-date "2023-10"
     :capabilities (reasoning)
     :request-params (:stream :json-false))
    (o3-mini
     :description "High intelligence at the same cost and latency targets of o1-mini"
     :context-window 200
     :input-cost 1.1
     :output-cost 4.4
     :cutoff-date "2023-10"
     :capabilities (reasoning)
     :request-params (:stream :json-false))
    (gpt-4o-2024-11-20
     :description "Advanced model for complex tasks; cheaper & faster than GPT-Turbo"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 2.5
     :output-cost 10
     :cutoff-date "2023-10")
    (gpt-4o-mini
     :description "Cheap model for fast tasks; cheaper & more capable than GPT-3.5 Turbo"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 0.15
     :output-cost 0.6
     :cutoff-date "2023-10")
    (claude-3.7-sonnet
     :description "Hybrid model capable of standard thinking and extended thinking modes"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 3
     :output-cost 15
     :cutoff-date "2025-02")
    (claude-3.7-sonnet-thought
     :description "Hybrid model capable of standard thinking and extended thinking modes"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 3
     :output-cost 15
     :cutoff-date "2025-02")))
