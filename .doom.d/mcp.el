;;; .nixpkgs/.doom.d/mcp.el -*- lexical-binding: t; -*-

(progn
  (setq!
   mcp-hub-servers
   `(
     ;; ("palywright" .
     ;;   (:command "npx"
     ;;     :args
     ;;     ("@playwright/mcp@latest"
     ;;       "--storage-state=/Users/yqrashawn/.cache/playwrite-mcp/storage.json")))

     ("context7" .
      (:command "bunx"
       :args
       ("@upstash/context7-mcp"
        "--api-key"
        ,+context-7-api-key)))

     ;; ("nextjs-devtools" .
     ;;  (:command "npx"
     ;;   :args
     ;;   ("-y" "next-devtools-mcp@latest")))

     ("exa" .
      (:command "bun"
       :args ("run"
              ,(-> "~/Dropbox/sync/exa-mcp-server/.smithery/stdio/index.cjs"
                   file-truename))))

     ;; ("perplexity" .
     ;;  (:command "npx"
     ;;   :args ("-y" "perplexity-mcp")
     ;;   :env
     ;;   (:PERPLEXITY_API_KEY ,+perplexity-api-key
     ;;    :PERPLEXITY_TIMEOUT_MS "600000")))

     ;; ("jina_search" .
     ;;  (:url "http://localhost:18682/mcp"))

     ("jina_search" .
      (:command "npx"
       :args
       ("-y"
        "mcp-remote"
        "http://localhost:18682/mcp")))

     ;; ("github" .
     ;;  (:command "npx"
     ;;   :args
     ;;   ("-y"
     ;;    "mcp-remote"
     ;;    "https://api.githubcopilot.com/mcp"
     ;;    "--header"
     ;;    ,(format! "Authorization:Bearer %s" +gh-mcp-server-token))))

     ;; ("github" .
     ;;  (:command "gh"
     ;;   ;; :env ("GITHUB_DYNAMIC_TOOLSETS" "1")
     ;;   :args
     ;;   ("mcp")))
     
     ;; ("shadcn" .
     ;;  (:command "bunx"
     ;;   :args
     ;;   ("shadcn@latest"
     ;;    "mcp")))

     ;; ("clojure-mcp" .
     ;;  (:command "clojure"
     ;;   :args
     ;;   ("-Sdeps"
     ;;    "{:deps {io.github.bhauman/clojure-mcp {:local/root \"/Users/yqrashawn/workspace/third/clojure-mcp\"}}}"
     ;;    "-X"
     ;;    "clojure-mcp.main/start"
     ;;    ":config-profile"
     ;;    ":cli-assist")))


     ("clojure-mcp" .
      (:command "clojure"
       :args
       ("-Sdeps"
        "{:deps {io.github.bhauman/clojure-mcp {:git/tag \"v0.2.6\" :git/sha \"cc68ad0\"}}}"
        "-X"
        "clojure-mcp.main/start"
        ":config-profile"
        ":cli-assist")))

     ("lattice" .
      (:command "bunx"
       :args ("-p" "matryoshka-rlm" "lattice-mcp")))

     ;; ("emacs" .
     ;;  (:command ,(concat
     ;;              (expand-file-name user-emacs-directory)
     ;;              "emacs-mcp-stdio.sh")))
     
     ;; "/Users/yqrashawn/.emacs.d/.local/cache/emacs-mcp-stdio.sh"
     
     ("qmd" .
      (:command "bunx"
       :args ("@tobilu/qmd" "mcp")))
     
     ;; ("daisyui-blueprint" .
     ;;   (:command "npx"
     ;;     :args ("-y" "daisyui-blueprint@latest")
     ;;     :env
     ;;     (:LICENSE ,+daisyui-license
     ;;       :EMAIL ,+daisyui-email)))

     ;; ("flyonui" .
     ;;  (:command "npx"
     ;;   :args ("-y" "flyonui-mcp")))

     ;; ("0g-code" .
     ;;  (:command "node"
     ;;   :args
     ;;   (,(expand-file-name "~/workspace/office/webai-mcp-server/dist/index.js"))
     ;;   :env
     ;;   (:ZEROG_NETWORK "testnet",
     ;;    :ZEROG_PRIVATE_KEY ,+zerog-ai-pk)))
     
     ("emacs" .
      (:url "http://localhost:18684/mcp/v1/messages"))
     

     ;; ("desktop-commander" . (:command "bunx"
     ;;                         :args ("@wonderwhy-er/desktop-commander")))
     ;; ("clojure-mcp-miniser" . (:command "clojure-mcp"
     ;;                           :args ("--port" "8002")))
     
     ;; ("fetch" .
     ;;   (:command "uvx" :args ("mcp-server-fetch")))
     
     ;; ("dash-api" .
     ;;  (:command "uvx"
     ;;   :args
     ;;   ("--from"
     ;;    "git+https://github.com/Kapeli/dash-mcp-server.git"
     ;;    "dash-mcp-server")))

     ;; ("ramcp-macmini" .
     ;;  (:url "http://macmini.local:9897/api/mcp/sse"))

     ;; ("ramcp-local" .
     ;;  (:url "http://localhost:7999/api/mcp/sse"))

     ;; ("mcp-filesystem-server" .
     ;;  (:command "mcp-filesystem-server"
     ;;   :args ("/Users/yqrashawn/")))

     ;; ("markitdown" .
     ;;   (:command "uvx"
     ;;     :args ("markitdown-mcp")))

     ;; ("figma" .
     ;;   (:command "npx"
     ;;     :args ("-y"
     ;;             "figma-developer-mcp"
     ;;             ,(concat "--figma-api-key=" +figma-access-token)
     ;;             "--stdio")))

     ;; ("core-memory" .
     ;;  (:url "https://mcp.getcore.me/api/v1/mcp?source=Claude-Code"))
     ))
  (+gen-mcp-json-conf))
