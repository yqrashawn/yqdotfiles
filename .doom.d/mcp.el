;;; .nixpkgs/.doom.d/mcp.el -*- lexical-binding: t; -*-

(defun +gen-mcp-json-conf ()
  "Generate MCP JSON config from `mcp-hub-servers'.

Writes the config to ~/Downloads/mcp.json and replaces \"mcpServers\" in ~/.claude.json."
  (interactive)
  (let* ((output-file (expand-file-name "~/Downloads/mcp.json"))
         (claude-json-file (expand-file-name "~/.claude.json"))
         (servers-plist
          (cl-loop for (name . cfg) in mcp-hub-servers
                   append
                   (list
                    (intern name)  ; Convert string name to symbol for plist key
                    (let* ((command (plist-get cfg :command))
                           (args (plist-get cfg :args))
                           (url (plist-get cfg :url))
                           (type (plist-get cfg :type))
                           (env (plist-get cfg :env))
                           (env-plist
                            (when env
                              (cond
                               ((and (listp env) (keywordp (car env)))
                                (cl-loop for (k v) on env by #'cddr
                                         when v
                                         append (list (intern (substring (symbol-name k) 1)) v)))
                               ((and (listp env) (consp (car env)))
                                (cl-loop for (k . v) in env
                                         append (list (intern (if (keywordp k)
                                                                  (substring (symbol-name k) 1)
                                                                (format "%s" k)))
                                                      v)))
                               (t nil))))
                           (server-plist nil))
                      (when command
                        (setq server-plist (plist-put server-plist :command command)))
                      (when args
                        (setq server-plist
                              (plist-put server-plist :args
                                         (cond
                                          ((vectorp args) args)
                                          ((listp args) (vconcat args))
                                          (t args)))))
                      (when url
                        (setq server-plist (plist-put server-plist :url url))
                        (setq server-plist (plist-put server-plist :type "http")))
                      (when type
                        (setq server-plist (plist-put server-plist :type type)))
                      (when env-plist
                        (setq server-plist (plist-put server-plist :env env-plist)))
                      server-plist)))))
    (make-directory (file-name-directory output-file) t)
    (with-temp-file output-file
      (insert (json-serialize (list :mcpServers servers-plist)
                              :null-object :null
                              :false-object :json-false)))
    (let ((claude-conf
           (if (file-exists-p claude-json-file)
               (with-temp-buffer
                 (insert-file-contents claude-json-file)
                 (goto-char (point-min))
                 (json-parse-buffer
                  :object-type 'plist
                  :null-object :null
                  :false-object :json-false))
             (list :mcpServers nil))))
      (plist-put claude-conf :mcpServers servers-plist)
      (with-temp-file claude-json-file
        (insert (json-serialize claude-conf
                                :null-object :null
                                :false-object :json-false))
        (json-pretty-print-buffer))
      (message "Wrote MCP config to %s and updated %s"
               output-file claude-json-file))))

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

     ;; ("exa" .
     ;;  (:command "bun"
     ;;   :args ("run"
     ;;          ,(-> "~/Dropbox/sync/exa-mcp-server/.smithery/stdio/index.cjs"
     ;;               file-truename))))

     ("exa" .
      (:url "http://localhost:18681/mcp"))

     ;; ("perplexity" .
     ;;  (:command "npx"
     ;;   :args ("-y" "perplexity-mcp")
     ;;   :env
     ;;   (:PERPLEXITY_API_KEY ,+perplexity-api-key
     ;;    :PERPLEXITY_TIMEOUT_MS "600000")))
     
     ("jina_search" .
      (:url "http://localhost:18682/v1"))

     ;; ("jina_search" .
     ;;  (:command "npx"
     ;;   :args
     ;;   ("-y"
     ;;    "mcp-remote"
     ;;    "http://localhost:18682/mcp")))

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


     ;; ("clojure-mcp" .
     ;;  (:command "clojure"
     ;;   :args
     ;;   ("-Sdeps"
     ;;    "'{:deps {io.github.bhauman/clojure-mcp {:git/tag \"v0.3.1\" :git/sha \"81005ce\" } io.modelcontextprotocol.sdk/mcp {:mvn/version \"0.17.2\"}}'"
     ;;    "-X"
     ;;    "clojure-mcp.main/start")))

     ;; ("clojure-mcp" .
     ;;  (:command "clojure"
     ;;   :args
     ;;   ("-Tmcp"
     ;;    "start")))

     ("lattice" .
      (:command "bunx"
       :args ("-p"
              "matryoshka-rlm@latest"
              "lattice-mcp"
              "--dangerously-skip-cwd-checking")))

     ;; ("cclsp" .
     ;;  (:command "bunx"
     ;;   :args ("cclsp@latest")
     ;;   :env
     ;;   (:CCLSP_CONFIG_PATH
     ;;    ,(expand-file-name "~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/cclsp.json"))))

     ;; ("emacs" .
     ;;  (:command ,(concat
     ;;              (expand-file-name user-emacs-directory)
     ;;              "emacs-mcp-stdio.sh")))
     
     ;; "/Users/yqrashawn/.emacs.d/.local/cache/emacs-mcp-stdio.sh"
     
     ;; ("qmd" .
     ;;  (:command "bunx"
     ;;   :args ("@tobilu/qmd" "mcp")))
     
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

     ("scrapling" .
      (:url "http://localhost:17982/mcp"))

     ("cchp" .
      (:url "http://localhost:8035/mcp"))

     ;; ("slack" .
     ;;  (:url "http://localhost:13080/mcp"))
     

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
     ;;  (:url "http://mini.local:9897/api/mcp/sse"))

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

(use-package! mcp-server-lib
  :defer t
  :init
  (setq! mcp-server-lib-http-port 18684
         mcp-server-lib-async-timeout 3600)
  (unless (file-exists-p
           (concat (expand-file-name user-emacs-directory)
                   "emacs-mcp-stdio.sh"))
    (mcp-server-lib-install))
  (setq! mcp-server-lib-default-directory-function
         (defun +mcp-server-lib-default-directory-function (session-id)
           (or
            mcp-server-lib--request-cwd
            (when (fboundp 'gptel-claude-code--mcp-default-directory)
              (gptel-claude-code--mcp-default-directory session-id))
            (when (fboundp '++workspace-current-project-root)
              (++workspace-current-project-root)))))
  (load! "gptel-tools.el")
  (+gptel-reinit)

  ;; (mcp-server-lib-http-stop)
  (comment
    (mcp-server-lib-stop))
  (unless (and (boundp 'mcp-server-lib--running) mcp-server-lib--running)
    (mcp-server-lib-start)))

(use-package! mcp
  :after mcp-server-lib
  :config
  (require 'gptel-integrations)
  (require 'mcp-hub)

  (defadvice! +mcp-make-text-tool (orig-fn name tool-name &optional asyncp)
    :around #'mcp-make-text-tool
    (plist-put (funcall orig-fn name tool-name asyncp) :include t)))
