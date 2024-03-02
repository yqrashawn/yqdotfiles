;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

(package! zoxide)
(package! mwim)
(package! emamux)
;; (package! company-flx)
;; (package! ccc)
(package! el-patch)
(package! git-link)
(package! copy-as-format)
(package! iflipb)
(package! rg)
(package! evil-iedit-state)
;; (package! evil-textobj-anyblock :disable t)
(unpin! evil-textobj-anyblock)
(package! evil-textobj-anyblock :recipe (:host github :repo "noctuid/evil-textobj-anyblock" :branch "master"))
(package! string-inflection)
(package! side-notes)
(package! explain-pause-mode :recipe (:type git :host github :repo "lastquestion/explain-pause-mode"))

;; (package! ivy :recipe (:host github :repo "abo-abo/swiper" :branch "master"
;;                        :files
;;                        (:defaults (:exclude "swiper.el" "counsel.el" "ivy-hydra.el") "doc/ivy-help.org")
;;                        :upstream (:host github :repo "abo-abo/swiper")))

;; (package! swiper :recipe (:host github :repo "abo-abo/swiper" :branch "master"
;;                           :files ("swiper.el")
;;                           :upstream (:host github :repo "abo-abo/swiper")))

;; (package! counsel :recipe (:host github :repo "abo-abo/swiper" :branch "master"
;;                            :files ("counsel.el")
;;                            :upstream (:host github :repo "abo-abo/swiper")))

;; (package! counsel-tramp)
;; (package! dired-filter)
(package! dired-quick-sort)
(package! loccur)
(package! double-saber)
(package! bicycle)
(package! backline)

(package! abridge-diff)

(package! jest)
(package! outline-minor-faces)
(package! magit-cz :recipe (:host github :repo "yqrashawn/magit-cz.el"))
(package! smeargle)
(package! eval-sexp-fu)
(package! lisp-extra-font-lock :recipe (:host github :repo "Lindydancer/lisp-extra-font-lock"))
(package! highlight-function-calls)
(package! easy-escape)
;; (package! company-tabnine)
;; (package! company-ctags)

;(package! jenkinsfile-mode)
(package! jq-mode)
(package! separedit)
(package! help-fns+)
(package! adoc-mode)
(package! nginx-mode)
;; (package! company-nginx)
(package! dotenv-mode)
(package! crontab-mode)

(package! modus-themes)

(package! reveal-in-osx-finder)

;; (package! company-manually)
(package! vlang-mode :recipe (:type git :host github :repo "Naheel-Azawy/vlang-mode"))
(package! ivy-dired-history)



(package! olivetti)
;; (package! orderless)
;; (package! corfu)
;; (package! cape)
(package! ix)
(package! orgbox)
;; (package! fancy-dabbrev)
;; (package! sis)
;; (package! affe)

(package! thing-edit :recipe (:type git :host github :repo "manateelazycat/thing-edit"))
;(package! dogears :recipe (:type git :host github :repo "alphapapa/dogears.el"))

(package! yaml-imenu)
;; (package! imenu-list)
;; (package! side-hustle)
;; (package! github-review)
(package! comb)
(package! wucuo)
;; (package! mini-frame)
(package! jsonnet-mode)
(package! apheleia :recipe (:type git :host github :repo "raxod502/apheleia"))
;; (package! vulpea)
(package! deadgrep)
(package! declutter :recipe (:type git :host github :repo "sanel/declutter"))
(package! turbo-log :recipe (:type git :host github :repo "Artawower/turbo-log"))

;; (package! urgrep :recipe (:host github :repo "jimporter/urgrep"))
(unpin!
  evil
  doom-snippets
  cider
  lispy
  ;; mu4e
  link-hint
  fd-dired
  ;; code-review
  clj-refactor
  clojure-mode
  flycheck-golangci-lint)

(package! elcord)
(package! parinfer-rust-mode)
(package! idle-highlight-mode)
(package! symex :recipe (:host github :repo "countvajhula/symex.el"))
;; (package! rigpa :recipe (:host github :repo "countvajhula/rigpa"))
(package! proced-narrow)
(package! capf-autosuggest)
(package! adoc-mode)
(package! sly-quicklisp)
(package! sly-asdf)
(package! string-edit-at-point)
(package! fit-text-scale)
;; (package! centered-cursor-mode)
;; (package! meow)
(package! embark)
(package! keycast)
(package! elfeed-protocol)
;; (package! tzc)
;; (package! clj-deps-new)
(package! isearch-mb)
(package! jsdoc :recipe (:type git :host github :repo "isamert/jsdoc.el"))
;; (package! dogears :recipe (:type git :host github :repo "alphapapa/dogears.el"))
;; (package! hl-prog-extra :recipe (:type git :host gitlab :repo "ideasman42/emacs-hl-prog-extra"))
(package! unmodified-buffer :recipe (:type git :host github :repo "arthurcgusmao/unmodified-buffer"))
(package! elfeed-dashboard)
(package! notmuch-transient)
(package! tray)
;; (package! company-tabnine-capf
;;   :recipe (:local-repo "./lisp/"
;;            :files ("company-tabnine-capf.el")
;;            ;; :build (:not compile)
;;            ))

(package! walkclj
  :recipe (:host github
           :repo "plexus/walkclj"
           :files ("walkclj.el")))

(package! clj-ns-name
  :recipe (:host github
           :repo "plexus/plexmacs"
            :files ("clj-ns-name/clj-ns-name.el")))

(package! pprint-to-buffer
  :recipe (:host github
           :files ("pprint-to-buffer/pprint-to-buffer.el")
           :repo "plexus/plexmacs"))

;; (package! corkey
;;   :recipe (:host github
;;            :branch "main"
;;            :files ("corkey/corkey.el"
;;                    "corkey/corgi-keys.el"
;;                    "corkey/corgi-signals.el"
;;                    "corkey/user-keys-template.el"
;;                    "corkey/user-signals-template.el")
;;            :repo "lambdaisland/corgi-packages"))

;; (package! elisp-slime-nav)

(unpin! helpful
  ;; company-mode
  )
(package! cider-eval-sexp-fu)
(package! org-modern)
;; carp
(package! carp-mode
  :recipe (:host github
           :repo "carp-lang/carp-emacs"
           :files ("carp-mode.el")))
(package! carp-flycheck
  :recipe (:host github
           :repo "carp-lang/carp-emacs"
           :files ("carp-flycheck.el")))
(package! inf-carp-mode
  :recipe (:host github
           :repo "carp-lang/carp-emacs"
           :files ("inf-carp-mode.el")))
(package! parseclj :recipe (:host github :repo "clojure-emacs/parseclj" :branch "main"))
(package! textsize)
(package! eshell-follow
  :recipe (:type git :repo "https://git.sr.ht/~e-v/eshell-follow.el"))
;; (package! plz :recipe (:host github :repo "alphapapa/plz.el")) ;; http library
;; (package! ement :recipe (:host github :repo "alphapapa/ement.el")) ;; matrix client
;; (package! esup)
(package! vundo :recipe (:host github :repo "casouri/vundo"))
;; (package! zoom) ;; balance window
(package! evil-matchit)
;; (package! evil-owl)
(package! gc-buffers
  :recipe (:type git :repo "https://codeberg.org/akib/emacs-gc-buffers.git"))
(package! detached)

;; (package! fuz-bin
;;   :recipe (:host github :repo "jcs-elpa/fuz-bin"))

(package! fuz
  :recipe (:host github :repo "rustify-emacs/fuz.el"))

;; (package! flx-rs :recipe (:host github :repo "jcs-elpa/flx-rs" :files (:defaults "bin")))
(package! fussy :recipe (:host github :repo "jojojames/fussy"))
(package! hotfuzz)

;; (disable-packages! orderless)

;; crash when open json file
;; (package! jsonian :recipe (:host github :repo "iwahbe/jsonian"))
(disable-packages! json-mode)
(package! magit-delta)
(package! todoist)
;; (package! relative-date :recipe (:host github :repo "rougier/relative-date"))
;; (package! fancy-compilation)
;; (package! cursory)
(package! ef-themes)
(package! protobuf-mode)
;; find all major/minor modes and there relations
(package! mode-minder :recipe (:host github :repo "jdtsmith/mode-minder"))
(package! clojure-mode-extra-font-locking)
(package! dwim-shell-command :recipe (:type git :host github :repo "xenodium/dwim-shell-command"))
(package! consult-tramp :recipe (:type git :host github :repo "Ladicle/consult-tramp"))
(package! topsy)
;; (package! evil-textobj-anyblock
;;   :recipe (:host github
;;            :repo "willghatch/evil-textobj-anyblock"
;;            :branch "fix-inner-block")
;;   :pin "29280cd71a05429364cdceef2ff595ae8afade4d")

(package! fzf-native :recipe (:host github :repo "dangduc/fzf-native" :files (:defaults "bin")))
(package! languagetool)
(disable-packages! langtool)
(package! consult-git-log-grep)

(package! eat)

;; (package! magit-libgit)
;; (package! emacsql-sqlite-builtin
;;   :recipe (:host github :repo "magit/emacsql" :files ("*.el"))
;;   :pin "415dbfd846f46d921a70a351695f0d0e8f75da35")
;; (package! sqlite3)
;; (package! emacsql-sqlite-module)

(disable-packages! company)

(package! treesit-auto)
(package! titlecase)
(package! denote)
(package! ht)

;; (package! chatgpt-arcana :recipe (:host github :repo "CarlQLange/ChatGPT-Arcana.el" :files ("*.el")))
;; (package! jit-spell)
(package! gptel :recipe (:host github :repo "karthink/gptel"))
(package! chatgpt-shell)

;; (package! closql :pin "0a7226331ff1f96142199915c0ac7940bac4afdd")
;; (package! emacsql :pin "415dbfd846f46d921a70a351695f0d0e8f75da35")
;; (package! emacsql-sqlite-builtin :pin "415dbfd846f46d921a70a351695f0d0e8f75da35")
(package! emacsql-sqlite-builtin)
(package! dts-mode)
(package! shrface)
;; (package! codeium :recipe (:host github :repo "Exafunction/codeium.el"))

;; (package! ollama
;;   :recipe (:host github :repo "zweifisch/ollama"))

(disable-packages! dap-mode treemacs lsp-treemacs)

(package! expreg)
(package! jarchive)
(package! pinentry)
(package! llm :recipe (:host github :repo "ahyatt/llm"))
;; (package! org-ai)
(unpin! nim-mode)
(package! whisper :recipe (:host github :repo "natrys/whisper.el"))
(package! memoize)
;; (package! kagi)
(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el" "dist")))
;; (package! devdocs)

(package! combobulate)

;; (when (package! lsp-bridge
;;         :recipe (:host github
;;                  :repo "manateelazycat/lsp-bridge"
;;                  :branch "master"
;;                  :files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
;;                  ;; do not perform byte compilation or native compilation for lsp-bridge
;;                  :build (:not compile)))
;;   (package! markdown-mode)
;;   (package! yasnippet))

;; (package! tabnine)

;; Local Variables:
;; eval: (apheleia-mode -1)
;; End:
