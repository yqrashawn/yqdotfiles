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
(package! company-flx)
(package! ccc)
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

(package! counsel-tramp)
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
(package! company-tabnine)
(package! company-ctags)

(package! tree-sitter)
(package! tree-sitter-langs)
(package! jenkinsfile-mode)
(package! jq-mode)
(package! separedit)
(package! help-fns+)
(package! adoc-mode)
(package! nginx-mode)
(package! company-nginx)
(package! dotenv-mode)
(package! crontab-mode)

(package! modus-themes)

(unpin! cider lispy)