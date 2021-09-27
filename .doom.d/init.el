;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a "Module Index" link where you'll find
;;      a comprehensive list of Doom's modules and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(defmacro define-obsolete-variable-alias (obsolete-name current-name &optional when docstring)
  "Make OBSOLETE-NAME a variable alias for CURRENT-NAME and mark it obsolete.

WHEN should be a string indicating when the variable was first
made obsolete, for example a date or a release number.

This macro evaluates all its parameters, and both OBSOLETE-NAME
and CURRENT-NAME should be symbols, so a typical usage would look like:

  (define-obsolete-variable-alias 'foo-thing 'bar-thing \"27.1\")

This macro uses `defvaralias' and `make-obsolete-variable' (which see).
See the Info node `(elisp)Variable Aliases' for more details.

If CURRENT-NAME is a defcustom or a defvar (more generally, any variable
where OBSOLETE-NAME may be set, e.g. in an init file, before the
alias is defined), then the define-obsolete-variable-alias
statement should be evaluated before the defcustom, if user
customizations are to be respected.  The simplest way to achieve
this is to place the alias statement before the defcustom (this
is not necessary for aliases that are autoloaded, or in files
dumped with Emacs).  This is so that any user customizations are
applied before the defcustom tries to initialize the
variable (this is due to the way `defvaralias' works).

For the benefit of Customize, if OBSOLETE-NAME has
any of the following properties, they are copied to
CURRENT-NAME, if it does not already have them:
`saved-value', `saved-variable-comment'."
  (declare (doc-string 4)
           (advertised-calling-convention
            (obsolete-name current-name when &optional docstring) "23.1"))
  `(progn
     (defvaralias ,obsolete-name ,current-name ,docstring)
     (dolist (prop '(saved-value saved-variable-comment))
       (and (get ,obsolete-name prop)
            (null (get ,current-name prop))
            (put ,current-name prop (get ,obsolete-name prop))))
     (make-obsolete-variable ,obsolete-name ,current-name ,when)))

(defmacro define-obsolete-face-alias (obsolete-face current-face &optional when)
  "Make OBSOLETE-FACE a face alias for CURRENT-FACE and mark it obsolete.
WHEN should be a string indicating when the face was first made
obsolete, for example a date or a release number."
  `(progn (put ,obsolete-face 'face-alias ,current-face)
          (put ,obsolete-face 'obsolete-face (or (purecopy ,when) t))))

(defmacro define-obsolete-function-alias (obsolete-name current-name &optional when docstring)
  "Set OBSOLETE-NAME's function definition to CURRENT-NAME and mark it obsolete.

\(define-obsolete-function-alias \\='old-fun \\='new-fun \"22.1\" \"old-fun's doc.\")

is equivalent to the following two lines of code:

\(defalias \\='old-fun \\='new-fun \"old-fun's doc.\")
\(make-obsolete \\='old-fun \\='new-fun \"22.1\")

WHEN should be a string indicating when the function was first
made obsolete, for example a date or a release number.

See the docstrings of `defalias' and `make-obsolete' for more details."
  (declare (doc-string 4))
  `(progn (defalias ,obsolete-name ,current-name ,docstring)
          (make-obsolete ,obsolete-name ,current-name ,when)))

(defadvice! doom--fix-wrong-number-of-args-during-byte-compile (recipe)
  :override #'straight--build-compile
  (let* ((package (plist-get recipe :package))
         (dir (straight--build-dir package))
         (program (concat invocation-directory invocation-name))
         (args
          `("-Q" "-L" ,dir
            ,@(apply #'append
                     (mapcar (lambda (d)
                               (let ((d (straight--build-dir d)))
                                 (when (file-exists-p d) (list "-L" d))))
                             (straight--get-dependencies package)))
            "--batch"
            "--eval"
            ,(prin1-to-string
              '(progn
                 (defmacro define-obsolete-face-alias (obsolete-face current-face &optional when)
                   `(progn (put ,obsolete-face 'face-alias ,current-face)
                           (put ,obsolete-face 'obsolete-face (or (purecopy ,when) t))))
                 (defmacro define-obsolete-function-alias (obsolete-name current-name &optional when docstring)
                   `(progn (defalias ,obsolete-name ,current-name ,docstring)
                           (make-obsolete ,obsolete-name ,current-name ,when)))
                 (defmacro define-obsolete-variable-alias (obsolete-name current-name &optional when docstring)
                   `(progn (defvaralias ,obsolete-name ,current-name ,docstring)
                           (dolist (prop '(saved-value saved-variable-comment))
                             (and (get ,obsolete-name prop)
                                  (null (get ,current-name prop))
                                  (put ,current-name prop (get ,obsolete-name prop))))
                           (make-obsolete-variable ,obsolete-name ,current-name ,when)))))
            "--eval"
            ,(format "(byte-recompile-directory %S 0 'force)" dir))))
    (when straight-byte-compilation-buffer
      (with-current-buffer (get-buffer-create straight-byte-compilation-buffer)
        (insert "\n$ " (replace-regexp-in-string
                        "\\(-L [^z-a]*? \\)"
                        "\\1\\\\ \n  "
                        (string-join `(,program ,@args) " "))
                "\n")))
    (apply #'call-process program nil straight-byte-compilation-buffer nil args)))

(doom! :input
       ;;chinese
       ;;japanese
       ;;layout            ; auie,ctsrnm is the superior home row

       :completion
       (company +childframe); the ultimate code completion backend
       ;;helm              ; the *other* search engine for love and life
       ;;ido               ; the other *other* search engine...
       ivy               ; a search engine for love and life
       ;; vertico           ; the search engine of the future

       :ui
       ;;deft              ; notational velocity for Emacs
       ;;doom              ; what makes DOOM look the way it does
       ;;doom-dashboard    ; a nifty splash screen for Emacs
       ;;doom-quit         ; DOOM quit-message prompts when you quit Emacs
       ;;(emoji +unicode)  ; 🙂
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       hydra
       indent-guides     ; highlighted indent columns
       ;;(ligatures +extra +pragmata-pro)         ; ligatures and symbols to make your code pretty again
       ;;minimap           ; show a map of the code on the side
       modeline          ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink cursor line after big motions
       ;;neotree           ; a project drawer, like NERDTree for vim
       ;;ophints           ; highlight the region an operation acts on
       popup               ; tame sudden yet inevitable temporary windows
       ;;tabs              ; a tab bar for Emacs
       ;;treemacs          ; a project drawer, like neotree but cooler
       ;;unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe
       ;;vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       ;;window-select     ; visually switch windows
       ;; workspaces        ; tab emulation, persistence & separate workspaces
       zen               ; distraction-free coding or writing

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       (format +onsave)  ; automated prettiness
       ;;god               ; run Emacs commands without modifier keys
       lispy             ; vim for lisp, for people who don't like vim
       multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;;parinfer          ; turn lisp into python, sort of
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       word-wrap         ; soft wrapping with language-aware indent

       :emacs
       dired             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       ibuffer         ; interactive buffer management
       undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       eshell            ; the elisp shell that works everywhere
       ;;shell             ; simple shell REPL for Emacs
       ;;term              ; basic terminal emulator for Emacs
       vterm               ; the best terminal emulation in Emacs

       :checkers
       (syntax +childframe); tasing you for every semicolon you forget
       (spell +flyspell +hunspell) ; tasing you for misspelling mispelling
       ;;grammar           ; tasing grammar mistake every you make

       :tools
       ;;ansible
       ;;debugger          ; FIXME stepping through code, to help you add bugs
       direnv
       docker
       editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)     ; run code, run (also, repls)
       gist              ; interacting with github gists
       (lookup +dectionary +docsets)              ; navigate your code and its documentation
       lsp
       (magit +forge)      ; a git porcelain for Emacs
       ;;make              ; run make tasks from Emacs
       ;;pass              ; password manager for nerds
       pdf               ; pdf enhancements
       ;;prodigy           ; FIXME managing external services & code builders
       rgb               ; creating color strings
       taskrunner        ; taskrunner for all your projects
       ;;terraform         ; infrastructure as code
       tmux              ; an API for interacting with tmux
       ;;upload            ; map local to remote projects via ssh/ftp

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS
       tty               ; improve the terminal Emacs experience

       :lang
       ;;agda              ; types of types of types of types...
       ;;beancount         ; mind the GAAP
       ;;cc                ; C > C++ == 1
       (clojure +lsp)           ; java with a lisp
       common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       data              ; config/data formats
       ;;(dart +flutter)   ; paint ui and not much else
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       ;;erlang            ; an elegant language for a more civilized age
       ;;ess               ; emacs speaks statistics
       ;;factor
       ;;faust             ; dsp, but you get to keep your soul
       ;;fsharp            ; ML stands for Microsoft's Language
       ;;fstar             ; (dependent) types and (monadic) effects and Z3
       ;;gdscript          ; the language you waited for
       ;;(go +lsp)         ; the hipster dialect
       ;;(haskell +dante)  ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ; a language you can depend on
       (json +lsp)              ; At least it ain't XML
       (java +lsp +meghanada) ; the poster child for carpal tunnel syndrome
       (javascript +lsp)        ; all(hope(abandon(ye(who(enter(here))))))
       ;;julia             ; a better, faster MATLAB
       ;;kotlin            ; a better, slicker Java(Script)
       ;;latex             ; writing papers in Emacs has never been so fun
       ;;lean              ; for folks with too much to prove
       ;;ledger            ; be audit you can be
       (lua +fennel +lsp)  ; one-based indices? one-based indices
       (markdown +grip)    ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       nix                 ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       (org +dragndrop +hugo +journal +noter +pandoc +pretty +roam2 +present) ; organize your plain life in plain text
       ;;php               ; perl's insecure younger brother
       plantuml            ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       python              ; beautiful is better than ugly
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       ;;raku              ; the artist formerly known as perl6
       rest                ; Emacs as a REST client
       ;;rst               ; ReST in peace
       (ruby +rails +rbenv +lsp); 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       (rust +lsp)              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       ;;(scheme +guile)   ; a fully conniving family of lisps
       (sh +lsp)                ; she sells {ba,z,fi}sh shells on the C xor
       ;;sml
       solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       (web +lsp)               ; the tubes
       (yaml +lsp)              ; JSON, but readable
       zig               ; C, but simpler

       :email
       (mu4e +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       ;;calendar
       ;;emms
       everywhere        ; *leave* Emacs!? You must be joking
       irc               ; how neckbeards socialize
       ;;(rss +org)        ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought

       :config
       ;;literate
       (default +bindings +smartparens))

;; needs to put this in init.el
;; https://github.com/hlissner/doom-emacs/issues/401
(setq evil-respect-visual-line-mode nil)
(setq straight-vc-git-default-clone-depth 'full)
(add-hook 'tty-setup-hook (lambda () (load! "./tty/config.el")))
