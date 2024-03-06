;;; helper.el -*- lexical-binding: t; -*-


(defalias 'prn #'print)

(setq! +lispy-modes '(cider-repl-mode
                      clojure-mode
                      clojurec-mode
                      clojurescript-mode
                      clojurex-mode
                      common-lisp-mode
                      emacs-lisp-mode
                      eshell-mode
                      fennel-mode
                      fennel-repl-mode
                      geiser-repl-mode
                      gerbil-mode
                      inf-clojure-mode
                      inferior-emacs-lisp-mode
                      inferior-lisp-mode
                      inferior-scheme-mode
                      lisp-interaction-mode
                      lisp-mode
                      monroe-mode
                      racket-mode
                      racket-repl-mode
                      scheme-interaction-mode
                      scheme-mode
                      slime-repl-mode
                      sly-mrepl-mode
                      stumpwm-mode
                      ielm-mode
                      hy-mode
                      fennel-mode
                      dune-mode
                      lfe-mode))

(defun +xcrun-devices ()
  (apply #'a-merge (a-vals (a-get (json-parse-string (shell-command-to-string "xcrun simctl list devices -j")) "devices"))))

(defun +xcrun-device (device-name)
  (seq-find (lambda (v) (string= device-name (log/spy (a-get v "name")))) (+xcrun-devices)))
