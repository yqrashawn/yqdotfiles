;;; lang/clojure.el -*- lexical-binding: t; -*-


(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.bb\\'" . clojure-mode))
(add-to-list 'magic-mode-alist '("^#!/.*\\(boot\\|clj\\|clojure\\|bb\\|lumo\\)$" . clojure-mode))

(after! clojure-mode
  (setq! clojure-toplevel-inside-comment-form t
         clojure-align-reader-conditionals t
         clojure-defun-indents '(fn-traced)
         clojure-verify-major-mode nil)
  ;; #_ is not a logical sexp
  (defadvice! corgi/clojure--looking-at-non-logical-sexp (command)
    :around #'clojure--looking-at-non-logical-sexp
    "Return non-nil if text after point is \"non-logical\" sexp.
\"Non-logical\" sexp are ^metadata and #reader.macros."
    (comment-normalize-vars)
    (comment-forward (point-max))
    (looking-at-p "\\(?:#?\\^\\)\\|#:?:?[[:alpha:]]\\|#_"))

  (setq-hook! '(clojure-mode-hook clojurec-mode-hook clojurescript-mode-hook)
    lsp-ui-sideline-show-code-actions nil
    lsp-ui-sideline-show-diagnostics nil))

(defun +in-babashka-p ()
  (and (memq major-mode '(clojure-mode))
       (functionp 'cider--babashka-version)
       (cider--babashka-version)))
(defun +in-clj-p ()
  (memq major-mode '(clojure-mode clojurec-mode clojurescript-mode)))


(use-package! lispy
  :diminish lispy " ʪ"
  :hook ((ielm-mode
          lisp-mode
          clojure-mode
          clojurec-mode
          scheme-mode
          racket-mode
          hy-mode
          lfe-mode
          dune-mode
          fennel-mode
          emacs-lisp-mode
          cider-repl-mode
          clojurescript-mode) . lispy-mode)
  :config
  ;; make lispy-eval works in babashka repl
    (defadvice! +lispy-eval (orig-fn &rest args)
      :around #'lispy-eval
      (if (+in-clj-p)
          (if (and lispy-mode (lispy-left-p))
              (save-excursion
                (call-interactively 'lispy-different)
                (call-interactively 'cider-eval-last-sexp))
            (call-interactively 'cider-eval-last-sexp))
        (apply orig-fn args)))

    (defadvice! +lispy-eval-and-insert (func &rest args)
      :around #'lispy-eval-and-insert
      (if (+in-clj-p)
          (progn
            (setq current-prefix-arg '(1))
            (call-interactively 'cider-pprint-eval-last-sexp))
        (apply func args))))

(after! cider
  (setq! cider-default-cljs-repl 'shadow)
  (setq! cider-auto-jump-to-error nil)
  ;; When asking for a "matching" REPL (clj/cljs), and no matching REPL is found,
  ;; return any REPL that is there. This is so that cider-quit can be called
  ;; repeatedly to close all REPLs in a process. It also means that , s s will go
  ;; to any REPL if there is one open.
  (defadvice! corgi/around-cider-current-repl (command &optional type ensure)
    :around #'cider-current-repl
    (let ((repl (or
                 (if (not type)
                     (or (funcall command nil)
                         (funcall command 'any))
                   (funcall command type))
                 (get-buffer "*babashka-repl*"))))
      (if (and ensure (null repl))
          (cider--no-repls-user-error type)
        repl)))

  ;; This essentially redefines cider-repls. The main thing it does is return all
  ;; REPLs by using sesman-current-sessions (plural) instead of
  ;; sesman-current-session. It also falls back to the babashka repl if no repls
  ;; are connected/linked, so we can always eval.
  (defadvice! corgi/around-cider-repls (command &optional type ensure)
    :around #'cider-repls
    (let ((type (cond
                 ((listp type)
                  (mapcar #'cider-maybe-intern type))
                 ((cider-maybe-intern type))))
          (repls (delete-dups (seq-mapcat #'cdr (or (sesman-current-sessions 'CIDER)
                                                    (when ensure
                                                      (user-error "No linked %s sessions" system)))))))
      (or (seq-filter (lambda (b)
                        (and (cider--match-repl-type type b)
                             (not (equal b (get-buffer "*babashka-repl*")))))
                      repls)
          (list (get-buffer "*babashka-repl*")))))

  ;; (set-popup-rules!
  ;;   '(("^\\*cider-repl" :side right :size 0.5 :quit +doom/just-escaped-p :ttl nil)))
  )

(add-hook! (clojure-mode clojurescript-mode clojurec-mode)
  (cmd! (setq-local evil-shift-width 1)))
