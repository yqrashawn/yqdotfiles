;;; lang/clojure.el -*- lexical-binding: t; -*-

(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.bb\\'" . clojure-mode))
(add-to-list 'magic-mode-alist '("^#![^\n]*/\\(clj\\|clojure\\|bb\\|lumo\\)" . clojure-mode))

(setq! +clojure-load-clj-refactor-with-lsp t)

(add-hook! (clojure-mode clojurescript-mode clojurec-mode)
  (setq-local evil-shift-width 1))

(setq-hook! '(cider-mode-hook) company-idle-delay 0.3)
(setq-hook! '(clojure-mode-hook) lsp-lens-enable nil)

;;; clojure-mode
(after! clojure-mode
  (setq! clojure-toplevel-inside-comment-form t
         clojure-verify-major-mode nil
         clojure-align-reader-conditionals t
         clojure-defun-indents '(fn-traced))
  ;; letsubs in status-mobile defview
  (pushnew! clojure-align-binding-forms "letsubs")
  ;; better-cond
  (pushnew! clojure-align-cond-forms "bc/cond" "b/cond")

  (require 'clojure-mode-extra-font-locking)
  (pushnew! clojure-built-in-vars "defview")
  (defun +re-add-clojure-mode-extra-font-locking ()
    (font-lock-add-keywords 'clojure-mode
                            `((,(concat "(\\(?:\.*/\\)?"
                                        (regexp-opt clojure-built-in-vars t)
                                        "\\>")
                               1 font-lock-builtin-face)))

    (font-lock-add-keywords 'clojure-mode
                            `((,(concat "\\<"
                                        (regexp-opt clojure-built-in-dynamic-vars t)
                                        "\\>")
                               0 font-lock-builtin-face))))
  (+re-add-clojure-mode-extra-font-locking)
  (setq-hook! '(clojure-mode-hook clojurec-mode-hook clojurescript-mode-hook)
    lsp-ui-sideline-show-code-actions nil
    lsp-ui-sideline-show-diagnostics nil)
  ;; (setq cider-clojure-cli-global-options "-T:portal-cli")
  )

(defun +setup-clojure-mode ()
  "sort namespace, cleanup log namespace on save"
  (add-hook! 'before-save-hook :local '+clojure-clean-log-ns 'clojure-sort-ns))

(add-hook! '(clojure-mode-hook clojurescript-mode-hook clojurec-mode-hook) '+setup-clojure-mode)

;;; lispy
(defun +in-babashka-p ()
  (and (memq major-mode '(clojure-mode))
       (functionp 'cider--babashka-version)
       (cider--babashka-version)))

(defun +in-clj-p ()
  (memq major-mode '(clojure-mode clojurec-mode clojurescript-mode)))

(use-package! lispy
  :defer t
  :diminish lispy " ʪ"
  :init
  (add-hook! +lispy-modes #'lispy-mode)
  :config
  ;; make lispy-eval works in babashka repl
  (defadvice! +lispy-eval (orig-fn &rest args)
    :around #'lispy-eval
    (if (+in-clj-p)
        (if (and lispy-mode (lispy-left-p))
            ;; eval on the right side
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

;;; cider
(defun +clojure-use-cider-over-lsp ()
  "use cider over clojure-lsp for completion when cider is not connected"
  (pushnew! completion-at-point-functions #'cider-complete-at-point)
  (setq-local cider-font-lock-dynamically '(macro core deprecated function var core))
  (setq-local lsp-completion-enable nil))
(defun +clojure-use-lsp-over-cider ()
  "use clojure-lsp over cider for completion when cider is not connected"
  (delq! #'cider-complete-at-point completion-at-point-functions)
  (setq-local cider-font-lock-dynamically nil)
  (setq-local lsp-completion-enable t))
(add-hook! cider-mode '+clojure-use-cider-over-lsp)
(add-hook! 'cider-disconnected-hook '+clojure-use-lsp-over-cider)
(add-hook! 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
(add-hook! 'cider-mode-hook #'cider-company-enable-fuzzy-completion)

(after! cider
  (setq!
   cider-repl-buffer-size-limit 1048576
   cider-repl-use-content-types t
   cider-repl-pop-to-buffer-on-connect nil
   cider-repl-display-output-before-window-boundaries t
   ;; cider-preferred-build-tool 'clojure-cli
   cider-default-cljs-repl 'shadow
   cider-auto-jump-to-error nil
   ;; cider-print-fn 'fipp
   cider-print-fn 'puget
   ;; cider-print-fn 'zprint
   ;; cider-print-options '(("style" ("community" "no-comma")))
   ;; cider-format-code-options '(("indents" (("letsubs" 0))))
   )

  ;; status h/deftest-sub
  (pushnew! cider-test-defining-forms "deftest-sub")

  (defadvice cider-find-var (before add-evil-jump activate)
    (evil-set-jump))


  (defadvice! corgi/around-cider-current-repl (command &optional type ensure)
    "When asking for a \"matching\" REPL (clj/cljs), and no matching REPL is found,
  return any REPL that is there. This is so that cider-quit can be called
  repeatedly to close all REPLs in a process. It also means that , s s will go
  to any REPL if there is one open."
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

  (defadvice! corgi/around-cider-repls (command &optional type ensure)
    "This essentially redefines cider-repls. The main thing it does is return all
  REPLs by using sesman-current-sessions (plural) instead of
  sesman-current-session. It also falls back to the babashka repl if no repls
  are connected/linked, so we can always eval."
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

  (defadvice! corgi/around-cider--choose-reusable-repl-buffer (command params)
    "Redefine cider--choose-reusable-repl-buffer to something more
sensible. If any dead REPL buffers exist when creating a new one
then simply delete them first. Return nil co `cider-creat-repl'
creates a new one. Don't unnecessarily bother the user."
    :around #'cider--choose-reusable-repl-buffer
    (seq-do #'kill-buffer
            (seq-filter (lambda (b)
                          (with-current-buffer b
                            (and (derived-mode-p 'cider-repl-mode)
                                 (not (process-live-p (get-buffer-process b))))))
                        (buffer-list)))
    nil)

  ;; (set-popup-rules!
  ;;   '(("^\\*cider-repl" :side right :size 0.5 :quit +doom/just-escaped-p :ttl nil)))

  (defadvice! +cider-jack-in-clj (orig-fn params)
    "Support babashka for cider-jack-in-clj"
    :around #'cider-jack-in-clj
    (interactive "P")
    (if (save-excursion
          (goto-char (point-min))
          (end-of-line)
          (re-search-forward "^#![^\n]*/bb" nil t))
        (funcall-interactively #'corgi/cider-jack-in-babashka (doom-project-root))
      (funcall-interactively orig-fn params)))

  (require 'cider-eval-sexp-fu)

  (defun +cider-test-execute-cljs ()
    "able to run `deftest' in cljs file, not support showing test result"
    (interactive)
    (let* ((ns (clojure-find-ns))
           (def (clojure-find-def)) ; it's a list of the form (deftest something)
           (deftype (car def)))
      (cider-interactive-eval
       (apply #'buffer-substring-no-properties (cider-defun-at-point 'bounds))
       (lambda (a)
         ;; (print a)
         (when (nrepl-dict-get a "value")
           (let* ((form (format "
(println \"-----run test------\")
(cljs.test/test-var %s)
(println \"---test finished---\")
"
                                (nrepl-dict-get a "value"))))
             ;; (message "---%s---" form)
             (if (and ns (member deftype cider-test-defining-forms))
                 (cider-interactive-eval form nil nil (cider--nrepl-pr-request-map))
               (message "No test at point"))))))))

  (defadvice! +cider-test-run-test (orig-fn)
    :around #'cider-test-run-test
    (if (eq major-mode 'clojurescript-mode)
        (call-interactively '+cider-test-execute-cljs)
      (call-interactively orig-fn))))

;; use ns rather than file name for clj buffer name
(use-package! clj-ns-name
  :after clojure-mode
  :config
  (clj-ns-name-install))

(after! clj-refactor
  ;; enter evil-insert-state for some cljr fn
  (defadvice! +cljr-add-require-to-ns (orig-fn &rest args)
    :around #'cljr-add-require-to-ns
    (interactive "P")
    (evil-insert-state 1)
    (call-interactively orig-fn))
  (defadvice! +cljr-add-import-to-ns (orig-fn &rest args)
    :around #'cljr-add-import-to-ns
    (interactive "P")
    (evil-insert-state 1)
    (call-interactively orig-fn))
  (defadvice! +cljr-add-use-to-ns (orig-fn &rest args)
    :around #'cljr-add-use-to-ns
    (interactive "P")
    (evil-insert-state 1)
    (call-interactively orig-fn))

  (after! cider
    ;; https://ag91.github.io/blog/2022/06/09/make-adding-a-clojure-require-more-interactive-with-cider-and-cljr/
    (defun +make-cljr-add-use-snippet-interactive ()
      (setq-local cljr--add-use-snippet "[${1:$$(yas-choose-value (ignore-errors (cider-sync-request:ns-list)))} :refer ${2:[$3]}]"))
    (add-hook! 'cider-mode-hook '+make-cljr-add-use-snippet-interactive)))
