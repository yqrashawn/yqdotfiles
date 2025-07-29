;;; lang/clojure.el -*- lexical-binding: t; -*-

(set-lookup-handlers! '(cider-mode cider-repl-mode)
  :definition #'+clojure-cider-lookup-definition
  :documentation #'cider-doc)

(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.bb\\'" . clojure-mode))
(add-to-list 'magic-mode-alist '("^#![^\n]*/\\(clj\\|clojure\\|bb\\|lumo\\)" . clojure-mode))

(setq! +clojure-load-clj-refactor-with-lsp t)

;;; clojure-mode
(after! clojure-mode
  (setq! clojure-toplevel-inside-comment-form t
         clojure-max-backtracking 10
         clojure-verify-major-mode nil
         clojure-align-reader-conditionals t
         clojure-defun-indents '(fn-traced))

  (setq-hook!
      '(clojure-mode-hook clojurec-mode-hook clojurescript-mode-hook)
    evil-shift-width 1
    lsp-lens-enable nil
    lsp-ui-sideline-show-code-actions nil
    lsp-ui-sideline-show-diagnostics nil)
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
  (+re-add-clojure-mode-extra-font-locking))

;;; lispy
(defun +in-babashka-p ()
  (and (memq major-mode '(clojure-mode))
       (functionp 'cider--babashka-version)
       (cider--babashka-version)))

(defun +in-clj-p ()
  (memq major-mode +clojure-modes))

(defun +cider-repl-dir ()
  (when (and (bound-and-true-p cider-mode) (cider-connected-p))
    (with-current-buffer (cider-current-repl)
      (expand-file-name default-directory))))

(defun +project-cider-repl-connected ()
  (let ((repl-dir (+cider-repl-dir)))
    (if (and repl-dir (string= (doom-project-root repl-dir) (doom-project-root)))
        t
      nil)))

(use-package! lispy
  :defer t
  :diminish lispy " ʪ"
  :init
  (add-hook! +lispy-modes #'lispy-mode)
  :config
  (defun +cider-pprint-eval-last-sexp-and-insert ()
    "Evaluate the last sexp and insert result as comment.

The formatting of the comment is controlled via three options:
    `cider-comment-prefix'           \";; => \"
    `cider-comment-continued-prefix' \";;    \"
    `cider-comment-postfix'          \"\"

so that with customization you can optionally wrap the output
in the reader macro \"#_( .. )\", or \"(comment ... )\", or any
other desired formatting.

If INSERT-BEFORE is non-nil, insert before the form, otherwise afterwards."
    (interactive)
    (let ((cider-comment-prefix "")
          (cider-comment-continued-prefix "")
          (cider-comment-postfix ""))
      (cider-pprint-form-to-comment 'cider-last-sexp nil)))

  ;; make lispy-eval works in babashka repl
  (defadvice! +lispy-eval (orig-fn &rest args)
    :around #'lispy-eval
    (if (+in-clj-p)
        (if (and lispy-mode (lispy-left-p))
            ;; eval on the right side
            (save-excursion
              (call-interactively #'lispy-different)
              (if current-prefix-arg
                  (call-interactively #'+cider-pprint-eval-last-sexp-and-insert)
                (call-interactively #'cider-eval-last-sexp)))
          (if current-prefix-arg
              (call-interactively #'+cider-pprint-eval-last-sexp-and-insert)
            (call-interactively #'cider-eval-last-sexp)))
      (apply orig-fn args)))

  (defadvice! +lispy-eval-and-insert (func &rest args)
    :around #'lispy-eval-and-insert
    (if (+in-clj-p)
        (progn
          (setq current-prefix-arg '(1))
          (call-interactively 'cider-pprint-eval-last-sexp))
      (apply func args)))

  ;; fix cljr-slash
  (after! cider
    (defadvice! +special-lispy-splice-cljr-slash ()
      :after #'special-lispy-splice
      (when (and
             (cider-connected-p)
             (called-interactively-p 'interactive)
             (eq (char-before) ?/))
        (delete-char -1)
        (call-interactively 'cljr-slash)))))

;;; cider
;; (set-popup-rule! "^\\*cider-test-report\\*" :select nil :side 'right :size 0.5 :ignore t)
(defun +cider-repl-clear-input ()
  (interactive)
  (when cider-repl-input-start-mark
    (cider-repl--clear-region cider-repl-input-start-mark (point-max))))

(add-hook! cider-inspector-mode
  (defun +disable-evil-matchit-mode ()
    (evil-matchit-mode -1)))

(defun +clojure-use-cider-over-lsp ()
  "use cider over clojure-lsp for completion when cider is not connected"
  (when (+project-cider-repl-connected)
    (remove-hook! 'completion-at-point-functions :local
      'cider-complete-at-point 'lsp-completion-at-point)
    (add-hook! 'completion-at-point-functions :local :depth -100
               #'cider-complete-at-point)
    (add-hook! 'completion-at-point-functions :local :depth -99
               #'lsp-completion-at-point))

  ;; fix Regular expression too big
  ;; https://github.com/clojure-emacs/cider/issues/2866
  ;; (setq-local cider-font-lock-dynamically '(macro core deprecated function var))
  (setq-local cider-font-lock-dynamically '(macro core deprecated)
              corfu-auto-delay 0.24))

(defun +clojure-use-lsp-over-cider ()
  "use clojure-lsp over cider for completion when cider is not connected"
  (remove-hook! 'completion-at-point-functions :local
    'cider-complete-at-point 'lsp-completion-at-point)
  (add-hook! 'completion-at-point-functions :local :depth -100
             #'lsp-completion-at-point)
  (add-hook! 'completion-at-point-functions :local :depth -99
             #'cider-complete-at-point)
  (setq-local cider-font-lock-dynamically nil
              corfu-auto-delay 0.5))

(defun +setup-clojure-mode ()
  "sort namespace, cleanup log namespace on save"
  (when (+in-clj-p)
    (add-hook! 'before-save-hook :local '+clojure-clean-log-ns 'clojure-sort-ns)
    (if (+project-cider-repl-connected)
        (+clojure-use-cider-over-lsp)
      (+clojure-use-lsp-over-cider))))

(let ((+setup-clojure-mode-hooks
       (append
        +clojure-modes-hooks
        '(cider-disconnected-hook
          cider-connected-hook
          cider-mode-hook
          lsp-completion-mode-hook))))
  (dolist (h +setup-clojure-mode-hooks)
    (add-hook h '+setup-clojure-mode)))

(defun +cider-enable-fuzzy-completion ()
  (interactive)
  (when (< emacs-major-version 27)
    (user-error "`+cider-enable-fuzzy-completion' requires Emacs 27 or later"))
  (let* ((cider (assq 'cider completion-category-overrides))
         (found-styles (when cider (assq 'styles cider)))
         (found-cycle (when cider (assq 'cycle cider))))
    (setq completion-category-overrides
          (seq-remove (lambda (x)
                        (equal 'cider (car x)))
                      completion-category-overrides))
    ;; (setq found-styles '(styles fussy))
    (setq found-styles '(styles hotfuzz))
    (add-to-list 'completion-category-overrides (apply #'list 'cider found-styles (when found-cycle
                                                                                    (list found-cycle))))))

(add-hook! '(cider-mode-hook cider-repl-mode-hook)
           '+cider-enable-fuzzy-completion)

(after! cider
  (setq!
   cider-auto-select-test-report-buffer nil
   cider-repl-buffer-size-limit 1048576
   ;; Regular expression too big
   cider-font-lock-reader-conditionals nil
   cider-repl-use-content-types t
   cider-repl-pop-to-buffer-on-connect nil
   cider-repl-display-output-before-window-boundaries t
   ;; cider-preferred-build-tool 'clojure-cli
   cider-default-cljs-repl 'shadow
   cider-auto-jump-to-error nil
   ;; cider-print-fn 'fipp
   cider-print-fn 'puget
   ;; cider-print-fn 'pprint
   ;; cider-print-fn 'pr
   ;; cider-print-fn 'zprint
   ;; cider-print-options '(("width" 80))
   cider-print-options '(("map-delimiter" ""))
   ;; cider-print-options '(("style" ":community"))
   ;; cider-print-options '(("style" ((keyword "community" ) (keyword "no-comma" ))))
   ;; cider-format-code-options '(("indents" (("letsubs" 0))))
   )
  ;; (pushnew! cider-jack-in-dependencies '("io.github.tonsky/clj-reload" "RELEASE"))
  ;; (defadvice! +cider-ns-refresh (orig-fn &optional mode)
  ;;   :around #'cider-ns-refresh
  ;;   (cider-interactive-eval
  ;;    "(do (require '[clj-reload.core :as reload]) (reload/reload))"))

  ;; status h/deftest-sub
  (setq! +cider-test-defining-forms '("deftest" "defspec" "deftest-sub"))

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
  (set-popup-rule! "^\\*cider-error\\*" :select nil :side 'right :size 0.4)
  (set-popup-rule! "^\\*cider-repl" :side 'bottom :size 0.2 :quit t :modeline t :ttl nil)
  (set-popup-rule! "^\\*cider-inspect\\*" :side 'bottom :size 0.2 :quit t :modeline nil :ttl nil)

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
         (when (nrepl-dict-get a "value")
           (let* ((form (format "
(println \"-----run test------\")
(cljs.test/test-var %s)
(println \"---test finished---\")
"
                                (nrepl-dict-get a "value"))))
             (if (and ns (member deftype +cider-test-defining-forms))
                 (cider-interactive-eval form nil nil (cider--nrepl-pr-request-plist))
               (message "No test at point"))))))))

  (defun +cider-test-ns-execute-cljs ()
    "able to run `deftest' in cljs file, not support showing test result"
    (interactive)
    (cider-interactive-eval "(cljs.test/run-tests)"))

  (defadvice! +cider-test-run-test (orig-fn)
    :around #'cider-test-run-test
    (interactive)
    (if (eq major-mode 'clojurescript-mode)
        (call-interactively '+cider-test-execute-cljs)
      (when (cider--extract-test-var-at-point)
        (call-interactively #'cider-eval-defun-at-point)
        (call-interactively orig-fn))))

  (defadvice! +cider-test-run-ns-tests (orig-fn suppress-inference &optional silent prompt-for-filters)
    :around #'cider-test-run-ns-tests
    (interactive "P")
    (if (eq major-mode 'clojurescript-mode)
        (cider-interactive-eval "(cljs.test/run-tests)")
      (call-interactively orig-fn)))

  (defadvice! +cider-test-run-project-tests (orig-fn prompt-for-filters)
    :around #'cider-test-run-project-tests
    (if (eq major-mode 'clojurescript-mode)
        (cider-interactive-eval "(cljs.test/run-all-tests)")
      (call-interactively orig-fn prompt-for-filters)))

  (defadvice! +cider--display-interactive-eval-result (orig-fn value value-type &optional point overlay-face)
    :around #'cider--display-interactive-eval-result
    (let ((point (cond
                  ((numberp point) point)
                  ((numberp value-type) value-type)
                  (t nil)))
          (value-type (if (symbolp value-type) value-type 'value)))
      (funcall orig-fn value value-type point overlay-face)))

  (cider-add-to-alist
   'cider-jack-in-dependencies
   "org.clojars.abhinav/snitch"
   "0.1.16")
  (cider-add-to-alist
   'cider-jack-in-cljs-dependencies
   "org.clojars.abhinav/snitch"
   "0.1.16")

  (defadvice! +cider-repls (orig-fn &optional type ensure)
    :around #'cider-repls
    (let ((proj-root (doom-project-root))
          (repls (funcall orig-fn type ensure)))
      (seq-filter
       (lambda (repl-buf)
         (when (bufferp repl-buf)
           (with-current-buffer repl-buf
             (string= (doom-project-root) proj-root))))
       repls))))


;; use ns rather than file name for clj buffer name
(use-package! clj-ns-name
  :after clojure-mode
  :config
  (clj-ns-name-install))

(add-hook! '(clojure-mode-hook clojurescript-mode-hook clojurec-mode-hook)
  (defun +enable-cljr () (clj-refactor-mode 1)))

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
    (add-hook! 'cider-mode-hook '+make-cljr-add-use-snippet-interactive))

  (require 'a)
  (setq!
   cljr-magic-require-namespaces
   '(("edn"  . "clojure.edn")
     ("io"     "clojure.java.io" :only ("clj"))
     ("pp"     "cljs.pprint" :only ("cljs"))
     ("oops"     "oops.core" :only ("cljs"))
     ("math" . "clojure.math")
     ("set"  . "clojure.set")
     ("string"  . "clojure.string")
     ("a" "clojure.core.async" :only ("clj"))
     ("a" "cljs.core.async" :only ("cljs"))
     ("resp" . "ring.util.response")
     ("hresp" . "ring.util.http-response")
     ("enc" . "taoensso.encore")
     ("tr" . "taoensso.truss")
     ("ig" . "integrant.core")
     ("d" . "datalevin.core")
     ("t" . "tick.core")
     ("r" "radix" :only ("cljs"))
     ("rf" "re-frame.core" :only ("cljs"))
     ("walk" . "clojure.walk")
     ("zip"  . "clojure.zip"))
   cljr-clojure-test-declaration "[clojure.test :as t :refer [deftest testing is]]"
   cljr-cljc-clojure-test-declaration "#?(:clj [clojure.test :as t :refer [deftest testing is]]
:cljs [cljs.test :as t :include-macros])"))

(defvar +local-cljr-magic-require-namespaces '())
(make-variable-buffer-local '+local-cljr-magic-require-namespaces)
(put '+local-cljr-magic-require-namespaces 'safe-local-variable #'listp)

(defun +build-cljr-magic-require-namespaces ()
  (when (and (boundp '+local-cljr-magic-require-namespaces)
             (listp +local-cljr-magic-require-namespaces))
    (setq-local
     cljr-magic-require-namespaces
     (seq-concatenate 'list
                      +local-cljr-magic-require-namespaces
                      cljr-magic-require-namespaces))))
(add-hook! 'clj-refactor-mode-hook '+build-cljr-magic-require-namespaces)

;; (after! lispy
;;   (after! clojure-mode
;;     (set-formatter! 'cljstyle '("cljstyle" "pipe")
;;       :modes lispy-clojure-modes)
;;     (set-formatter! 'zprint '("zprint" "{:search-config? true :cwd-zprintrc? true}")
;;       :modes lispy-clojure-modes)
;;     (set-formatter! 'cljfmt '+apheleia-lsp-format-buffer
;;       :modes lispy-clojure-modes)))

(setq-default +cider-project-reload-exec-cmd-clj nil
              +cider-project-reload-exec-cmd-cljs nil)


"[snitch.core :refer [defn* defmethod* *fn *let]]"

(defun +cljr--re-search-parent (re)
  (unless (lispyville--top-level-p)
    (ignore-errors (paredit-backward-up))
    (let ((s (or (ignore-errors (cljr--extract-sexp)) "")))
      (when (string-match re s)
        (point)))))

(defun +cljr--find-snitch-point (&optional snitched-point?)
  (let ((not-done? t)
        (rst nil))
    (while not-done?
      (let ((found (+cljr--re-search-parent
                    (if snitched-point?
                        "^(\\(defn\\*\\|defmethod\\*\\|\\*fn\\|\\*let\\) "
                      "^(\\(defn\\|defmethod\\|fn\\|let\\) "))))
        (cond
         (found
          (setq not-done? nil
                rst found))
         ((lispyville--top-level-p)
          (setq not-done? nil))
         (t nil))))
    rst))

(defun +add-snitch ()
  (let ((snitch-found (+cljr--find-snitch-point)))
    (when snitch-found
      (let ((sexp (save-excursion
                    (goto-char snitch-found)
                    (forward-char)
                    (cljr--extract-sexp))))
        (forward-char)
        (when (or
               (string= sexp "defn")
               (string= sexp "defmethod"))
          (forward-sexp))
        (insert "*")
        snitch-found))))

(defun +remove-snitch ()
  (let ((snitched-found (+cljr--find-snitch-point t)))
    (when snitched-found
      (let ((sexp (save-excursion
                    (goto-char snitched-found)
                    (forward-char)
                    (cljr--extract-sexp))))
        (forward-char)
        (if (or
             (string= sexp "defn*")
             (string= sexp "defmethod*"))
            (forward-sexp)
          (forward-char))
        (delete-char -1)
        snitched-found))))

(defun +clojure-inject-snitch (arg)
  (interactive "P")
  (let* ((injected? (ignore-errors (save-excursion (re-search-backward "snitch.core :refer")))))
    (unless injected?
      (save-excursion
        (cljr--insert-in-ns ":require")
        (if (eq major-mode 'clojurescript-mode)
            (insert "[snitch.core :refer-macros [defn* defmethod* *fn *let]]")
          (insert "[snitch.core :refer [defn* defmethod* *fn *let]]"))))

    (save-excursion
      (if arg
          (+add-snitch)
        (unless (save-excursion (+remove-snitch))
          (+add-snitch))))))
