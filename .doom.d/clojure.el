;;; lang/clojure.el -*- lexical-binding: t; -*-

(defvar pkg-status-mobile--references nil
  "Store references, for debugging purposes.")

(defvar pkg-status-mobile--debug-p nil
  "When non-nil, enable debugging.")

(defun pkg-status-mobile/re-frame-find-file-from-ref (reference keyword-to-find curr-point-marker)
  (let* ((path (replace-regexp-in-string "file://" "" (map-elt reference :uri)))
         (line-number (map-nested-elt reference [:range :start :line]))
         (line-column (map-nested-elt reference [:range :start :character]))
         (open-file-at-reg-call (lambda ()
                                  (find-file path)
                                  (goto-char (point-min))
                                  (forward-line line-number)
                                  (forward-char line-column)
                                  (recenter-top-bottom)
                                  (evil-set-jump)
                                  (xref-push-marker-stack curr-point-marker)
                                  (throw 'found 'deferred))))
    (with-temp-buffer
      (insert-file-contents path)
      (clojure-mode)
      (goto-char (point-min))
      (forward-line line-number)
      (forward-char line-column)
      (when (beginning-of-defun)
        (paredit-forward-down)
        (let ((thing (thing-at-point 'symbol 'no-properties)))
          (cond
           ((string-equal "rf/defn" thing)
            ;; Cursor will be at the end of the (optional) docstring closing
            ;; double quotes or at the end of a line like {:events XYZ}, or the
            ;; body of the event handler, in which case no match will be found.
            (paredit-forward 3)
            (backward-char)
            (if (clojure--in-string-p) ; at docstring
                (progn
                  (forward-line)
                  (beginning-of-line)
                  (when (search-forward (concat "{:events [" keyword-to-find "]") (line-end-position) t)
                    (funcall open-file-at-reg-call)))
              (beginning-of-line)
              (when (search-forward (concat "{:events [" keyword-to-find "]") (line-end-position) t)
                (funcall open-file-at-reg-call))))

           ((or (string-match-p "rf/reg-.*" thing)
                (string-match-p "re-frame/reg-.*" thing)
                (string-match-p "reg-root-key-sub" thing))
            (paredit-forward 2)
            (when (string-equal keyword-to-find (thing-at-point 'symbol 'no-properties))
              (funcall open-file-at-reg-call)))))))))

(defun pkg-status-mobile/re-frame-find-references-raw ()
  (let ((exclude-declaration nil))
    (lsp-request "textDocument/references"
                 (append (lsp--text-document-position-params)
                         (list :context
                               `(:includeDeclaration ,(lsp-json-bool (not (or exclude-declaration lsp-references-exclude-definition)))))))))

(defun pkg-status-mobile/re-frame-find-registration ()
  (interactive)
  (let ((keyword-to-find (thing-at-point 'symbol 'no-properties))
        (references (pkg-status-mobile/re-frame-find-references-raw))
        (curr-point-marker (point-marker)))
    (when pkg-status-mobile--debug-p
      (setq pkg-status-mobile--references references))
    (catch 'found
      (seq-doseq (ref references)
        (pkg-status-mobile/re-frame-find-file-from-ref ref keyword-to-find curr-point-marker)))))

(defadvice! ++lsp-lookup-definition-handler (_orig-fn)
  "Find definition of the symbol at point using LSP."
  :around #'+lsp-lookup-definition-handler
  (interactive)
  (when-let (loc (lsp-request "textDocument/definition"
                              (lsp--text-document-position-params)))
    (if (and
         (require 'clojure-mode nil t)
         (eq major-mode 'clojurescript-mode)
         (string= (lsp:location-uri loc) (concat "file://" (buffer-file-name (current-buffer)))))
        (let* ((current-bounds (bounds-of-thing-at-point 'symbol))
               (start (car current-bounds))
               (end (cdr current-bounds))
               (start-line (line-number-at-pos start))
               (end-line (line-number-at-pos end))
               (start-column (save-excursion (goto-char start) (current-column)))
               (end-column (save-excursion (goto-char end) (current-column))))

          (when (and (-> loc
                         lsp:location-range
                         lsp:range-start
                         lsp:position-line
                         (+ 1)
                         (eq start-line))
                     (-> loc
                         lsp:location-range
                         lsp:range-start
                         lsp:position-character
                         (eq start-column))
                     (-> loc
                         lsp:location-range
                         lsp:range-end
                         lsp:position-line
                         (+ 1)
                         (eq end-line))
                     (-> loc
                         lsp:location-range
                         lsp:range-end
                         lsp:position-character
                         (eq end-column)))
            (log/spy (call-interactively #'pkg-status-mobile/re-frame-find-registration))))
      (progn
        (lsp-show-xrefs (lsp--locations-to-xref-items loc) nil nil)
        'deferred))))

(set-lookup-handlers! '(cider-mode cider-repl-mode)
  :definition #'+clojure-cider-lookup-definition
  :documentation #'cider-doc)

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
         clojure-max-backtracking 10
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

  ;; (setq-hook! 'clojurescript-mode-hook
  ;;   +lookup-definition-functions
  ;;   (seq-concatenate 'list '(+lookup-status-mobile-re-frame-event-handler-defination) +lookup-definition-functions))
  )

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
      (apply func args))))

;;; cider
;; (set-popup-rule! "^\\*cider-test-report\\*" :select nil :side 'right :size 0.5 :ignore t)
(defun +cider-repl-clear-input ()
  (interactive)
  (when cider-repl-input-start-mark
    (cider-repl--clear-region cider-repl-input-start-mark (point-max))))

(add-hook! cider-mode
  (defun +clojure-use-cider-over-lsp ()
    "use cider over clojure-lsp for completion when cider is not connected"
    (remove-hook! 'completion-at-point-functions :local
      'cider-complete-at-point 'lsp-completion-at-point)
    (add-hook! 'completion-at-point-functions :local :depth -100
               #'cider-complete-at-point)
    (add-hook! 'completion-at-point-functions :local :depth -99
               #'lsp-completion-at-point)

    ;; fix Regular expression too big
    ;; https://github.com/clojure-emacs/cider/issues/2866
    ;; (setq-local cider-font-lock-dynamically '(macro core deprecated function var))
    (setq-local cider-font-lock-dynamically '(macro core deprecated))))

(add-hook! cider-inspector-mode
  (defun +disable-evil-matchit-mode ()
    (evil-matchit-mode -1)))

(defun +clojure-use-lsp-over-cider ()
  "use clojure-lsp over cider for completion when cider is not connected"
  (remove-hook! 'completion-at-point-functions :local
    'cider-complete-at-point 'lsp-completion-at-point)
  (add-hook! 'completion-at-point-functions :local :depth -100
             #'lsp-completion-at-point)
  (add-hook! 'completion-at-point-functions :local :depth -99
             #'cider-complete-at-point)
  (setq-local cider-font-lock-dynamically nil))

(add-hook! 'cider-disconnected-hook '+clojure-use-lsp-over-cider)

(defun +setup-clojure-mode ()
  "sort namespace, cleanup log namespace on save"
  (add-hook! 'before-save-hook :local '+clojure-clean-log-ns 'clojure-sort-ns)
  (if (bound-and-true-p lsp-mode)
      (+clojure-use-lsp-over-cider)
    (+clojure-use-cider-over-lsp)))

(add-hook! '(clojure-mode-hook clojurescript-mode-hook clojurec-mode-hook) '+setup-clojure-mode)

(defun +cider-enable-fuzzy-completion ()
  (interactive)
  (when (< emacs-major-version 27)
    (user-error "`+cider-enable-fuzzy-completion' requires Emacs 27 or later"))
  (let ((found-styles (when-let ((cider (assq 'cider completion-category-overrides)))
                        (assq 'styles cider)))
        (found-cycle (when-let ((cider (assq 'cider completion-category-overrides)))
                       (assq 'cycle cider))))
    (setq completion-category-overrides (seq-remove (lambda (x)
                                                      (equal 'cider (car x)))
                                                    completion-category-overrides))
    ;; (unless (member (car completion-styles) found-styles)
    ;;   (setq found-styles (append found-styles (list (car completion-styles)))))
    (setq found-styles '(styles fussy))
    (add-to-list 'completion-category-overrides (apply #'list 'cider found-styles (when found-cycle
                                                                                    (list found-cycle))))))

(add-hook! 'cider-repl-mode-hook '+cider-enable-fuzzy-completion)
(add-hook! 'cider-mode-hook '+cider-enable-fuzzy-completion)

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
         (when (nrepl-dict-get a "value")
           (let* ((form (format "
(println \"-----run test------\")
(cljs.test/test-var %s)
(println \"---test finished---\")
"
                                (nrepl-dict-get a "value"))))
             (if (and ns (member deftype +cider-test-defining-forms))
                 (cider-interactive-eval form nil nil (cider--nrepl-pr-request-map))
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
      (funcall orig-fn value value-type point overlay-face))))

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
    (add-hook! 'cider-mode-hook '+make-cljr-add-use-snippet-interactive))

  (require 'a)
  (setq!
   cljr-magic-require-namespaces
   '(("edn"  . "clojure.edn")
     ("io"     "clojure.java.io" :only ("clj"))
     ("pp"     "cljs.pprint" :only ("cljs"))
     ("math" . "clojure.math")
     ("set"  . "clojure.set")
     ("string"  . "clojure.string")
     ("a" . "cljure.core.async")
     ("response" . "ring.util.response")
     ("enc" . "taoensso.encore")
     ("ig" . "integrant.core")
     ("d" . "datalevin.core")
     ("r" . "radix")
     ("rf" . "re-frame.core")
     ("walk" . "clojure.walk")
     ("zip"  . "clojure.zip"))
   cljr-clojure-test-declaration "[clojure.test :as t :refer [deftest testing is]]"
   cljr-cljc-clojure-test-declaration "#?(:clj [clojure.test :as t :refer [deftest testing is]]
:cljs [cljs.test :as t :include-macros])"))

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
