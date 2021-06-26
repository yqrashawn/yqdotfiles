;;; lisp.el -*- lexical-binding: t; -*-

(el-patch-feature lispy)
(after! lispy
  (setq! lispy-close-quotes-at-end-p nil
         lispy-eval-display-style 'overlay)
  ;; make lispy-eval works in babashka repl
  (defadvice! +lispy-eval (orig-fn &rest args)
    :around #'lispy-eval
    (if (and (memq major-mode '(clojure-mode)) (functionp 'cider--babashka-version) (cider--babashka-version))
        (if (and lispy-mode (lispy-left-p))
            (save-excursion
              (call-interactively 'lispy-different)
              (call-interactively 'cider-eval-last-sexp))
          (call-interactively 'cider-eval-last-sexp))
      (apply orig-fn args)))
  (defadvice! +lispy-eval-and-insert (func &rest args)
    :around #'lispy-eval-and-insert
    (if (and (memq major-mode '(clojure-mode)) (functionp 'cider--babashka-version) (cider--babashka-version))
        (progn
          ;; (setq current-prefix-arg '(1))
          (call-interactively 'cider-pprint-eval-last-sexp))
      (apply func args)))
  (defadvice! +lispy-right (a) :after #'lispy-right (call-interactively #'lispy-tab))
  (defadvice! +lispy-left (a) :after #'lispy-left (call-interactively #'lispy-tab))
  (defadvice! +lispy-up (a) :after #'lispy-up (call-interactively #'lispy-tab))
  (defadvice! +lispy-down (a) :after #'lispy-down (call-interactively #'lispy-tab))
  (defadvice! +lispy-tab (orig-fn)
    :around #'lispy-tab
    (if (memq major-mode '(clojure-mode clojurescript-mode clojurec-mode))
        (and (functionp #'clojure-align) (call-interactively #'clojure-align))
      (call-interactively orig-fn)))

  (use-package! ccc                     ; for cursor style
    :init
    (defun +lispy-update-cursor-style ()
      (when (and lispy-mode (evil-insert-state-p))
        (if (or (lispy-right-p) (lispy-left-p) (region-active-p))
            (progn (setq-local cursor-type '(bar . 3))
                   (ccc-set-buffer-local-cursor-color "plum1"))
          (progn (setq-local cursor-type '(bar . 3))
                 (ccc-set-buffer-local-cursor-color "green")))))
    :config
    (add-hook 'post-command-hook '+lispy-update-cursor-style))

  (after! hydra
    (defhydra lh-knight ()
      "knight"
      ("j" lispy-knight-down)
      ("k" lispy-knight-up)
      ("r" evil-open-folds :exit t)
      ("h" save-buffer :exit t)
      ("l" (lambda (arg)
             (interactive "p")
             (backward-char)
             (hs-hide-level arg)
             (forward-char)) :exit t)
      ("z" nil)))

  (el-patch-defun lispy-outline-level ()
    "Compute the outline level of the heading at point."
    (save-excursion
      (save-match-data
        (end-of-line)
        (if (re-search-backward lispy-outline nil t)
            (max (el-patch-swap (cl-count ?* (match-string 0))
                                (- (cl-count ?\; (match-string 0)) 2)) 1)
          0))))
  (el-patch-defun lispy-backtick ()
    "Insert `."
    (interactive)
    (if (region-active-p)
        (el-patch-swap (lispy--surround-region "`" "`")
                       (if (memq major-mode '(clojure-mode clojurescript-mode))
                           (lispy--surround-region "`" "`")
                         (lispy--surround-region "`" "'")))
      (el-patch-swap (lispy--space-unless "\\s-\\|\\s(\\|[:?`']\\|\\\\")
                     (if (memq major-mode '(clojure-mode clojurescript-mode))
                         (lispy--space-unless "\\s-\\|\\s(\\|[:?`]\\|\\\\")
                       (lispy--space-unless "\\s-\\|\\s(\\|[:?`']\\|\\\\")))
      (insert "`")))
  (el-patch-defun lispy--oneline (expr &optional ignore-comments)
    "Remove newlines from EXPR.
When IGNORE-COMMENTS is not nil, don't remove comments.
Instead keep them, with a newline after each comment."
    (lispy-mapcan-tree
     (lambda (x y)
       (cond ((el-patch-swap (equal x '(ly-raw newline))
                             (or (equal x '(ly-raw newline))
                                 (equal x '(ly-raw clojure-symbol ","))))
              y)
             ((lispy--raw-comment-p x)
              (if (null ignore-comments)
                  (progn
                    (push x lispy--oneline-comments)
                    y)
                (if (equal (car y) '(ly-raw newline))
                    (cons x y)
                  `(,x (ly-raw newline) ,@y))))
             ((and (lispy--raw-string-p x)
                   (null ignore-comments))
              (cons `(ly-raw string ,(replace-regexp-in-string "\n" "\\\\n" (cl-caddr x)))
                    y))
             (t
              (cons x y))))
     expr)))

;;; lispyville
(after! lispyville
  (setq!
   lispyville-motions-put-into-special t
   lispyville-key-theme '(c-w
                          operators
                          prettify
                          ;; text-objects
                          (atom-movement t) ;; bind to WORD
                          additional-movement
                          commentary
                          slurp/barf-lispy
                          ;; wrap                               ;; M-( M-{ M-[
                          ;; (additional-wrap normal visual insert)
                          (additional normal visual insert) ;; M-j M-k M-J M-k M-s M-S M-r M-t M-v
                          additional-insert
                          escape
                          mark-special
                          mark-toggle))
  (lispyville-set-key-theme)

  ;; REVIEW Delete this once https://github.com/noctuid/lispyville/pull/297 is merged
  (defadvice! +lispy--fix-lispyville-end-of-defun-a (_)
    "lispyville-end-of-defun doesn't go to the next defun when
point is already at the end of a defun, whereas
lispyville-beginning-of-defun does."
    :before #'lispyville-end-of-defun
    (when (<= (- (line-end-position)
                 (point))
              1)
      (forward-line)))

  (lispy-define-key lispy-mode-map "v" #'lispyville-toggle-mark-type)
  (lispy-define-key lispy-mode-map "m" #'lispy-view)

  (lispyville--define-key 'normal
    ";" (cmd! (lispy-comment) (evil-next-visual-line))
    "ti" #'lispyville-backward-up-list
    "ta" (cmd! (lispyville-up-list) (lispy-newline-and-indent))
    "tR" #'lispyville-raise-list
    "tr" #'lispy-raise-sexp
    "tt" (cmd! (lispyville-backward-up-list) (lispy-parens 1))
    "td" #'transpose-sexps
    "tw" #'lispy-move-up
    "tJ" #'lispy-join
    "t/" #'lispy-splice
    "ts" #'lispy-split
    "tC" #'lispy-convolute
    "txb" (lambda ()
            (interactive)
            (if (and (fboundp 'cljr-introduce-let)
                     (memq major-mode lispy-clojure-modes))
                (cljr-introduce-let)
              (lispy-bind-variable)))
    (kbd "M-RET") #'lispyville-wrap-round
    "{" (cmd! (lispyville-insert-at-beginning-of-list 1) (insert " ") (backward-char))
    "}" (cmd! (lispyville-insert-at-end-of-list 1) (insert "")))

  (lispyville--define-key 'insert
    (kbd "M-RET") #'lispyville-wrap-round
    (kbd "C-r") #'lispyville-backward-up-list)
  (lispyville--define-key 'visual
    (kbd "(") #'lispyville-wrap-round
    (kbd "{") #'lispyville-wrap-braces
    (kbd "[") #'lispyville-wrap-brackets))
