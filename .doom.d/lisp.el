;;; lisp.el -*- lexical-binding: t; -*-

(el-patch-feature lispy)
(after! lispy
  (setq! lispy-close-quotes-at-end-p nil
         lispy-eval-display-style 'overlay)
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

(after! semantic
  (setq! semanticdb-find-default-throttle '(file local project omniscience recursive)))

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
  (lispy-define-key lispy-mode-map "v" #'lispyville-toggle-mark-type)
  (lispy-define-key lispy-mode-map "m" #'lispy-view))

(use-package! parinfer-rust-mode
  :defer t
  ;; :hook (emacs-lisp-mode clojure-mode clojurescript-mode)
  :init
  (setq! parinfer-rust-preferred-mode "paren"
         parinfer-rust-check-before-enable nil))

(use-package! symex
  :defer t
  ;; :hook ((scheme-mode
  ;;         arc-mode
  ;;         clojure-mode
  ;;         clojurescript-mode
  ;;         lisp-mode) . symex-initialize)
  :config
  (global-set-key (kbd "s-;") 'symex-mode-interface))

(after! eval-sexp-fu
  (after! lispy
    (define-eval-sexp-fu-flash-command special-lispy-eval
      (eval-sexp-fu-flash (when (ignore-errors (elisp--preceding-sexp))
                            (with-esf-end-of-sexp
                              (save-excursion
                                (when (lispy-right-p)
                                  (backward-sexp))
                                (bounds-of-thing-at-point 'sexp)))))))
  (after! eros
    (define-eval-sexp-fu-flash-command eros-eval-defun
      (eval-sexp-fu-flash (when (ignore-errors (elisp--preceding-sexp))
                            (save-excursion
                              (end-of-defun)
                              (beginning-of-defun)
                              (bounds-of-thing-at-point 'sexp))))))
  (after! pprint-to-buffer
    (define-eval-sexp-fu-flash-command pprint-to-buffer-last-sexp
      (eval-sexp-fu-flash (when (ignore-errors (elisp--preceding-sexp))
                            (with-esf-end-of-sexp
                              (save-excursion
                                (backward-sexp)
                                (bounds-of-thing-at-point 'sexp))))))))
