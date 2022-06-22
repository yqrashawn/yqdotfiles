;;; lisp.el -*- lexical-binding: t; -*-

;;; lispy
(el-patch-feature lispy)
(after! lispy
  (setq! lispy-close-quotes-at-end-p nil
         lispy-eval-display-style 'overlay)
  (defadvice! +lispy-tab (orig-fn)
    :around #'lispy-tab
    (if (memq major-mode '(clojure-mode clojurescript-mode clojurec-mode))
      (progn (and (functionp #'clojure-align) (call-interactively #'clojure-align))
        (when (region-active-p)
          (call-interactively orig-fn)))
      (call-interactively orig-fn)))

  (use-package! ccc                     ; for cursor style
    :after lispy
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
(use-package! lispyville
  :defer t
  :init
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
  :config
  (lispyville-set-key-theme)
  (lispy-define-key lispy-mode-map "v" #'lispyville-toggle-mark-type)
  (lispy-define-key lispy-mode-map "m" #'lispy-view)
  (lispy-define-key lispy-mode-map "p" #'lispy-paste)
  (lispy-define-key lispy-mode-map "P" #'lispy-eval-other-window))

;;; parinfer
(use-package! parinfer-rust-mode
  :defer t
  ;; :hook (emacs-lisp-mode clojure-mode clojurescript-mode)
  :init
  (setq! parinfer-rust-preferred-mode "paren"
         parinfer-rust-check-before-enable nil))

;;; symex
(use-package! symex
  :after lispy
  :init
  (setq! symex-modal-backend 'evil)
  :config
  (setq! symex-lisp-modes +lispy-modes)
  (setq! symex--user-evil-keyspec
    '(("j" . symex-go-forward)
       ("k" . symex-go-backward)
       ("l" . symex-go-up)
       ("h" . symex-go-down)
       ("C-j" . symex-climb-branch)
       ("C-k" . symex-descend-branch)
       ("C-a" . symex-goto-first)
       ("C-e" . symex-goto-last)
       ("M-j" . symex-goto-highest)
       ("M-k" . symex-goto-lowest)
       (">" . symex-capture-forward)
       ("<" . symex-capture-backward)
       ("[" . symex-soar-backward)
       ("]" . symex-soar-forward)
       ("{" . symex-create-square)
       ("}" . symex-wrap-square)
       ("M-[" . symex-leap-backward)
       ("M-]" . symex-leap-forward)))
  (require 'hi-lock)
  ;; cursor color
  (defadvice! +evil-update-cursor-color-h-symex ()
    :after #'+evil-update-cursor-color-h
    (put 'cursor 'evil-symex-color (face-background 'hi-green)))
  (put 'cursor 'evil-symex-color (face-background 'hi-green))
  (defun +evil-symex-cursor-fn ()
    (evil-set-cursor-color (get 'cursor 'evil-symex-color)))
  (setq! evil-symex-state-cursor '+evil-symex-cursor-fn)

  ;; toggle lispy-mode
  (defun +symex-mode-interface ()
      (lispy-mode -1)
      (lispyville-mode -1))
  (defun +symex-state-exit ()
      (lispy-mode 1))
  (add-hook! 'evil-symex-state-entry-hook '+symex-mode-interface)
  (add-hook! 'evil-symex-state-exit-hook '+symex-state-exit)

  (remove-hook! 'evil-symex-state-exit-hook #'symex-disable-editing-minor-mode)
  (defadvice! +evil-force-normal-state (orig)
    :around #'evil-force-normal-state
    (if (and (not (evil-symex-state-p)) lispy-mode)
      (symex-mode-interface)))

  (symex-initialize)

  ;; disable default symex-mode-map
  (undefine-key! symex-mode-map "(" ")" "[" "]" "<backspace>" "<DEL>" "\"")
  (general-evil-define-key '(symex) '(symex-editing-mode-map) "s" 'yq-s-map)
  (global-set-key (kbd "s-;") #'symex-mode-interface)

  (defadvice! +symex-evaluate (orig-fn count)
    "evaluate at right paren"
    :around #'symex-evaluate
    (interactive "p")
    (if (eq (char-after) 41)
      (save-excursion
        (evil-jump-item)
        (call-interactively orig-fn count))
      (call-interactively orig-fn count))))

;;; eval-sexp-fu
(after! eval-sexp-fu
  (after! lispy
    (define-eval-sexp-fu-flash-command lispy--eval
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

;;; tweak
(after! hideshow
  (defadvice! +hs-hide-level (orig-fn arg)
    "hs-hide-level when cursor at left paren in lispy-modes"
    :around #'hs-hide-level
    (interactive "p")
    (if (and (+lispy-modes-p) (eq (char-after) 40))
      (save-excursion
        (backward-char)
        (call-interactively orig-fn arg))
      (call-interactively orig-fn arg))))
