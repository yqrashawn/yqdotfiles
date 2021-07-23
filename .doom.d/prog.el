;;; prog.el -*- lexical-binding: t; -*-

(setq +company-backend-alist
      '((text-mode (:separate company-dabbrev company-yasnippet company-files company-ispell))
        (prog-mode company-tabnine company-capf company-yasnippet company-files company-keywords company-dabbrev-code company-dabbrev)
        (conf-mode company-tabnine company-capf company-dabbrev-code company-yasnippet)))

(setq! projectile-project-search-path '("~/workspace/office" "~/workspace/home" "~/workspace/third"))

(use-package! company-flx
  :defer t
  :init (add-hook! emacs-lisp-mode #'company-flx-mode))

(after! dash-docs
  (setq! dash-docs-docsets-path
         (let ((original-dash-path (expand-file-name "~/Library/Application Support/Dash/DocSets")))
           (if (and (string-equal system-type 'darwin)
                    (file-directory-p original-dash-path))
               original-dash-path
             dash-docs))))

(defvar yq//company-numbers '(59 ?a ?s ?d ?f ?g ?h ?j ?k ?l))

(defun yq//company-format-numbers (numbered)
  (format " %s" (char-to-string (nth (mod numbered 10) yq//company-numbers))))

(after! company
  (setq! company-selection-wrap-around t
         company-show-numbers t
         company-require-match nil
         company-dabbrev-minimum-length 2
         company-search-regexp-function #'company-search-flex-regexp
         company-show-numbers-function 'yq//company-format-numbers)

  (dotimes (i 10)
    (define-key! company-active-map
      (read-kbd-macro (format "M-%d" i)) #'company-complete-number
      (read-kbd-macro (format "C-x C-6 %d" i)) #'company-complete-number)))

(use-package! company-tabnine
  :defer t
  :init
  (setq! company-tabnine-binaries-folder "~/.TabNine/binaries/"
         company-tabnine-context-radius 6000
         company-tabnine-context-radius-after 6000
         company-tabnine-log-file-path "~/Downloads/tabnine.log")
  :config
  (setq! company-tabnine--disabled t) )
(use-package! copy-as-format :defer t)
(use-package! separedit :defer t)

(after! format-all
  (defadvice! +format-all--formatter-executable (orig-fn formatter)
    :around #'format-all--formatter-executable
    (let* ((home (concat (getenv "HOME") "/"))
           (root (doom-project-root))
           (root (if (or (not root) (string= home root)) (expand-file-name "~/.config/yarn/global/") root)))
      (if (file-executable-p (concat root "node_modules/" ".bin/" (symbol-name formatter)))
          (concat root "node_modules/" ".bin/" (symbol-name formatter))
        (apply orig-fn formatter))))

  ;; run prettier after lsp format (eslint)
  (defadvice! ++format/region-or-buffer (orig-fn)
    :around #'+format/region-or-buffer
    (ignore-errors (call-interactively orig-fn))
    (when (memq major-mode '(rjsx-mode js-mode js2-mode typescript-mode))
     (let ((+format-with-lsp nil))
       (ignore-errors (call-interactively orig-fn))))))

(use-package! company-ctags :defer t)

;; try fix company overlay performance
;; TODO: check if this works
(defadvice! +company-tng-frontend (orig command)
  :around #'company-tng-frontend
  (overlay-recenter (point))
  (setq-local inhibit-field-text-motion t)
  (funcall orig command))
(defadvice! +company-enable-overriding-keymap (orig keymap)
  :around #'company-enable-overriding-keymap
  (if keymap (setq-local inhibit-field-text-motion t)
    (setq-local inhibit-field-text-motion nil))
  (funcall orig keymap))

;; try to speed up the overlay
(defvar last-post-command-position 0
  "Holds the cursor position from the last run of post-command-hooks.")

(make-variable-buffer-local 'last-post-command-position)

(defun do-stuff-if-moved-post-command ()
  (let ((p (point)))
    (unless (equal p last-post-command-position)
      (overlay-recenter p))
    (setq last-post-command-position p)))

(add-hook! 'post-command-hook #'do-stuff-if-moved-post-command)

(setq display-line-numbers-type nil)

(after! lsp-mode
  (delq! 'lsp-ui-mode lsp-mode-hook))