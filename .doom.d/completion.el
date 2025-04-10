;;; completion.el -*- lexical-binding: t; -*-

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

(use-package! capf-autosuggest
  :hook ((eshell-mode comint-mode) . capf-autosuggest-mode))

(after! orderless
  (setq orderless-component-separator "[ ,j]"))

(defun +company-abort ()
  (when (fboundp 'company--active-p)
    (when (company--active-p)
      (company-abort))))

(after! evil
  (when (modulep! :completion company)
    (add-hook! 'evil-normal-state-entry-hook '+company-abort)))

(use-package! fuz
  :after orderless
  :init
  (set-popup-rule! "^\\*fuz compilation\\*" :select nil :side 'bottom :size 0.2 :quit t)
  :config
  (unless (require 'fuz-core nil t)
    (fuz-build-and-load-dymod))
  (setq! fussy-score-fn 'fussy-fuz-score))

(use-package! fussy
  :after fuz
  :config
  (setq!
   fussy-filter-fn #'fussy-filter-default
   fussy-ignore-case t
   fussy-use-cache t
   fussy-default-regex-fn #'fussy-pattern-flex-2
   ;; completion-category-defaults nil
   completion-category-overrides '((cider (styles fussy))
                                   (lsp-capf (styles fussy))))
  ;; (pushnew! completion-styles 'fussy)

  (after! corfu
    (advice-add 'corfu--capf-wrapper :before 'fussy-wipe-cache)
    (add-hook 'corfu-mode-hook
              (lambda ()
                (setq-local fussy-max-candidate-limit 5000
                            fussy-default-regex-fn 'fussy-pattern-first-letter
                            fussy-prefer-prefix nil))))

  (defadvice! +read-extended-command (orig-fn &optional prompt)
    :around #'read-extended-command
    (let ((completion-styles '(fussy orderless basic)))
      (funcall orig-fn prompt))))

(use-package! hotfuzz
  :after orderless
  :init
  (setq! completion-ignore-case t)
  :config
  (require 'hotfuzz-module nil t)
  (setq! completion-styles '(orderless hotfuzz basic))
  ;; (pushnew! completion-styles 'hotfuzz)

  ;; (setq! completion-styles '(hotfuzz orderless basic))
  ;; (setq! completion-styles '(orderless basic))
  ;; (setq! completion-styles '(basic))

  ;; (require 'orderless)
  ;; (defadvice! +hotfuzz-all-completions (orig-fn string table pred point)
  ;;   :around #'hotfuzz-all-completions
  ;;   (if (eq (length string) 0)
  ;;       (funcall #'orderless-all-completions string table pred point)
  ;;     (funcall orig-fn string table pred point)))

  ;; (setq! completion-styles '(hotfuzz))
  ;; (setq! completion-styles '(orderless))
  ;; (setq! fussy-score-fn 'fussy-hotfuzz-score)
  )

(after! cape
  (setq! cape-dict-file (list
                         (expand-file-name "~/Dropbox/sync/personal_dict")
                         (expand-file-name "~/Dropbox/sync/gh_username_dict")
                         "/usr/share/dict/words")))

(after! yasnippet-capf
  (add-hook! 'yas-minor-mode-hook :append
    (defun +corfu-remove-t-in-completion-at-point-functions ()
      (remove-hook! 'completion-at-point-functions :local 't))))

(after! vertico
  (require 'marginalia))

(defun +tabnine-setup ()
  (require 'tabnine))

(use-package! pabbrev
  :hook (doom-first-file . global-pabbrev-mode)
  :init
  (require 'cape)
  (setq! pabbrev-use-built-in-completion nil
         pabbrev-idle-timer-verbose nil)

  ;; (setq-local completion-at-point-functions (list))
  (add-hook! 'pabbrev-mode-hook
    (defun +corfu-add-pabbrev-capf-h ()
      (add-hook 'completion-at-point-functions #'pabbrev-capf 1 t)
      (add-hook 'completion-at-point-functions #'cape-abbrev 1 t)
      (add-hook 'completion-at-point-functions #'cape-keyword 1 t)
      ;; (add-hook 'completion-at-point-functions #'tabnine-completion-at-point 1 t)
      ))
  :config
  (add-to-list 'hippie-expand-try-functions-list #'pabbrev-expand-maybe)

  (defadvice! +pabbrev-pre-command-hook (orig-fn)
    :around #'pabbrev-pre-command-hook
    (unless (eq this-command '+complete-at-point)
      (funcall orig-fn)))
  (defadvice! +pabbrev-post-command-hook (orig-fn)
    :around #'pabbrev-post-command-hook
    (unless (eq this-command '+complete-at-point)
      (funcall orig-fn))))

;; (defun force-debug (func &rest args)
;;   (condition-case e
;;       (apply func args)
;;     ((debug error) (signal (car e) (cdr e)))))

;; (defun ++debug-corfu-post-command ()
;;   (setq debug-on-error t)

;;   (advice-add #'corfu--post-command :around #'force-debug))

(after! corfu
  (setq! corfu-preselect 'directory))


;; (use-package! fuzzy-matcher
;;   :ensure nil
;;   :demand t
;;   :init
;;   (load-library "libfuzzy_matcher_el.dylib")
;;   :config
;;   (defun fuzzy-matcher-without-tofu-char (string)
;;     (if (and
;;          (fboundp 'consult--tofu-p)
;;          (consult--tofu-p (aref string (- (length string) 1))))
;;         (substring string 0 (- (length string) 1))
;;       string))
;;   (defun fuzzy-matcher-propertize (pattern candidate)
;;     (let* ((score (fuzzy-matcher-fuzzy-indices pattern (fuzzy-matcher-without-tofu-char candidate)))
;;            (candidate (copy-sequence candidate)))
;;       (unless (string-empty-p pattern)
;;         (put-text-property 0 1 'completion-score (- (* (or (car score) 0) 100) (length candidate)) candidate))
;;       (dolist (char (cdr score))
;;         (add-face-text-property char (1+ char) 'completions-common-part nil candidate))
;;       (when-let* ((char (last (cdr score)))
;;                   (char (car char)))
;;         (when (length> candidate (1+ char))
;;           (add-face-text-property (1+ char) (+ 2 char) 'completions-first-difference nil candidate)))
;;       candidate))
;;   (defun fuzzy-matcher-all-completions (string table pred point)
;;     (let* ((beforepoint (substring string 0 point))
;;            (afterpoint (substring string point))
;;            (bounds (completion-boundaries beforepoint table pred afterpoint))
;;            (infix (concat
;;                    (substring beforepoint (car bounds))
;;                    (substring afterpoint 0 (cdr bounds)))))
;;       (pcase-let ((`(,all ,_pattern ,prefix ,_suffix ,_carbounds)
;;                    (completion-substring--all-completions string table pred point #'completion-flex--make-flex-pattern)))
;;         (when all
;;           (nconc (mapcar (-partial #'fuzzy-matcher-propertize (downcase infix)) all) (length prefix))))))
;;   (add-to-list 'completion-styles-alist '(fuzzy
;;                                           completion-flex-try-completion
;;                                           fuzzy-matcher-all-completions
;;                                           "Fuzzy completion with scoring."))
;;   (defun fuzzy-matcher-adjust-metadata (metadata)
;;     (let ((existing-dsf
;;            (completion-metadata-get metadata 'display-sort-function))
;;           (existing-csf
;;            (completion-metadata-get metadata 'cycle-sort-function)))
;;       (cl-flet
;;           ((compose-flex-sort-fn
;;              (existing-sort-fn)
;;              (lambda (completions)
;;                (sort
;;                 (funcall existing-sort-fn completions) (lambda (c1 c2) (let ((s1 (get-text-property 0 'completion-score c1)) (s2 (get-text-property 0 'completion-score c2))) (> (or s1 0) (or s2 0)))))))) `(metadata (display-sort-function . ,(compose-flex-sort-fn (or existing-dsf #'identity))) (cycle-sort-function . ,(compose-flex-sort-fn (or existing-csf #'identity)))
;;                                                                                                                                                                                                               (cdr metadata)))))
;;   (put 'fuzzy 'completion--adjust-metadata (lambda (metadata)
;;                                              (if (let ((input (minibuffer-contents-no-properties)))
;;                                                    (or
;;                                                     (string-empty-p input)
;;                                                     (and
;;                                                      (eq (completion-metadata-get metadata 'category) 'file)
;;                                                      (string-suffix-p "/" input))))
;;                                                  metadata
;;                                                (fuzzy-matcher-adjust-metadata metadata))))
;;   (setq completion-styles '(fuzzy)))
