(define-configuration document-buffer
    ((default-modes
      (pushnew 'nyxt/mode/vi:vi-normal-mode %slot-value%))))

(define-configuration input-buffer
    ((default-modes
      (pushnew 'nyxt/mode/vi:vi-insert-mode %slot-value%))))

(define-configuration prompt-buffer
    ((default-modes
      (pushnew 'nyxt/mode/emacs:emacs-mode %slot-value%))))

;;; helpers
(define-command-global current-time ()
  "Show the current time."
  (echo "~a" (local-time:now)))

;;; key bindings
;;;; base
(define-configuration base-mode
  "Note the :import part of the define-keyscheme-map.
It re-uses the other keymap (in this case, the one that was slot value before
the configuration) and merely adds/modifies it."
  ((keyscheme-map
    (define-keyscheme-map "my-base" (list :import %slot-value%)
      nyxt/keyscheme:vi-normal
      (list
       "space space" 'execute-command
       "space r r" 'load-config-file
       "space h t" 'tutorial
       "space h m" 'manual
       "space h a" 'describe-any
       "space h b" 'describe-bindings
       "space h C" 'describe-class
       "space h c" 'describe-command
       "space h f" 'describe-function
       "space h k" 'describe-key
       "space h p" 'describe-package
       "space h s" 'describe-slot
       "space h v" 'describe-variable
       "space b b" 'switch-buffer
       "space b n" 'switch-buffer-next
       "space b p" 'switch-buffer-previous

       "r" 'reload-current-buffer
       "R" 'reopen-buffer
       "b" 'switch-buffer
       "h" 'switch-buffer-previous
       "l" 'switch-buffer-next
       "X" 'delete-buffer
       "x" 'delete-current-buffer)))))
;;;; hint
(define-configuration hint-mode
    ((hinting-type :emacs)))

;;;; search
(define-configuration search-buffer-mode
    ((keyscheme-map
      (define-keyscheme-map "my-search-buffer" (list :import %slot-value%)
        nyxt/keyscheme:vi-normal
        (list
         "C-s" 'nothing
         "C-s" 'search-buffer
         "/" 'search-buffer)))))

;;;; history
(define-configuration history-mode
    ((keyscheme-map
      (define-keyscheme-map "my-history" (list :import %slot-value%)
        nyxt/keyscheme:vi-normal
        (list
         "space f r" 'history-all-query
         "H" 'history-backwards
         "L" 'history-forwards)))))

;;;; document
(define-configuration document-mode
    ((keyscheme-map
      (define-keyscheme-map "my-document" (list :import %slot-value%)
        nyxt/keyscheme:vi-normal
        (list
         "h" 'nothing
         "l" 'nothing
         "C-b" 'nothing
         "C-f" 'nothing
         "space" 'nothing
         "C-u" 'scroll-page-up
         "C-d" 'scroll-page-down)))))

;;;; prompt
;; (define-configuration prompt-buffer-mode
;;     ((keyscheme-map
;;       (define-keyscheme-map "my-prompt-buffer-mode" (list :import %slot-value%)
;;         nyxt/keyscheme:emacs
;;         (list
;;          "C-p" 'previous-source
;;          "C-n" 'next-source
;;          "C-k" 'previous-suggestion
;;          "C-j" 'next-suggestion
;;          "C-l" 'set-action-on-return)))))
