;;; evil.el -*- lexical-binding: t; -*-

;; C-h
;; (keyboard-translate ?\C-h ?\C-?)
;; (global-set-key [(control ?h)] 'delete-backward-char)

(after! evil
  (setq! evil-escape-key-sequence nil
         evil-split-window-below t
         evil-vsplit-window-right t
         evil-move-cursor-back nil
         evil-kill-on-visual-paste nil
         evil-esc-delay 0
         evil-shift-width 2
         evil-ex-substitute-global t
         evil-want-fine-undo t
         evil-search-module 'isearch
         evil-want-C-i-jump t
         evil-want-C-d-scroll t
         evil-want-C-u-scroll t
         evil-want-C-w-delete t)
  ;; (setq evil-insert-state-cursor '(bar "green"))
  ;; (setq local-function-key-map (delq '(kp-tab . [9]) local-function-key-map))
  ;; (define-key input-decode-map [?\C-i] [C-i])
  (mapc #'evil-declare-change-repeat
        '(company-complete-common
          company-complete-selection
          company-complete-number
          hippie-expand))
  (evil-ex-define-cmd "ie[dit]" #'evil-multiedit-ex-match))

(after! evil-textobj-anyblock
  (setq! evil-textobj-anyblock-blocks
         '(("(" . ")")
           ("{" . "}")
           ("\\[" . "\\]")
           ("<" . ">")
           ("'" . "'")
           ("\"" . "\"")
           ("`" . "`")
           ("“" . "”"))))

(after! evil-snipe
  (setq! evil-snipe-scope 'whole-buffer
         evil-snipe-repeat-scope 'buffer))

(after! expand-region
  (setq! expand-region-contract-fast-key "V"
         expand-region-reset-fast-key "r"))

(use-package! evil-iedit-state
  :commands (evil-iedit-state/iedit-mode-from-expand-region evil-iedit-state/iedit-mode)
  :config
  (defalias 'iedit-lib-cleanup #'iedit-cleanup)
  (after! evil-multiedit (setq! evil-multiedit-store-in-search-history t))
  (define-key evil-iedit-state-map "V" nil)
  (define-key evil-iedit-state-map "m" 'iedit-show/hide-unmatched-lines))
(use-package mwim
  :commands (mwim-beginning-of-code-or-line mwim-end-of-code-or-line))

;; TODO: to upstream
(use-package! evil-anzu
  :after-call
  evil-search-next
  evil-search-previous
  evil-search-word
  evil-search-word-forward
  evil-search-word-backward
  evil-search-forward
  evil-search-backward)
