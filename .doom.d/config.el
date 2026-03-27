;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; (setq compile-angel-verbose t)
;; (compile-angel-on-load-mode)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "yqrashawn"
      user-mail-address "namy.19@gmail.com")

;; Prevent editorconfig from triggering tramp connections on remote files
;; The advice doesn't check editorconfig-exclude-regexps, so we wrap it directly
(after! editorconfig
  (advice-add 'editorconfig--advice-find-file-noselect :around
              (defun +editorconfig-skip-remote-a (fn f filename &rest args)
                (if (file-remote-p filename)
                    (apply f filename args)
                  (apply fn f filename args)))))

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq
 doom-font (font-spec :family "PragmataPro Mono Liga" :size 18 :weight 'medium :slant 'normal)
 ;; doom-variable-pitch-font (font-spec :family "Times Newer Roman")
 doom-variable-pitch-font (font-spec :family "Lucida Grande")
 ;; doom-variable-pitch-font (font-spec :family "Times Newer Roman" :size 20 :weight 'medium :slant 'normal)
 ;; doom-variable-pitch-font (font-spec :family "Noto Serif" :size 16 :weight 'medium)
 doom-serif-font (font-spec :family "PragmataPro Mono Liga")
 ;; doom-serif-font (font-spec :family "PragmataPro Mono Liga" :size 20 :weight 'medium :slant 'normal)
 ;; doom-unicode-font (font-spec :family "PragmataPro Mono Liga")
 ;; doom-big-font (font-spec :family "PragmataPro Mono Liga" :size 28 :weight 'medium :slant 'normal)
 doom-font-increment 1)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
;; (setq doom-theme 'modus-vivendi)
(setq doom-theme
      ;; (if (string-prefix-p "Dark" (shell-command-to-string "defaults read -globalDomain AppleInterfaceStyle"))
      ;;   'modus-vivendi 'modus-operandi)
      (if (and
           (eq system-type 'darwin)
           (string-prefix-p "Dark"
                            (shell-command-to-string "defaults read -globalDomain AppleInterfaceStyle")))
          'ef-cherie 'ef-day))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory (expand-file-name "~/Dropbox/ORG/"))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; (setq debug-on-message "Wrong type argument: listp")

;; Fix: evil-get-auxiliary-keymap returns temporary composed keymaps when both
;; a keymap and its parent have [STATE-state] entries. Emacs's lookup-key creates
;; a fresh merged keymap each call, so evil-define-key* writes to ephemeral objects
;; and bindings are silently lost. Walk the keymap directly instead.
(defun +evil-get-direct-auxiliary-keymap-a (orig-fn map state &optional create ignore-parent)
  "Return the direct aux-map, not a composed one from lookup-key."
  (if (and create (keymapp map) (keymap-parent map))
      (let* ((state-sym (intern (format "%s-state" state)))
             (direct-aux (let ((cell (cdr map)) found)
                           (while (and cell (consp cell) (not (keymapp cell)))
                             (when (and (consp (car cell))
                                        (eq (caar cell) state-sym)
                                        (keymapp (cdar cell)))
                               (setq found (cdar cell)))
                             (setq cell (cdr cell)))
                           found)))
        (if (and direct-aux (evil-auxiliary-keymap-p direct-aux))
            direct-aux
          (evil-set-auxiliary-keymap map state)))
    (funcall orig-fn map state create ignore-parent)))
(advice-add 'evil-get-auxiliary-keymap :around #'+evil-get-direct-auxiliary-keymap-a)

;; Safety net: ensure org localleader bindings exist when entering org-mode.
;; Re-applies Doom's org keybindings to the direct aux-map if missing.
(defun +org-ensure-localleader-bindings-h ()
  "Verify org localleader bindings and re-apply if missing."
  (when (and (eq major-mode 'org-mode)
             (boundp 'org-mode-map)
             (fboundp '+org-init-keybinds-h))
    (let ((aux (let ((cell (cdr org-mode-map)) found)
                 (while (and cell (consp cell) (not (keymapp cell)))
                   (when (and (consp (car cell))
                              (eq (caar cell) 'normal-state)
                              (keymapp (cdar cell)))
                     (setq found (cdar cell)))
                   (setq cell (cdr cell)))
                 found)))
      (unless (and aux (lookup-key aux ",t"))
        (+org-init-keybinds-h)))))
(add-hook 'org-mode-hook #'+org-ensure-localleader-bindings-h 90)

(load! "clj-elisp.el")
(load! "helper.el")
(load! "not-secret.el" (expand-file-name "~/Dropbox/sync/") t)
(load! "map.el")
(load! "better-default.el")
(load! "llm.el")
(load! "evil.el")
(load! "navigation.el")
(load! "prog.el")
(load! "completion.el")
(load! "version-control.el")
(load! "lisp.el")

(load! "elisp.el")
(load! "clojure.el")
(load! "js.el")

(load! "lang.el")
(load! "golang.el")
(load! "clisp.el")
(load! "org.el")
(load! "write.el")
(load! "visual.el")
(load! "read.el")
(load! "mail.el")
(load! "slack.el")
(load! "orun.el")
;; tmp fix for +fold--ensure-hideshow-mode not defined err
(load! "~/.emacs.d/modules/editor/fold/autoload/fold.el")
;; (load! "embr.el")
(load! "pragmatapro-prettify-symbols.el")
(load! "ai-behaviors.el")
