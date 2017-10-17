;;; el.el --- Easy shell command that can interact with emacs. -*- lexical-binding: t -*-

;; Copyright (C) 2015-2017  Free Software Foundation, Inc.

;; Author: yqrashawn <namy.19@gmail.com>
;; URL: https://github.com/yqrashawn/el.el
;; Version: 0.0.1
;; Keywords: shell

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This package gives an overview of the current regex search

;;; Code:

(diredp-make-find-file-keys-reuse-dirs)
(defun buffer-string* (buffer)
  (with-current-buffer buffer
    (buffer-string)))

(defun el-shell (command func)
  "easily call function with shell out put"
  (interactive "P")
  (progn
    (defvar fasd-buffer "*fasd-buffer*")
    (shell-command command fasd-buffer)
    (funcall func (buffer-string* fasd-buffer))
    (diredp-find-file-reuse-dir-buffer)))

(defun el-fasd (str)
  "fasd dired to the directory returned by fasd"
  (interactive "P")
  (el-shell (concat  "fasd " str) #'dired))


(defun evil-ex-fasd-eval (orign-func str)
  "docstring"
  (interactive "P")
  (if (string-prefix-p "j " str)
      (el-fasd (string-remove-prefix "j " str))
    (orign-func str)
    ))

(advice-add 'evil-ex-execute :around 'evil-ex-fasd-eval)
;; (evil-ex-execute "j lmv")


(defun circe-network-connected-p (network)
  "Return non-nil if there's any Circe server-buffer whose
`circe-server-netwok' is NETWORK."
  (catch 'return
    (dolist (buffer (circe-server-buffers))
      (with-current-buffer buffer
        (if (string= network circe-server-network)
            (throw 'return t))))))

(defun circe-maybe-connect (network)
  "Connect to NETWORK, but ask user for confirmation if it's
already been connected to."
  (interactive "sNetwork: ")
  (if (or (not (circe-network-connected-p network))
          (y-or-n-p (format "Already connected to %s, reconnect?" network)))
      (circe network)))

(defun irc ()
  "Connect to IRC"
  (interactive)
  (circe-maybe-connect "Freenode"))

(setq circe-reduce-lurker-spam t)
(setq circe-use-cycle-completion t)
;; (circe-set-display-handler "JOIN" (lambda (&rest ignored) nil))
(setq circe-format-server-topic "*** Topic change by {userhost}: {topic-diff}")
(add-hook 'circe-chat-mode-hook 'my-circe-prompt)

(setq circe-format-say "{nick:-3s} {body}")

(eval-after-load 'circe
  '(defun lui-irc-propertize (&rest args)))

(defun my-circe-prompt ()
  (lui-set-prompt
   (concat (propertize (concat (buffer-name) "-->")
                       'face 'circe-prompt-face)
           " ")))

(setq lui-flyspell-p t
      lui-flyspell-alist '((".*" "american")))

(setq
 lui-time-stamp-position 'right-margin
 lui-time-stamp-format "%H:%M")

(add-hook 'lui-mode-hook 'my-circe-set-margin)
(defun my-circe-set-margin ()
  (setq right-margin-width 5))

(setq lui-fill-type nil)

(add-hook 'lui-mode-hook 'my-lui-setup)

(defun my-lui-setup ()
  (setq
   fringes-outside-margins t
   right-margin-width 5
   word-wrap t
   wrap-prefix "   "))

(setq lui-track-bar-behavior 'before-switch-to-buffer)
(setq tracking-ignored-buffers '(("#spacemacs$" circe-highlight-nick-face)))
(enable-circe-notifications)
(enable-lui-track-bar)

(defun my-fetch-password (&rest params)
  (require 'auth-source)
  (let ((match (car (apply 'auth-source-search params))))
    (if match
        (let ((secret (plist-get match :secret)))
          (if (functionp secret)
              (funcall secret)
            secret))
      (error "Password not found for %S" params))))

(defun my-freenode-password (server)
  (my-fetch-password :user "yqrashawn" :host "irc.freenode.net"))

(setq circe-network-options
      '(("Freenode"
         :host "irc.freenode.net"
         :port (6667 . 6697)
         :tls t
         :nick "yqrashawn"
         :sasl-username "yqrashawn"
         :sasl-password my-freenode-password
         :channels ("#emacs")
         )
        ("Gitter"
         :host "irc.gitter.im"
         :port 6697
         :tls t
         :nick "yqrashawn"
         :sasl-username "yqrashawn"
         :sasl-password gitter-token
         :channels ("#syl20bnr/spacemacs")
         )))
