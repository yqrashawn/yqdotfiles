;;; buffer-flip.el --- Use key-chord to cycle through buffers like Alt-Tab in Windows
;; Copyright (C) 2015 Russell Black

;; Author: Russell Black (killdash9@github)
;; Keywords: convenience
;; Package-Version: 20160811.713
;; URL: https://github.com/killdash9/buffer-flip.el
;; Created: 10th November 2015
;; Version: 1.2
;; Package-Requires: ((key-chord "20150808"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Inspired by Alt-Tab.  Quickly flip through recently-used buffers.

;;; Code:
(eval-when-compile (require 'cl))
(require 'key-chord)

(defvar buffer-flip-mode-map '(keymap)
  "The mode map for `buffer-flip-mode'.")

;;;###autoload
(define-minor-mode buffer-flip-mode
  "A global minor mode that streamlines the operation of
switching between recent buffers, with an emphasis on minimizing
keystrokes.  Inspired by the Alt-Tab convention in Windows, it
keeps the most recently used buffers on the top of the stack.
Depends on `key-chord-mode'.

By default, the key chord to begin flipping through buffers is
\"u8\".  You can customize these keys with the variable
`buffer-flip-keys'.

\"u\" and \"8\" are roughly analogous to Alt and Tab,
respectively.  To begin cycling through the buffers, press u and
8 at the same time or in rapid succession, `key-chord' style.
This begins the flipping process by switching to the most
recently used buffer.  At this point, pressing \"8\" by itself
will continue to cycle through the buffer stack, more recent
buffers first.  Pressing * (shift-8 on an English keyboard) will
cycle in the opposite direction.  Just begin working in the
current buffer to stop cycling.  Doing so places the current
buffer on top of the stack.  C-g cancels cycling and restores the
buffer you were in before, analagous to Esc when cycling in
Windows."
  :global t :keymap buffer-flip-mode-map
  (when buffer-flip-mode
    (unless key-chord-mode
      (when (yes-or-no-p "key-chord-mode must be enabled for buffer-flip-mode \
to work.  Enable key-chord-mode?") (key-chord-mode 1)))))

(defun buffer-flip-set-keys (symbol value)
  "Set the variable `buffer-flip-keys'.
Called from variable customization.  SYMBOL is ignored, and VALUE
is a three-character string.  This function registers the first
two characters in VALUE as a key-chord for `buffer-flip'.  See
`buffer-flip-keys' for a full description of the VALUE string."
  (when (not (and (stringp value) (= 3 (length value))))
    (user-error "buffer-flip-keys must be a three character string"))
  (set-default 'buffer-flip-keys value)
  ;; empty the mode map to clear out previous bindings
  (setcdr buffer-flip-mode-map nil)
  (key-chord-define buffer-flip-mode-map (substring value 0 2) 'buffer-flip))

(defcustom buffer-flip-keys "u8*"
  "Keys for flipping through buffers.
The first two characters form the key-chord that begins buffer
cycling.  They are automatically registered with `key-chord` when
this variable is customized.  The second character pressed on its
own continues cycling in the forward direction.  The third
character cycles in the backward direction.  This would typically
be the shifted version of the second character.  These may not be
modifier keys, and because of a restriction in key-chord, they
must be characters between 32 and 126.  Choose a key combination
not likely to be used in succession in normal editing."
  :set 'buffer-flip-set-keys :type '(string) :group 'buffer-flip)

(defun buffer-flip ()
  "Flip to the next or previous buffer in the stack, depending on key pressed.
See `buffer-flip-mode' for more information."
  (interactive) ; ensure current buffer is on the top of the stack at outset
  (when (vectorp (this-command-keys)) (switch-to-buffer (current-buffer)))
  (switch-to-buffer              ; Switch to next/prev buffer in stack
   (do ((buf (current-buffer)    ; Starting with the current buffer
             (nth (mod (+ (cl-position buf (buffer-list)) ; Advance buf
                          (if (cl-find (elt buffer-flip-keys 2) ; Back key?
                                       (this-command-keys)) -1 1)) ; Fwd or back
                       (length (buffer-list))) (buffer-list)))) ; Mod to wrap
       ((not (or (get-buffer-window buf) ; not already visible 
                 (equal ? (elt (buffer-name buf) 0)))) buf)) t) ; not internal
  (set-transient-map                                   ; Read next key
   `(keymap (,(elt buffer-flip-keys 1) . buffer-flip)  ; Flip forward
            (,(elt buffer-flip-keys 2) . buffer-flip)) ; Flip backward
   t (lambda () (switch-to-buffer (if (cl-find 7 (this-command-keys)) ; Cancel?
                                 (car (buffer-list)) (current-buffer))))))

(provide 'buffer-flip)
;;; buffer-flip.el ends here
