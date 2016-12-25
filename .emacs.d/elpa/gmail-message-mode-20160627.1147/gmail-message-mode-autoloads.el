;;; gmail-message-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "gmail-message-mode" "gmail-message-mode.el"
;;;;;;  (22618 11161 0 0))
;;; Generated autoloads from gmail-message-mode.el

(defvar gmm/auto-mode-list `("[\\\\/]\\(inbox\\|mail\\)-google-com.*\\.\\(ckr\\|html?\\|txt\\)\\'" "[\\\\/]itsalltext[\\\\/]\\(inbox\\|mail\\)\\.google\\..*\\.txt\\'" "[\\\\/]pentadactyl\\.\\(inbox\\|mail\\)\\.google\\..*\\.txt\\'") "\
List of regexps which will be added to `auto-mode-alist' (associated to `gmail-message-mode').

If the file path matches any of these, `gmail-message-mode' will be
activated on the current file.

If you don't want `gmail-message-mode' to add itself to your
`auto-mode-alist' simply set this variable to nil.

If you add items manually (not through the customization
interface), you'll need to call `gmm/set-amlist' for it
to take effect.
Removing items only takes effect after restarting Emacs.")

(custom-autoload 'gmm/auto-mode-list "gmail-message-mode" nil)

(autoload 'gmail-message-mode "gmail-message-mode" "\
Designed for GMail messages. Transparently edit an html file using markdown.

When this mode is activated in an html file, the buffer is
converted to markdown and you may edit at will, but the file is
still saved as html behind the scenes.
\\<gmail-message-mode-map>
Also defines a key \\[gmm/save-finish-suspend] for `gmm/save-finish-suspend'.

\\{gmail-message-mode-map}

\(fn)" t nil)

(autoload 'gmail-message-edit-server-mode "gmail-message-mode" "\
Designed for GMail messages coming from google-chrome's \"Edit with Emacs\".

Not actually meant for editing. This just sets up the buffer as a
mirrored version of an html file that you'll be editing with the
actual `gmail-message-mode'.

This is supposed to be added to `edit-server-url-major-mode-alist',
so that it's called in an edit-server buffer. If you're trying to
use this in any other way, you're probably using the wrong
function. Try using (or extending) `gmail-message-mode' instead.

\(fn)" t nil)

(autoload 'gmail-message-client-mode "gmail-message-mode" "\
Designed for GMail messages coming from google-chrome's \"Edit with Emacs\".

This mode is meant for editing, it is the sister of
`gmail-message-edit-server-mode', which is not meant for editing.
It works exactly as the simpler `gmail-message-mode', except that
saving or killing this buffer also affects the edit-server's
buffer (which is the mirror of this one).

This is supposed to be added to `auto-mode-alist', so that it's
called when we open mirror files setup by
`gmail-message-edit-server-mode'. If you're trying to use this in
any other way, you're probably using the wrong function. Try
using (or extending) `gmail-message-mode' instead.

\(fn)" t nil)

(autoload 'gmm/set-amlist "gmail-message-mode" "\
Reset the auto-mode-alist.

\(fn &optional SYM VAL)" nil nil)

(eval-after-load 'edit-server '(add-to-list 'edit-server-url-major-mode-alist '("\\(mail\\|inbox\\)\\.google\\." . gmail-message-edit-server-mode)))

(mapc (lambda (x) (add-to-list 'auto-mode-alist (cons x 'gmail-message-mode))) gmm/auto-mode-list)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; gmail-message-mode-autoloads.el ends here
