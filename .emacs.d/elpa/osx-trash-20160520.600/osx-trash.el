;;; osx-trash.el --- System trash for OS X           -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2016  Sebastian Wiesner <swiesner@lunaryorn.com>

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; Version: 0.3-cvs
;; URL: https://github.com/lunaryorn/osx-trash.el
;; Keywords: files, convenience, tools, unix
;; Package-Requires: ((emacs "24.1"))

;; This file is not part of GNU Emacs.

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

;; Add support for system trash on OS X.  In other words, make
;; `delete-by-moving-to-trash' do what you expect it to do.
;;
;; Emacs does not support the system trash of OS X.  `system-move-file-to-trash'
;; is not defined.  This library provides `osx-trash-move-file-to-trash' as an
;; implementation of `system-move-file-to-trash' for OS X.
;;
;; `osx-trash-move-file-to-trash' tries to use `trash' utility from
;; https://github.com/ali-rantakari/trash (brew install trash).  If `trash' is
;; not available, the script falls back to an AppleScript helper which trashes
;; the file via finder.  `trash' is generally preferred, because AppleScript is
;; slow.
;;
;; To enable, call `osx-trash-setup' and set `delete-by-moving-to-trash' to a
;; non-nil value.

;;; Code:

(defconst osx-trash-pkg-file
  (expand-file-name (if load-in-progress load-file-name (buffer-file-name)))
  "The absolute path to this file.")

(defconst osx-trash-pkg-dir
  (file-name-directory osx-trash-pkg-file)
  "The absolute path to the directory of this package.")

(defconst osx-trash-script-file
  (expand-file-name "trashfile.AppleScript" osx-trash-pkg-dir))

(defun osx-trash-move-file-to-trash (file-name)
  "Move FILE-NAME to trash.

Try to call the `trash' utility first, because it's faster, and
fall back to AppleScript if `trash' wasn't found."
  (let ((file-name (expand-file-name file-name)))
    (with-temp-buffer
      (let ((retcode (condition-case nil
                         (call-process "trash" nil t nil file-name)
                       (file-error
                        (call-process "osascript" nil t nil
                                      osx-trash-script-file file-name)))))
        (unless (equal retcode 0)
          (error "Failed to trash %S: %S" file-name (buffer-string)))))))

;;;###autoload
(defun osx-trash-setup ()
  "Provide trash support for OS X.

Provide `system-move-file-to-trash' as an alias for
`osx-trash-move-file-to-trash'.

Note that you still need to set `delete-by-moving-to-trash' to a
non-nil value to enable trashing for file operations."
  (when (and (eq system-type 'darwin)
             (not (fboundp 'system-move-file-to-trash)))
    (defalias 'system-move-file-to-trash
      'osx-trash-move-file-to-trash)))

(provide 'osx-trash)
;;; osx-trash.el ends here
