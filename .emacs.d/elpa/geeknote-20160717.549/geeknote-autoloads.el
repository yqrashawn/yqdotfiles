;;; geeknote-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "geeknote" "geeknote.el" (22581 12850 0 0))
;;; Generated autoloads from geeknote.el

(autoload 'geeknote-setup "geeknote" "\
Setup geeknote.

\(fn)" t nil)

(autoload 'geeknote-create "geeknote" "\
Create a new note with the given title.

TITLE the title of the new note to be created.

\(fn TITLE)" t nil)

(autoload 'geeknote-show "geeknote" "\
Open an existing note.

TITLE the title of the note to show.

\(fn TITLE)" t nil)

(autoload 'geeknote-edit "geeknote" "\
Open up an existing note for editing.

TITLE the title of the note to edit.

\(fn TITLE)" t nil)

(autoload 'geeknote-remove "geeknote" "\
Delete an existing note.

TITLE the title of the note to delete.

\(fn TITLE)" t nil)

(autoload 'geeknote-find "geeknote" "\
Search for a note with the given keyword.

KEYWORD the keyword to search the notes with.

\(fn KEYWORD)" t nil)

(autoload 'geeknote-tag-list "geeknote" "\
Show the list of existing tags in your Evernote.

\(fn)" t nil)

(autoload 'geeknote-notebook-list "geeknote" "\
Show the list of existing notebooks in your Evernote.

\(fn)" t nil)

(autoload 'geeknote--notebook-edit-with-oldtitle "geeknote" "\
Rename an existing notebook with a target.

TITLE the title of the notebook to rename.

\(fn OLDTITLE)" nil nil)

(autoload 'geeknote-notebook-edit "geeknote" "\
Rename an existing notebook.

TITLE the title of the notebook to rename.

\(fn OLDTITLE NEWTITLE)" t nil)

(autoload 'geeknote-user "geeknote" "\
Show information about active user.

\(fn)" t nil)

(autoload 'geeknote-move "geeknote" "\
Move a NOTE to a different NOTEBOOK.  If the provided NOTEBOOK is
non-existent, it will be created.

NOTE the title of the note to move.
NOTEBOOK the title of the notebook where NOTE should be moved.

\(fn NOTE NOTEBOOK)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; geeknote-autoloads.el ends here
