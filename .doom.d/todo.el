;;; todo.el -*- lexical-binding: t; -*-

(require 'ht)
(require 'request)

(defvar tabulated-list-format)
(defvar tabulated-list-entries)
(defvar tabulated-list-sort-key)

(defvar-keymap todo-mode-map
  "C-c C-e" #'identity)

(define-derived-mode todo-mode tabulated-list-mode "Todo"
  "Major mode for managing todos within Emacs"
  (setq tabulated-list-format [("Name" 35 t)
                               ("ID" 7 t)
                               ("Status" 7 t)
                               ;; ("Buffer" 25 t)
                               ])
  ;; (make-local-variable 'process-menu-query-only)
  (setq tabulated-list-sort-key (cons "Name" nil))
  (add-hook 'tabulated-list-revert-hook 'todo--refresh nil t))

(defun todo-list ()
  (interactive)
  (setq buffer (get-buffer-create "Todo"))
  (with-current-buffer buffer
    (todo-mode)
    (todo--refresh)
    (tabulated-list-print))
  (pop-to-buffer buffer))

(defun todo--refresh ()
  (setq tabulated-list-entries nil)
  (setq tabulated-list-entries (list
                                (list 1 (vector "NAMEEEEEEEEEEEEEEEEEEEEEEEEEEE" "IDDDDDDDDDDDDDDD" "STATUSSSSS"))
                                (list 2 (vector "NAMEEEEEEEEEEEEEEEEEEEEEEEEEEE" "IDDDDDDDDDDDDDDD" "STATUSSSSS"))
                                (list 3 (vector "NAMEEEEEEEEEEEEEEEEEEEEEEEEEEE" "IDDDDDDDDDDDDDDD" "STATUSSSSS"))
                                (list 4 (vector "NAMEEEEEEEEEEEEEEEEEEEEEEEEEEE" "IDDDDDDDDDDDDDDD" "STATUSSSSS"))
                                (list 5 (vector "NAMEEEEEEEEEEEEEEEEEEEEEEEEEEE" "IDDDDDDDDDDDDDDD" "STATUSSSSS"))
                                (list 6 (vector "NAMEEEEEEEEEEEEEEEEEEEEEEEEEEE" "IDDDDDDDDDDDDDDD" "STATUSSSSS"))
                                (list 7 (vector "NAMEEEEEEEEEEEEEEEEEEEEEEEEEEE" "IDDDDDDDDDDDDDDD" "STATUSSSSS"))
                                (list 8 (vector "NAMEEEEEEEEEEEEEEEEEEEEEEEEEEE" "IDDDDDDDDDDDDDDD" "STATUSSSSS")))))


(defun todo--query (cb)
  (request "http://localhost:6998/api/query"
    :type "POST"
    :sync t
    :headers '(("Content-Type" . "application/json")
               ("Accept" . "application/json"))
    :data (json-encode
           (list (cons "query" (format "%s" '(db/q
                                              '[:find (pull tx [*])
                                                :where
                                                [tx :gh.notification/unread?]])))))
    :parser (lambda () (json-parse-string (buffer-string)))
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (cb (ht-get data "body"))))))
