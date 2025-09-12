;;; .nixpkgs/.doom.d/clj.el -*- lexical-binding: t; -*-

(require 'map)
(require 'dash)

;; context given to llm
;; seq.el, s.el, ht.el, ring.el, dash.el, map.el, queue.el, cl-lib.el
;; list-utils.el, treepy.el

;;; clj/get
(defun clj/get (coll key &optional default)
  "Get the value at KEY in COLL (alist, plist, hash-table, array, or sequence).
Return DEFAULT if KEY not found."
  (cond
   ((hash-table-p coll) (gethash key coll default))
   ((arrayp coll)
    (if (and (natnump key) (< key (length coll)))
        (aref coll key)
      default))
   ((and (listp coll) (not (null coll)))
    ;; Try plist
    (if (plist-get coll key)
        (plist-get coll key)
      ;; Else try alist
      (let ((cell (assoc key coll)))
        (if cell (cdr cell) default))))
   (t default)))

;;; clj/get-in
(defun clj/get-in (m ks &optional default)
  "Get a value in a nested associative structure, where KS is a list of keys."
  (let ((current m))
    (catch 'found
      (dolist (k ks)
        (setq current (clj/get current k))
        (when (null current)
          (throw 'found default)))
      current)))

;;; clj/assoc
(defun clj/assoc (m key val)
  "Associate KEY with VAL in M. Returns a new collection when possible."
  (cond
   ((hash-table-p m)
    (let ((copy (copy-hash-table m)))
      (puthash key val copy)
      copy))
   ((arrayp m)
    (let ((copy (copy-sequence m)))
      (when (and (natnump key) (< key (length copy)))
        (aset copy key val))
      copy))
   ((and (listp m) (not (null m)))
    ;; Try plist
    (if (plist-get m key)
        (let ((plist (copy-sequence m)))
          (plist-put plist key val))
      ;; Fallback: treat as alist
      (let ((alist (copy-tree m)))
        (if (assoc key alist)
            (setcdr (assoc key alist) val)
          (push (cons key val) alist))
        alist)))
   (t m)))

;;; clj/assoc-in
(defun clj/assoc-in (m ks v)
  "Associate a value V in a nested associative structure at path KS."
  (if (null ks)
      v
    (let ((k (car ks))
          (ks-rest (cdr ks)))
      (clj/assoc m k (clj/assoc-in (clj/get m k) ks-rest v)))))

;;; clj/update
(defun clj/update (m key f &rest args)
  "Update the value at KEY in M by applying F to it with ARGS."
  (let ((val (clj/get m key)))
    (clj/assoc m key (apply f val args))))

;;; clj/update-in
(defun clj/update-in (m ks f &rest args)
  "Update the value at path KS in M by applying F to it with ARGS."
  (if (null ks)
      (apply f m args)
    (let ((k (car ks))
          (ks-rest (cdr ks)))
      (clj/assoc m k (apply #'clj/update-in (clj/get m k) ks-rest f args)))))

;;; clj/dissoc
(defun clj/dissoc (m key)
  "Return a new collection with KEY removed from M."
  (cond
   ((hash-table-p m)
    (let ((copy (copy-hash-table m)))
      (remhash key copy)
      copy))
   ((arrayp m)
    (let ((copy (copy-sequence m)))
      (if (and (natnump key) (< key (length copy)))
          (aset copy key nil))
      copy))
   ((and (listp m) (not (null m)))
    ;; Try to treat as plist first
    (if (plist-get m key)
        (let* ((plist (copy-sequence m))
               (pos (cl-position key plist)))
          (if (and pos (= 0 (% pos 2)))
              (let* ((before (cl-subseq plist 0 pos))
                     (after (cl-subseq plist (+ pos 2))))
                (append before after))
            plist))
      ;; Else treat as alist
      (let ((alist (copy-tree m)))
        (delq nil (mapcar (lambda (cell)
                            (unless (and (consp cell)
                                         (equal (car cell) key))
                              cell))
                          alist)))))
   (t m)))

;;; clj/conj
(defun clj/conj (coll &rest items)
  "Add ITEM(s) to COLL, returning a new collection.
For lists: items are added to front (like `cons`), for vectors/arrays to the end,
for hash-tables/alists/plists: expects items as key-value pairs.
If multiple items, adds all in order."
  (cond
   ((null items) coll)
   ((listp coll)
    ;; For proper lists, add to front (like cons). Clojure's conj on list prepends.
    (if (and (not (null coll)) (consp (car items)) (not (arrayp (car items))))
        ;; If item looks like (key . val), treat as alist
        (append (reverse items) coll)
      ;; Regular list, prepend each item (reverse for leftmost first)
      (let ((result coll))
        (dolist (item (reverse items))
          (setq result (cons item result)))
        result)))
   ((vectorp coll)
    (apply #'vconcat coll items))
   ((arrayp coll)
    (apply #'vconcat coll items))
   ((hash-table-p coll)
    (let ((copy (copy-hash-table coll)))
      (while items
        (let ((k (pop items))
              (v (pop items)))
          (puthash k v copy)))
      copy))
   ;; For plists or alists, just append key-val pairs (user codes must ensure correct shape)
   ((and (listp coll) (plist-get coll (car items)))
    (append coll items))
   (t
    (error "clj/conj: unsupported collection type"))))

;;; clj/into
(defun clj/into (to from)
  "Return a new collection consisting of all elements of FROM added to TO.
TO and FROM can be list, vector, array, hash-table, plist, or alist.
TO's type determines the result's type."
  (cond
   ((listp to)
    (if (hash-table-p from)
        ;; Add key-value pairs as (k . v) conses
        (let ((result to))
          (maphash (lambda (k v) (setq result (cons (cons k v) result))) from)
          (nreverse result))
      (append to (if (listp from) from (append from nil)))))
   ((vectorp to)
    (vconcat to (if (vectorp from) from (apply 'vector (if (listp from) from (append from nil))))))
   ((arrayp to)
    (vconcat to (if (arrayp from) from (apply 'vector (if (listp from) from (append from nil))))))
   ((hash-table-p to)
    (let ((copy (copy-hash-table to)))
      (cond
       ((hash-table-p from)
        (maphash (lambda (k v) (puthash k v copy)) from))
       ((listp from)
        (dolist (elt from)
          (cond
           ((consp elt) (puthash (car elt) (cdr elt) copy))
           ((and (symbolp elt) (plist-get from elt))
            (puthash elt (plist-get from elt) copy)))))
       ((arrayp from)
        (dotimes (i (length from)) (puthash i (aref from i) copy))))
      copy))
   (t
    (error "clj/into: unsupported collection type"))))

;;; clj/merge
(defun clj/merge (&rest maps)
  "Merge MAPS (hash-tables, plists, alists, arrays) into a new collection.
For overlapping keys, the rightmost map's value is used.
The type of the first map determines the result."
  (when (null maps)
    (error "clj/merge: At least one map required"))
  (let ((result (copy-tree (car maps))))
    (dolist (m (cdr maps))
      (setq result
            (cond
             ((hash-table-p result)
              (let ((copy (copy-hash-table result)))
                (cond
                 ((hash-table-p m)
                  (maphash (lambda (k v) (puthash k v copy)) m))
                 ((and (listp m) (not (null m)))
                  (if (plist-get m (car m))
                      (let ((plist m))
                        (while plist
                          (puthash (pop plist) (pop plist) copy)))
                    (dolist (cell m)
                      (when (consp cell)
                        (puthash (car cell) (cdr cell) copy))))))
                copy))
             ((arrayp result)
              (let ((copy (copy-sequence result)))
                (cond
                 ((arrayp m)
                  (let ((len (min (length copy) (length m))))
                    (dotimes (i len)
                      (aset copy i (aref m i)))))
                 ((hash-table-p m)
                  (maphash (lambda (i v)
                             (when (and (natnump i) (< i (length copy)))
                               (aset copy i v))) m)))
                copy))
             ((and (listp result) (plist-get result (car result)))
              ;; plist
              (let* ((plist (copy-sequence result))
                     (pl m))
                (when (listp pl)
                  (while pl
                    (setq plist (plist-put plist (pop pl) (pop pl)))))
                plist))
             ((listp result)
              ;; alist
              (let ((alist (copy-tree result)))
                (when (listp m)
                  (dolist (cell m)
                    (when (consp cell)
                      (let ((a (assoc (car cell) alist)))
                        (if a
                            (setcdr a (cdr cell))
                          (push cell alist))))))
                alist))
             (t result))))
    result))

;;; clj/nested-merge
(defun clj/nested-merge (&rest maps)
  "Recursively merge MAPS (hash-tables, plists, alists, arrays).
If values are maps on the same key, merge recursively, else rightmost wins.
Result type follows the first map."
  (when (null maps)
    (error "clj/nested-merge: At least one map required"))
  (let ((result (copy-tree (car maps))))
    (dolist (m (cdr maps))
      (setq result
            (cond
             ((hash-table-p result)
              (let ((copy (copy-hash-table result)))
                (cond
                 ((hash-table-p m)
                  (maphash
                   (lambda (k v)
                     (let ((old (gethash k copy '__notfound)))
                       (puthash k
                                (if (and (not (eq old '__notfound))
                                         (or (hash-table-p old)
                                             (and (listp old) (or (plist-get old (car old)) (consp (car old))))))
                                    (clj/nested-merge old v)
                                  v)
                                copy)))
                   m))
                 ((and (listp m) (not (null m)))
                  (if (plist-get m (car m))
                      ;; treat as plist
                      (let ((plist m))
                        (while plist
                          (let ((k (pop plist))
                                (v (pop plist)))
                            (let ((old (gethash k copy '__notfound)))
                              (puthash k
                                       (if (and (not (eq old '__notfound))
                                                (or (hash-table-p old)
                                                    (and (listp old) (or (plist-get old (car old)) (consp (car old))))))
                                           (clj/nested-merge old v)
                                         v)
                                       copy))))))
                  ;; treat as alist
                  (dolist (cell m)
                    (when (consp cell)
                      (let ((k (car cell)) (v (cdr cell)))
                        (let ((old (gethash k copy '__notfound)))
                          (puthash k
                                   (if (and (not (eq old '__notfound))
                                            (or (hash-table-p old)
                                                (and (listp old) (or (plist-get old (car old)) (consp (car old))))))
                                       (clj/nested-merge old v)
                                     v)
                                   copy)))))))
                copy))
             ((arrayp result)
              (let ((copy (copy-sequence result)))
                (cond
                 ((arrayp m)
                  (let ((len (min (length copy) (length m))))
                    (dotimes (i len)
                      (let ((old (aref copy i))
                            (v (aref m i)))
                        (aset copy i
                              (if (and (or (hash-table-p old)
                                           (and (listp old) (or (plist-get old (car old)) (consp (car old)))))
                                       (or (hash-table-p v)
                                           (and (listp v) (or (plist-get v (car v)) (consp (car v))))))
                                  (clj/nested-merge old v)
                                v))))))
                 ((hash-table-p m)
                  (maphash (lambda (i v)
                             (when (and (natnump i) (< i (length copy)))
                               (let ((old (aref copy i)))
                                 (aset copy i
                                       (if (and (or (hash-table-p old)
                                                    (and (listp old) (or (plist-get old (car old)) (consp (car old)))))
                                                (or (hash-table-p v)
                                                    (and (listp v)
                                                         (or (plist-get v (car v)) (consp (car v))))))
                                           (clj/nested-merge old v)
                                         v)))))
                           m)))
                copy))
             ((and (listp result) (plist-get result (car result)))
              ;; plist
              (let* ((plist (copy-sequence result))
                     (pl m))
                (when (listp pl)
                  (while pl
                    (let* ((k (pop pl)) (v (pop pl))
                           (old (plist-get plist k)))
                      (setq plist (plist-put plist k
                                             (if (and old
                                                      (or (hash-table-p old)
                                                          (and (listp old) (or (plist-get old (car old)) (consp (car old)))))
                                                      (or (hash-table-p v)
                                                          (and (listp v) (or (plist-get v (car v)) (consp (car v))))))
                                                 (clj/nested-merge old v)
                                               v))))))
                plist))
             ((listp result)
              ;; alist
              (let ((alist (copy-tree result)))
                (when (listp m)
                  (dolist (cell m)
                    (when (consp cell)
                      (let ((k (car cell)) (v (cdr cell)))
                        (let ((a (assoc k alist)))
                          (if a
                              (setcdr a
                                      (if (and (consp a)
                                               (or (hash-table-p (cdr a))
                                                   (and (listp (cdr a)) (or (plist-get (cdr a) (car (cdr a))) (consp (car (cdr a)))))))
                                          (clj/nested-merge (cdr a) v)
                                        v))
                            (push cell alist)))))))
                alist))
             (t result))))
    result))
