#!/usr/bin/env bb
;; Scratch file for verifying the pr-review-loop hook fires. Safe to delete.

(defn chunk-indices
  "Return [start end) index pairs splitting `n` items into chunks of `size`."
  [n size]
  (loop [start 0
         acc   []]
    (if (>= start n)
      acc
      (recur (+ start size)
             (conj acc [start (+ start size)])))))

(defn percent
  "Format `part` of `whole` as a percentage string."
  [part whole]
  (str (int (* 100 (/ part whole))) "%"))

(println (chunk-indices 10 3))
(println (percent 1 3))
