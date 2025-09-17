(ns clj-helper
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(defn read-file-url
  ([file-url] (read-file-url file-url {}))
  ([file-url {:keys [limit offset] :or {limit 2000 offset 0}}]
   (try
     (let [content          (slurp file-url)
           lines            (string/split-lines content)
           total-lines      (count lines)
           start-line       (inc offset)
           end-line         (min total-lines (+ offset limit))
           selected-lines   (take limit (drop offset lines))
           selected-content (string/join "\n" selected-lines)]
       (str (format "[Total lines: %d]\n[Content lines: %d-%d]\n[File url: %s]\n"
                    total-lines start-line end-line file-url)
            "\n␂" selected-content "␃\n"))
     (catch Exception _
       (format "Failed to read file url: %s" file-url)))))
