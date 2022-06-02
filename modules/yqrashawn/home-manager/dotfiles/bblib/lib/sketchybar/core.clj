#!/run/current-system/sw/bin/bb

(ns lib.sketchybar.core
  (:require
   ;; [cheshire.core :as json]
   [clojure.java.shell :refer [sh]]))

(defn m [& args]
  (let [args                   (mapv #(if (keyword? %) (name %) (str %)) args)
        {:keys [exit out err]} (apply sh "/usr/local/bin/sketchybar" "-m" args)
        ;; out                    (and out (json/parse-string out keyword))
        ]
    (if (= exit 0)
      out
      (throw (java.lang.Exception. err)))))
