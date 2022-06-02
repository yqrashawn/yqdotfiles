#!/run/current-system/sw/bin/bb

(ns lib.yabai.core
  (:require
   [cheshire.core :as json]
   [clojure.java.shell :refer [sh]]))

(defn m [& args]
  (let [args                   (mapv #(if (keyword? %) (name %) (str %)) args)
        {:keys [exit out err]} (apply sh "yabai" "-m" args)
        out                    (and out (json/parse-string out keyword))]
    (if (= exit 0)
      out
      (throw (java.lang.Exception. err)))))

(defn cur-win [] (m :query :--windows :--window))
(defn cur-display [] (m :query :--displays :--display))
(defn space-win [s] (m :query :--windows :--space s))
