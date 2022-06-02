(ns lib.github.tag
  (:require
   [cheshire.core :as json]
   [lib.github.core :as gh]))

(defn tags [owner repo]
  (-> (str "/repos/" owner "/" repo "/tags")
      gh/cget
      deref
      :body
      (json/parse-string true)))

(comment
  (tags "Conflux-Chain" "helios"))
