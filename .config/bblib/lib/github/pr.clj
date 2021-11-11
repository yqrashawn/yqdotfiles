(ns lib.github.pr
  (:require
   [cheshire.core :as json]
   [lib.github.core :as gh]))

(defn my-open-pr [owner repo]
  (-> (str "/repos/" owner "/" repo "/pulls")
      (gh/cget {:query-params {:state :closed}})
      deref
      :body
      (json/parse-string true)))