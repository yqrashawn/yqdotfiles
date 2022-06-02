(ns lib.github.checks
  (:require
   [cheshire.core :as json]
   [lib.github.core :as gh]))

(defn runs [owner repo ref status]
  (-> (str "/repos/" owner "/" repo "/commits/" ref "/check-runs")
      (gh/cget {:query-params {:states status}})
      deref
      :body
      (json/parse-string true)))
