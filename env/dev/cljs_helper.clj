(ns cljs-helper
  (:require
   [shadow.cljs.devtools.api :as sdapi]))

(defn get-running-builds []
  (->>
   (-> (sdapi/get-runtime!)
       :supervisor
       :workers-ref
       deref
       keys)
   (into #{})))
