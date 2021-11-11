(ns lib.db.core
  (:require
   [babashka.pods :as pods]))

(pods/load-pod "dtlv")
(require '[pod.huahaiy.datalevin :as d])

(def schema {:const/id {:db/unique :db.unique/identity}})
(def conn (d/get-conn (str (System/getenv "HOME") "/.personal_db")))

(defn t [txs] (d/transact! conn txs))
(defn q [query & args] (apply d/q query (d/db conn) args))
(defn p [& args] (apply d/pull (d/db conn) args))
(defn close [] (d/close conn))
