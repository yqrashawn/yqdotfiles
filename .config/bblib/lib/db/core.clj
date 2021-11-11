(ns lib.db.core
  (:require
   [babashka.pods :as pods]))

(pods/load-pod "dtlv")
(require '[pod.huahaiy.datalevin :as d])

(defonce conn (d/get-conn "~/.personal-db"))

(defn t [txs] (d/transact! conn txs))
(defn q [query & args]
  (apply d/q query (d/db conn) args))
(def p (partial d/pull (d/db conn)))

(def close [] (d/close conn))
