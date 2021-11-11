(ns lib.github.notification
  (:require
   [lib.db.core :as db]
   [cheshire.core :as json]
   [lib.github.core :as gh])
  (:import [java.time Instant ZonedDateTime ZoneId]
           [java.time.format DateTimeFormatter]))

(defn- get-last-modified-header []
  (let [last-fetch (db/q '[:find ?time .
                           :where
                           [[:const/id :github/last-time-fetch-notification] ?time]])]
    (if last-fetch
      {"If-Modified-Since" last-fetch}
      {})))

(defn fetch
  ([] (fetch {}))
  ([opts]
   (let [opts (merge-with merge {:headers (get-last-modified-header)} opts)
         {:keys [body headers status]}
         (-> (gh/cget "/notifications" opts)
             deref)]
     (if (not= status 200) []
         (let [{:keys [last-modified]} headers]
           (when last-modified
             (db/t [{:const/id :github/notification-last-modified-time :const/value last-modified}]))
           body)))))

(comment
  (fetch))
