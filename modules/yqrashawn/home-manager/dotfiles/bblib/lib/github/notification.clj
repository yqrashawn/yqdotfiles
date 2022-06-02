(ns lib.github.notification
  (:require
   [lib.db.core :as db]
   [cheshire.core :as json]
   [lib.github.core :as gh])
  (:import [java.time Instant ZonedDateTime ZoneId]
           [java.time.format DateTimeFormatter]))

(defn- get-last-modified-header []
  (let [last-fetch
        (try
          (db/q '[:find ?time .
                  :where
                  [[:const/id :github/last-time-fetch-notification] :const/value ?time]])
          (catch Exception e nil))]
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
             (db/t [{:const/id :github/last-time-fetch-notification :const/value last-modified}]))
           body)))))

(comment
  (db/t [[:db.fn/retractEntity [:const/id :github/last-time-fetch-notification]]])
  (fetch {:query-params {:participating true :all true :per_page 100}}))
