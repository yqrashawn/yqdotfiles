(ns cljs-helper
  (:require
   [clojure.set :as set]
   [clojure.string :as string]
   [shadow.cljs.devtools.api :as sdapi]))

(declare get-build-runtimes cljs-eval)

(defn ensure-build [build-id]
  (let [all-builds (into #{} (sdapi/get-build-ids))]
    (when-not (all-builds build-id)
      (throw (ex-info (format "build %s is not valid" build-id)
                      {:valid-builds all-builds})))
    (when-not (sdapi/worker-running? build-id)
      (throw (ex-info (format "build %s is inactive" build-id)
                      {:active-builds (sdapi/active-builds)})))))

(defn runtime-active? [runtime]
  (get-in runtime [:connection-info :websocket]))

(defn ensure-runtime [build-id runtime-id]
  (ensure-build build-id)
  (let [runtimes (-> build-id
                     sdapi/get-worker
                     :state-ref
                     deref
                     :runtimes)
        runtime  (get runtimes runtime-id)]
    (when-not runtime
      (throw (ex-info (format "runtime %s is not valid" runtime-id)
                      {:build-id       build-id
                       :valid-runtimes (->> runtimes val (into #{}))})))
    (when-not (runtime-active? runtime)
      (throw (ex-info (format "runtime %s is inactive" runtime-id)
                      {:build-id       build-id
                       :active-runtimes
                       (->> runtimes
                            (filter runtime-active?)
                            keys
                            (into #{}))})))))

(defn cljs-eval
  ([build-id runtime-id code]
   (cljs-eval build-id runtime-id code "cljs.user"))
  ([build-id runtime-id code ns]
   (ensure-runtime build-id runtime-id)
   (sdapi/cljs-eval build-id code {:runtime-id runtime-id
                                   :ns         (symbol ns)})))

(comment
  (cljs-eval :ground 60 "js/location.haha.d"))

(defn get-runtime-href [build-id runtime-id]
  (-> (sdapi/cljs-eval build-id "js/location.href"
                       {:runtime-id runtime-id
                        :ns         'cljs.user})
      :results
      first
      (string/replace "\"" "")))

(defn format-runtime [{:keys [build-id client-id] :as runtime}]
  (-> runtime
      (set/rename-keys {:client-id :runtime-id
                        :since     :alive-since})
      (select-keys #{:runtime-id :dom :connection-info
                     :alive-since :build-id})
      (assoc :window.location.href
             (get-runtime-href build-id client-id))))

(defn get-build-runtimes [build-id]
  (->> (-> build-id
           sdapi/get-worker
           :state-ref
           deref
           :runtimes
           vals)
       (mapv format-runtime)))

(defn format-build [build-id]
  {:build-id build-id
   :runtimes (get-build-runtimes build-id)})

(defn get-shadow-cljs-info []
  (let [active-builds (sdapi/active-builds)]
    {:builds          (mapv format-build active-builds)
     :active-builds   active-builds
     :inactive-builds (->> (sdapi/get-build-ids)
                           (remove active-builds)
                           (into #{}))}))

(comment
  (get-shadow-cljs-info))

(comment
  (sdapi/watch-compile! :ground))
