(ns cljs-helper
  (:require
   [cljs.repl :as repl]
   [clojure.data.json :as json]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as string]
   [shadow.build.data]
   [shadow.cljs.devtools.api :as sdapi]))

(declare get-build-runtimes cljs-eval get-build-default-runtime-id
         resolve-runtime)

;;; utilities
;;;; checks
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
  (let [runtime-id (resolve-runtime build-id runtime-id)
        {:keys [runtimes default-runtime-id]}
        (-> build-id
            sdapi/get-worker
            :state-ref
            deref)
        runtime (get runtimes runtime-id)]
    (when-not runtime
      (throw (ex-info (format "runtime %s is not valid" runtime-id)
                      {:build-id           build-id
                       :default-runtime-id default-runtime-id
                       :valid-runtimes
                       (->> runtimes vals (into #{}))})))
    (when-not (runtime-active? runtime)
      (throw (ex-info (format "runtime %s is inactive" runtime-id)
                      {:build-id           build-id
                       :default-runtime-id default-runtime-id
                       :active-runtimes
                       (->> runtimes
                            (filter runtime-active?)
                            keys
                            (into #{}))})))))

;;;; get data
(defn resolve-runtime [build-id runtime-id]
  (if (= runtime-id -1)
    (get-build-default-runtime-id build-id)
    runtime-id))

(defn get-runtime-href [build-id runtime-id]
  (-> (cljs-eval
       build-id runtime-id "js/location.href" "cljs.user")
      :clj-read-string-results
      first))

(defn format-runtime [{:keys [build-id client-id] :as runtime}]
  (-> runtime
      (set/rename-keys {:client-id :runtime-id
                        :since     :alive-since
                        :host      :host-type})
      (select-keys #{:runtime-id :dom :connection-info
                     :alive-since :build-id :user-agent :host-type})
      (assoc :window.location.href
             (get-runtime-href build-id client-id))))

(defn get-build-state [build-id]
  (-> build-id
      sdapi/get-worker
      :state-ref
      deref))

(comment
  (get-build-state :ground))

(defn get-build-runtimes [build-id]
  (-> build-id
      get-build-state
      :runtimes))

(defn get-build-default-runtime-id [build-id]
  (-> build-id
      get-build-state
      :default-runtime-id))

(defn format-build [build-id]
  (let [build-state (get-build-state build-id)]
    (-> build-state
        (select-keys #{:autobuild :runtimes :build-id})
        (set/rename-keys {:autobuild :auto-build-on-file-change})
        (update :runtimes
                #(->> %
                      vals
                      (mapv format-runtime))))))

;;; cljs eval
(defn cljs-eval
  ([build-id runtime-id code]
   (cljs-eval build-id runtime-id code "cljs.user"))
  ([build-id runtime-id code ns]
   (ensure-runtime build-id runtime-id)
   (let [runtime-id (resolve-runtime build-id runtime-id)
         {:keys [results] :as rst}
         (sdapi/cljs-eval
          build-id code
          {:runtime-id runtime-id
           :ns         (symbol (or ns "cljs.user"))})]
     (-> rst
         (assoc :clj-read-string-results
                (mapv
                 (fn [r] (try
                           (read-string r)
                           (catch Exception _)))
                 results))))))

(comment
  (cljs-eval :ground 60 "js/location.haha.d")
  (cljs-eval :ground 10 "js/location.href" nil))

;;; get shadow-cljs info
(defn get-shadow-cljs-info
  "get info llm cares about of the whole shadow-cljs runtime"
  []
  (let [active-builds (sdapi/active-builds)]
    {:builds          (mapv format-build active-builds)
     :active-builds   active-builds
     :inactive-builds (->> (sdapi/get-build-ids)
                           (remove active-builds)
                           (into #{}))}))

(comment
  (get-shadow-cljs-info))

;;; get build namespace
(defn get-build-namespaces
  "build available ns of a build, filter by re"
  ([build-id] (get-build-namespaces build-id nil))
  ([build-id re]
   (ensure-build build-id)
   (let [namespaces
         (->> (-> build-id
                  sdapi/get-worker
                  :state-ref
                  deref
                  :build-state
                  :compiler-env
                  :cljs.analyzer/namespaces
                  keys)
              (mapv name))
         filter-fn (if (string? re) (partial re-find (re-pattern re)) identity)]
     (->> namespaces
          (filter filter-fn)
          (into #{})))))

(comment
  (get-build-namespaces :ground)
  (get-build-namespaces :ground "user"))

;;; get symbol source code
(defn get-symbol-source-code
  "get source code for symbol (def, defn, defmacro ...)"
  [build-id runtime-id sym-name sym-ns]
  (let [runtime-id (resolve-runtime build-id runtime-id)
        rst
        (-> (cljs-eval
             build-id
             runtime-id
             (format
              "(clojure.string/trim-newline (with-out-str (cljs.repl/source %s)))"
              sym-name)
             sym-ns)
            :clj-read-string-results
            first
            (string/trim)
            (string/trim-newline))
        no-source? (re-find #"(?i)source not found" rst)]
    (if no-source?
      (format "No source found for symbol \"%s\" in namespace \"%s\"" sym-name sym-ns)
      rst)))

(comment
  (get-symbol-source-code :ground -1 "init" "ground.core")
  (get-symbol-source-code :ground -1 "initt" "ground.core"))

;;; get symbol doc
(defn get-symbol-doc
  "get doc for symbol, (def, defn, defmacro ...)"
  [build-id runtime-id sym-name sym-ns]
  (let [runtime-id (resolve-runtime build-id runtime-id)
        rst
        (-> (cljs-eval
             build-id runtime-id
             (format
              "(clojure.string/trim-newline (with-out-str (cljs.repl/doc %s)))"
              sym-name)
             sym-ns)
            :clj-read-string-results
            first
            (string/trim)
            (string/trim-newline)
            (string/replace-first "-------------------------\n" ""))
        no-source? (string/blank? rst)]
    (if no-source?
      (format "No symbol \"%s\" found in namespace \"%s\"" sym-name sym-ns)
      rst)))

(comment
  (get-symbol-doc :ground -1 "defn" "cljs.user")
  (get-symbol-doc :ground -1 "init" "ground.core")
  (get-symbol-doc :ground -1 "initt" "ground.core"))

(comment
  (-> (cljs-eval
       :ground
       -1
       "(all-ns)")
      :clj-read-string-results)
  (cljs-eval
   :ground
   -1
   "(+ 1 1)" "ground.core")

  ;; Existing tests
  (get-build-runtimes :ground)
  (get-build-namespaces :ground "user")
  (slurp (io/resource "portfolio/ui/canvas/protocols.cljs"))
  (slurp (io/resource "ground/core.cljs"))
  (-> :ground
      sdapi/get-worker
      :state-ref
      deref)
  (-> :ground
      sdapi/get-worker
      :state-ref
      deref
      :build-state
      :compiler-env
      :cljs.analyzer/namespaces
      (get 'ground.core))
  (-> :ground
      sdapi/get-worker
      :state-ref
      deref
      :build-state
      :compiler-env
      :cljs.analyzer/namespaces)

  (sdapi/watch-compile! :ground)

  (get-shadow-cljs-info)
  ;; => {:active-builds #{:app :ground}
  ;;     :builds [{:auto-build-on-file-change true
  ;;               :build-id :ground
  ;;               :runtimes [{:alive-since #inst "2025-09-16T04:01:32.266-00:00"
  ;;                           :build-id :ground
  ;;                           :connection-info {:remote true :websocket true}
  ;;                           :dom true
  ;;                           :host-type :browser
  ;;                           :runtime-id 10
  ;;                           :user-agent "Firefox 141.0 [MacIntel]"
  ;;                           :window.location.href "http://localhost:3202/?id=ground.core%2Fbutton"}]}
  ;;              {:auto-build-on-file-change true :build-id :app :runtimes []}]
  ;;     :inactive-builds #{:npm}}
  )
