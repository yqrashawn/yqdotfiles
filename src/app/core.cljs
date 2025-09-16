(ns app.core
  (:require
   [app.config :as config]
   [app.styles :as styl]
   day8.re-frame.http-fx
   [goog.dom :as gdom]
   [re-frame.core :as rf]
   [reagent.dom.client :as rdc]))

(defn dev-setup []
  (when config/debug?
    (enable-console-print!)
    (println "dev mode")))

(defonce root (rdc/create-root (gdom/getElement "app")))

(defn mount-root []
  (rf/clear-subscription-cache!)
  (when config/debug?
    (styl/inject-trace-styles js/document))
  (rdc/render
   root
   [:div "root"]))

(defn ^:after-load re-render []
  (mount-root))

(defn ^:export init []
  (println "init again..")
  ;; (rf/dispatch-sync [:db/init])
  (dev-setup)
  (mount-root))
