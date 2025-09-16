(ns app.styles
  (:require
   [app.config :as config]
   [goog.dom :as gdom]
   [goog.string :as gstr]
      ;;[garden.core :as g]
   ))

(def styles
  [])

(defn inject-node! [old-node new-node document]
  (if old-node
    (gdom/replaceNode new-node old-node)
    (gdom/appendChild (.-head document) new-node)))

(defn inject-inline-style [document id style]
  (let [old-style (gdom/getElement  id)
        new-style (gdom/createDom "style"
                                  (clj->js {:type "text/css"
                                            :id   id})
                                  style)]

    (inject-node! old-style new-style document)))

(defn inject-inline-link [document id link]
  (let [old-link (gdom/getElement id)
        new-link (gdom/createDom "link"
                                 (clj->js {:id   id
                                           :rel  :stylesheet
                                           :href link}))]

    (inject-node! old-link new-link document)))

(defn inject-trace-styles [document]
  ;;(inject-inline-style document "--reframe-template--" (apply g/css styles))
  (inject-inline-link document "--app.css--"
                      (if config/debug?
                        (gstr/format "/app.css?now=%s" (.getTime (js/Date.)))
                        "app.css")))
