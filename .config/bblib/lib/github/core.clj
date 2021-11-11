(ns lib.github.core
  (:require
   [org.httpkit.client :as c]
   [clojure.java.shell :refer [sh]]))

(def ghtoken (:out (sh "security" "find-generic-password" "-s" "github-token" "-w")))
(def default-opts {:headers {"Accept"        "application/vnd.github.v3+json"
                             "Connection"    "close"
                             "Host"          "api.github.com"
                             "User-Agent"    "curl/7.79.1"
                             "Authorization" (str "token " ghtoken)}})
(defn cget
  ([url] (cget url {}))
  ([url opts]
   (c/get (str "https://api.github.com" url) (merge-with merge default-opts opts))))
