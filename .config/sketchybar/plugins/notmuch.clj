#!/usr/local/bin/bb

(ns notmuch
  (:require
   [clojure.core.async :refer [go]]
   [babashka.process :as p :refer [process]]
   [clojure.edn :as edn]
   [clojure.string :as s]
   [clojure.java.shell :refer [sh]]))

(defonce gmi "/usr/local/bin/gmi")
(defonce notmuch "/usr/local/bin/notmuch")
(defonce alerter "/usr/local/bin/alerter")

(sh gmi "sync" :dir (str (System/getenv "HOME") "/.mail/account.gmail"))
(sh gmi "sync" :dir (str (System/getenv "HOME") "/.mail/account.yqrashawn"))
(sh notmuch "new")

(defn set-label [label]
  (sh "/usr/local/bin/sketchybar" "-m" "--set" "notmuch" (str "label=" label)))

(defn new-unread? []
  (when (pos? (-> (sh notmuch "count" (str "tag:inbox and tag:unread and date:3m.."))
                  :out
                  s/trim-newline
                  Integer/parseInt))
    (let [new (-> (sh notmuch "search" "--format" "sexp" "tag:inbox and tag:unread and date:3m..")
                  :out
                  edn/read-string)
          new (map #(apply hash-map %) new)]
      (doseq [{:keys [authors subject]} new]
        (go
          (sh
           alerter
           "-message" (str "📧" subject)
           "-timeout" "20"
           "-group" "NOTMUCH"
           "-sender" "org.gnu.Emacs"
           "-contentImage" (str (System/getenv "HOME") "/.config/sketchybar/plugins/email.png")
           "-title" "Notmuch"
           "-subtitle" (str "New email from " authors)))))))

(let [unread-count (-> (sh notmuch "count" "tag:inbox and tag:unread")
                       :out
                       s/trim-newline
                       Integer/parseInt)]
  (set-label unread-count)
  (new-unread?))
