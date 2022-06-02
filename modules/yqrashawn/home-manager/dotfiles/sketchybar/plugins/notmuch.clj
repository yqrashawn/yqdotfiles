#!/usr/local/bin/bb

(ns notmuch
  (:require
   [clojure.core.async :refer [chan go-loop <! put! <!!]]
   ;; [babashka.process :as p :refer [process]]
   [clojure.edn :as edn]
   [clojure.string :as s]
   [clojure.java.shell :refer [sh]]))

(defonce c (chan))
(defonce gmi "/usr/local/bin/gmi")
(defonce notmuch "/usr/local/bin/notmuch")
(defonce alerter "/usr/local/bin/alerter")

(sh gmi "sync" :dir (str (System/getenv "HOME") "/.mail/account.gmail"))
(sh gmi "sync" :dir (str (System/getenv "HOME") "/.mail/account.yqrashawn"))
(sh notmuch "new")

(defn notify [title subtitle message]
  (sh
   alerter
   "-message" message
   "-timeout" "30"
   "-group" "NOTMUCH"
   "-sender" "org.gnu.Emacs"
   "-contentImage" (str (System/getenv "HOME") "/.config/sketchybar/plugins/email.png")
   "-sound" "default"
   "-title" title
   "-subtitle" subtitle))

(defn set-label [label]
  (sh "/usr/local/bin/sketchybar" "-m" "--set" "notmuch" (str "label=" label)))

(defn new-unread? []
  (when (pos? (-> (sh notmuch "count" (str "tag:inbox and tag:unread and date:5m.."))
                  :out
                  s/trim-newline
                  Integer/parseInt))
    (let [new (-> (sh notmuch "search" "--format" "sexp" "tag:inbox and tag:unread and date:5m..")
                  :out
                  edn/read-string)
          new (map #(apply hash-map %) new)]
      (doseq [{:keys [authors subject]} new]
        (put! c [(str "New email from " authors) (str "📧" subject) "New email from Notmuch"]))
      (put! c :end))))

(let [unread-count (-> (sh notmuch "count" "tag:inbox and tag:unread")
                       :out
                       s/trim-newline
                       Integer/parseInt)]
  (set-label unread-count)
  (new-unread?))

(<!! (go-loop []
       (let [msg (<! c)]
         (when (vector? msg)
           (apply notify msg)
           (recur)))))
