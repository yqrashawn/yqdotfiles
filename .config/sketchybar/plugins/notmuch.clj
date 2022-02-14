#!/usr/local/bin/bb

(ns notmuch
  (:require
   [babashka.process :as p :refer [process]]
   [clojure.edn :as edn]
   [clojure.string :as s]
   [clojure.java.shell :refer [sh]]))

(sh "gmi" "sync" :dir (str (System/getenv "HOME") "/.mail/account.gmail"))
(sh "gmi" "sync" :dir (str (System/getenv "HOME") "/.mail/account.yqrashawn"))
(sh "notmuch" "new")

(defn set-label [label]
  (sh "sketchybar" "-m" "--set" "notmuch" (str "label=" label)))

(defn new-unread? []
  (when (pos? (-> (sh "notmuch" "count" (str "tag:inbox and tag:unread and date:3m.."))
                  :out
                  s/trim-newline
                  Integer/parseInt))
    (let [new (-> (sh "notmuch" "search" "--format" "sexp" "tag:inbox and tag:unread and date:3m..")
                  :out
                  edn/read-string)
          new (map #(apply hash-map %) new)]
      (doseq [{:keys [authors subject]} new]
        (let [title    subject
              subtitle (str authors)]
          (prn (str
                "alerter -message '' -timeout 10 -group NOTMUCH -sender org.gnu.Emacs -contentImage"
                (System/getenv "$HOME")
                "/.config/sketchybar/plugins/email.png -title '"
                title
                "' -subtitle '"
                subtitle
                "'"))
          (process
           (str
            "alerter -message '' -timeout 10 -group NOTMUCH -sender org.gnu.Emacs -contentImage"
            (System/getenv "$HOME")
            "/.config/sketchybar/plugins/email.png -title '"
            title
            "' -subtitle '"
            subtitle
            "'")))))))

(let [unread-count (-> (sh "notmuch" "count" "tag:inbox and tag:unread")
                       :out
                       s/trim-newline
                       Integer/parseInt)]
  (set-label unread-count)
  (new-unread?))
