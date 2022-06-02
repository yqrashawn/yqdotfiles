#!/usr/local/bin/bb

(ns failed-gh-action-in-repos
  (:require
   [lib.github.checks :as checks]
   [lib.sketchybar.core :as skb]
   [lib.github.pr :as pr]))

(let [d          (->> (pr/my-open-pr "Conflux-Chain" "helios")
                      (filter #(-> :user :login (= "yqrashawn" %)))
                      (mapv (fn [{:keys [head number]}]
                              {:number number
                               :runs   (checks/runs "Conflux-Chain" "helios"
                                                    ;; "fb5d665ad963c9cc54444dc8783df7b6b03f63c3"
                                                    (:sha head)
                                                    :completed)})))
      any-faile? (some (fn [{:keys [conclusion]}]
                         (or (= conclusion "failed")
                             (= conclusion "timed_out")))
                       (:runs d))]
  (if any-faile?
    (skb/m :--set "ghactcheck" (str "label=#" (:number d)))
    (skb/m :--set "ghactcheck" "label=")))
