(ns user
  (:require
   [nextjournal.clerk :as clerk]))

(clerk/serve! {:browse? false})

(comment
  (clerk/show! "notebooks/default.clj"))
