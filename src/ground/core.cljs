(ns ground.core
  (:require
   [portfolio.reagent :as pr]
   [portfolio.ui :as ui]))

(pr/defscene button
  [:button.button "I am a button"])

(defn init []
  (ui/start!))
