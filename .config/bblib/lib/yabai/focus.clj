#!/run/current-system/sw/bin/bb

(ns lib.yabai.focus
  (:require
   [clojure.java.shell :refer [sh]]
   [cheshire.core :as json]
   [lib.yabai.core :as y]))

(defn m [direction]
  (let [{:keys [space]}        (y/cur-win)
        space-wins             (y/space-win space)
        same-space-zoomed-wins (reduce (fn [acc {:keys [zoom-parent zoom-fullscreen id]}]
                                         (cond (pos? zoom-parent)
                                               (conj acc {:toggle :zoom-parent :id id})
                                               (pos? zoom-fullscreen)
                                               (conj acc {:toggle :zoom-fullscreen :id id})
                                               :else acc))
                                       [] space-wins)]
    (doseq [{:keys [toggle id]} same-space-zoomed-wins]
      (y/m :window id :--toggle toggle))
    (y/m :window :--focus direction)))
