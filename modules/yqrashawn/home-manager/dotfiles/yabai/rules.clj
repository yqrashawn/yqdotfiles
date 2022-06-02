#!/run/current-system/sw/bin/bb
(ns rules
  (:require
   [clojure.string :as s]
   [babashka.process :as p]
   [babashka.fs :as fs]
   [cheshire.core :as json]
   [clojure.java.shell :refer [sh]]))

(defn remove-all-rules []
  (let [rules
        (-> (p/process ["yabai" "-m" "rule" "--list"])
            (p/process ["jq" ".[].index"])
            :out
            slurp
            s/split-lines)]
    (doseq [r (reverse rules)]
      (slurp (:out (p/process ['yabai '-m 'rule '--remove r]))))))

(defn lg-4k? []
  (-> "~/local/bin/has-lg-4k-hdr"
      fs/expand-home
      .toString
      sh
      :exit
      (= 0)))

(defn home-4k? []
  (-> "~/local/bin/home-4k"
      fs/expand-home
      .toString
      sh
      :exit
      (= 0)))

(defn count-display []
  (-> (p/process ['yabai '-m 'query '--displays])
      (p/process ['jq 'length])
      :out
      slurp
      s/trim-newline
      Integer/parseInt))

(defn process-each-conf [{:keys [app] :as conf}]
  (reduce
   (fn [acc [k v]]
     (conj acc
           (s/trim
            (str
             (name k)
             "="
             ;; (cond (= k :app)
             ;;       (if (keyword? v)
             ;;         (str "\"^" (name v) "$\"")
             ;;         (str "\"" v "\""))

             ;;       (= k :manage)
             ;;       (name v)

             ;;       (keyword? v)
             ;;       (str "\"" (name v) "\"")

             ;;       :else
             ;;       v)
             (cond (= k :app)
                   (if (keyword? v)
                     (str "^" (name v) "$")
                     v)

                   (= k :manage)
                   (name v)

                   (keyword? v)
                   (name v)

                   :else
                   v)))))
   []
   conf))

(defn process-config [conf]
  (let [prefix ["yabai" "-m" "rule" "--add"]

        into-prefix (fn [x] (into prefix x))

        commands

        (mapv (comp into-prefix
                    process-each-conf) conf)]
    commands))

(let [display-count (count-display)

      base-conf [{:app :Emacs :space 1 :manage :on}
                 {:app :Emacs :title "" :manage :off}
                 {:app :Emacs :title "^  —  " :manage :off :border :off :topmost :on}
                 {:app :Emacs :title "^ .Minibuf.*$" :manage :off :border :off :topmost :on}
                 {:app :Emacs :title "^(Emacs Everywhere|edit - )" :manage :off :topmost :on}
                 {:app :Alacritty :space 1 :manage :off}
                 {:app :Mail :space 1 :manage :off}
                 {:app :Slack :space 1 :manage :off}
                 {:app :Todoist :space 1 :manage :off}
                 {:app :WeChat :space 1 :manage :off}
                 {:app :ClickUp :space 1 :manage :off}
                 ;; {:app "^Firefox Developer Edition$" :space 1 :manage :off}
                 {:app :Surge :manage :off}
                 {:app :TencentMeeting :manage :off}
                 {:app :Twitterrific :manage :off}
                 {:app :Dash :manage :off}
                 {:app :Sensei :manage :off}
                 {:app :Finder :manage :off}
                 {:app :Install :manage :off}
                 {:app :Image2icon :manage :off}
                 {:app :iShot :manage :off}
                 {:app :Expressions :manage :off}
                 {:app :DingTalk :manage :off}
                 {:app :ToothFairy :manage :off}
                 {:app :CopyQ :manage :off}

                 {:app "^AutoSwitchInput.* :manage :off"}
                 {:app "^CleanMyMac.*" :manage :off}
                 {:app "^System Preferences$" :manage :off}
                 {:app "^Karabiner-Elements$" :manage :off}
                 {:app "^Karabiner-EventViewer$" :manage :off}
                 {:app "^Alfred Preferences$" :manage :off}
                 {:app "^Google Drive$" :manage :off}
                 {:app "^CleanShot X$" :manage :off}
                 {:app "^iStat Menus Status$" :manage :off}
                 {:title "(Copy|Bin|About This Mac|Info)" :manage :off}
                 {:app "^(Calculator|System Preferences|[sS]tats|yabai|[Jj]et[Bb]rains [Tt]ool[Bb]ox)$" :manage :off}

                 {:app :Typora :title "^Preferences$"}

                 {:app "^Digital Colou?r Meter$" :sticky :on}]

      config
      (cond (= display-count 2)
            [;; right
             {:app :Notion :space 2 :manage :on}
             {:app :ClickUp :space 3 :manage :on}
             {:app :Keybase :space 4 :manage :on}
             {:app :Discord :space 5 :manage :on}
             {:app :telegram :space 5 :manage :on}
             {:app :NeteaseMusic :space 5 :manage :off}

             ;; left
             {:app "^Firefox Developer Edition$" :space 6 :manage :on}
             {:app "Chrome" :space 7 :manage :on}
             {:app :Figma :space 8 :manage :on}
             {:app "^Adobe Photoshop" :space 8 :manage :on}]
            (and (= display-count 1) (or (lg-4k?) (home-4k?)))
            [{:app "^Firefox Developer Edition$" :space 1 :manage :off}
             {:app "Chrome" :space 1 :manage :on}
             ;; {:app ".*" :space 1 :manage :off}
             ]
            :else
            []
            ;; [{:app ".*" :space 1}]
            )

      config (into base-conf config)

      commands
      (process-config config)]
  (remove-all-rules)
  (if (or (= display-count 2) (lg-4k?) (home-4k?))
    (apply sh ["yabai" "-m" "config" "layout" "bsp"])
    (apply sh ["yabai" "-m" "config" "layout" "stack"]))
  (doseq [c commands]
    ;; (prn c)
    (slurp (:out (p/process c)))))
