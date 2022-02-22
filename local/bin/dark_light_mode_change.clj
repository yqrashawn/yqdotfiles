#!/usr/local/bin/bb
(ns dark-light-mode-change
  (:require
   [babashka.tasks :refer [shell]]
   [clojure.java.shell :refer [sh]]
   [clojure.string :as s]
   [babashka.fs :as fs]))

(def theme (if (= (System/getenv "DARKMODE") "1") :dark :light))
;; (def theme :dark)

(defn- yml-comment [line]
  (str "# " line))

;;; alacritty
(def theme-lines {:dark  "- ~/.config/alacritty/modus-vivendi.yml"
                  :light "- ~/.config/alacritty/modus-operandi.yml"})

(defn alacritty []
  (let [conf-file  (fs/file (fs/expand-home "~/.config/alacritty/current-theme.yml"))
        conf       (slurp conf-file)
        cur-theme  (get theme-lines (if (= theme :dark) :light :dark))
        next-theme (get theme-lines theme)
        ;; uncomment all
        conf       (-> conf
                       (s/replace (re-pattern (yml-comment cur-theme)) cur-theme)
                       (s/replace (re-pattern (yml-comment next-theme)) next-theme))
        conf       (-> conf
                       (s/replace (re-pattern cur-theme)
                                  (yml-comment cur-theme)))]
    (spit conf-file conf)))

(defn mcfly []
  (if (= theme :dark)
    (sh "export" "MCFLY_LIGHT=FALSE")
    (sh "export" "MCFLY_LIGHT=TRUE")))

(defn emacs []
  (if (= theme :dark)
    (sh "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient" "-n" "-q" "-e" "(load-theme 'modus-vivendi :no-confirm)")
    (sh "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient" "-n" "-q" "-e" "(load-theme 'modus-operandi :no-confirm)")))

(alacritty)
(emacs)
;; (mcfly)

(comment
  (shell "launchctl load -w" (fs/expand-home "~/Library/LaunchAgents/com.yqrashawn.dark-mode-notify.plist"))
  (shell "launchctl remove com.yqrashawn.dark-mode-notify")
  (shell "osascript" "-l" "JavaScript" "-e" "Application('System Events').appearancePreferences.darkMode=true")
  (shell "osascript" "-l" "JavaScript" "-e" "Application('System Events').appearancePreferences.darkMode=false"))
