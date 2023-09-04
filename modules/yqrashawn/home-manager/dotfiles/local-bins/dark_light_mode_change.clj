#!/usr/bin/env bb

(ns dark-light-mode-change
  (:require [babashka.fs :as fs]
            [babashka.tasks :refer [shell]]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as s]))

(def theme (if (= (System/getenv "DARKMODE") "1") :dark :light))
;; (def theme :dark)

(defn- yml-comment [line] (str "# " line))

;;; kitty
(defn kitty
  []
  (let [conf (fs/file (fs/expand-home "~/.config/kitty/current-theme.conf"))
        dark (fs/file (fs/expand-home "~/.config/kitty/dark.conf"))
        light (fs/file (fs/expand-home "~/.config/kitty/light.conf"))
        next-theme (if (= theme :dark) dark light)]
    (spit conf (slurp next-theme))))

(defn reload-kitty-conf
  []
  (when-let [kitty-pid (-> (:out (sh "pgrep" "kitty"))
                           s/trim-newline
                           (s/split #"\n")
                           first)]
    (sh "kill" "-USR1" kitty-pid)))

;;; alacritty
(def theme-lines
  {:dark "- ~/.config/alacritty/modus-vivendi.yml",
   :light "- ~/.config/alacritty/modus-operandi.yml"})

(defn alacritty
  []
  (let [conf-file (fs/file (fs/expand-home
                             "~/.config/alacritty-current-theme.yml"))
        conf (slurp conf-file)
        cur-theme (get theme-lines (if (= theme :dark) :light :dark))
        next-theme (get theme-lines theme)
        ;; uncomment all
        conf (-> conf
                 (s/replace (re-pattern (yml-comment cur-theme)) cur-theme)
                 (s/replace (re-pattern (yml-comment next-theme)) next-theme))
        conf (-> conf
                 (s/replace (re-pattern cur-theme) (yml-comment cur-theme)))]
    (spit conf-file conf)))

(defn emacs
  []
  (if (= theme :dark)
    (sh "/run/current-system/sw/bin/emacsclient"
        "-n" "-q"
        "-e" "(load-theme 'ef-cherie :no-confirm)")
    (sh "/run/current-system/sw/bin/emacsclient"
        "-n" "-q"
        "-e" "(load-theme 'ef-day :no-confirm)")))

(try (kitty) (reload-kitty-conf) (catch Exception _))
(try (alacritty) (catch Exception _))
(try (emacs) (catch Exception _))

(comment
  (when-let [kitty-pid (-> (:out (sh "pgrep" "kitty"))
                           s/trim-newline
                           (s/split #"\n")
                           first)]
    (sh "kill" "-USR1" kitty-pid))
  (slurp (:out (shell "pgrep -d x kitty")))
  (shell "launchctl load -w"
         (fs/expand-home
          "~/Library/LaunchAgents/com.yqrashawn.dark-mode-notify.plist"))
  (shell "launchctl remove com.yqrashawn.dark-mode-notify")
  (shell "osascript"
         "-l" "JavaScript"
         "-e"
         "Application('System Events').appearancePreferences.darkMode=true")
  (shell "osascript"
         "-l" "JavaScript"
         "-e"
         "Application('System Events').appearancePreferences.darkMode=false"))
