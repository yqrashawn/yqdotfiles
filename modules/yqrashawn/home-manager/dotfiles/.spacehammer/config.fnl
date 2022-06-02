;; Copyright (c) 2017-2020 Ag Ibragimov & Contributors
;;
;;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;;
;;; Contributors:
;;   Jay Zawrotny <jayzawrotny@gmail.com>
;;
;;; URL: https://github.com/agzam/spacehammer
;;
;;; License: MIT
;;


(require-macros :lib.macros)
(local log (hs.logger.new "\tcore.fnl\t" "debug"))
(local windows (require :windows))
(local emacs (require :emacs))
(local slack (require :slack))
(local vim (require :vim))

(local {:concat concat
        :logf logf} (require :lib.functional))

(hs.application.enableSpotlightForNameSearches true)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WARNING
;; Make sure you are customizing ~/.spacehammer/config.fnl and not
;; ~/.hammerspoon/config.fnl
;; Otherwise you will lose your customizations on upstream changes.
;; A copy of this file should already exist in your ~/.spacehammer directory.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Table of Contents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; [x] w - windows
;; [x] |-- w - Last window
;; [x] |-- cmd + hjkl - jumping
;; [x] |-- hjkl - halves
;; [x] |-- alt + hjkl - increments
;; [x] |-- shift + hjkl - resize
;; [x] |-- n, p - next, previous screen
;; [x] |-- shift + n, p - up, down screen
;; [x] |-- g - grid
;; [x] |-- m - maximize
;; [x] |-- c - center
;; [x] |-- u - undo
;;
;; [x] a - apps
;; [x] |-- e - emacs
;; [x] |-- g - chrome
;; [x] |-- f - firefox
;; [x] |-- i - Alacritty
;; [x] |-- s - Slack
;; [x] |-- b - Brave
;;
;; [x] j - jump
;;
;; [x] m - media
;; [x] |-- h - previous track
;; [x] |-- l - next track
;; [x] |-- k - volume up
;; [x] |-- j - volume down
;; [x] |-- s - play\pause
;; [x] |-- a - launch player
;;
;; [x] x - emacs
;; [x] |-- c - capture
;; [x] |-- z - note
;; [x] |-- f - fullscreen
;; [x] |-- v - split
;;
;; [x] alt-n - next-app
;; [x] alt-p - prev-app


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn activator
  [app-name]
  "
  A higher order function to activate a target app. It's useful for quickly
  binding a modal menu action or hotkey action to launch or focus on an app.
  Takes a string application name
  Returns a function to activate that app.

  Example:
  (local launch-emacs (activator \"Emacs\"))
  (launch-emacs)
  "
  (fn activate []
    (windows.activate-app app-name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If you would like to customize this we recommend copying this file to
;; ~/.spacehammer/config.fnl. That will be used in place of the default
;; and will not be overwritten by upstream changes when spacehammer is updated.
(local music-app "NeteaseMusic")

(local return
       {:key :space
        :title "Back"
        :action :previous})


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local window-jumps
       [{:mods [:cmd]
         :key "hjkl"
         :title "Jump"}
        {:mods [:cmd]
         :key :h
         :action "windows:jump-window-left"
         :repeatable true}
        {:mods [:cmd]
         :key :j
         :action "windows:jump-window-above"
         :repeatable true}
        {:mods [:cmd]
         :key :k
         :action "windows:jump-window-below"
         :repeatable true}
        {:mods [:cmd]
         :key :l
         :action "windows:jump-window-right"
         :repeatable true}])

(local window-halves
       [{:key "hjkl"
         :title "Halves"}
        {:key :h
         :action "windows:resize-half-left"
         :repeatable true}
        {:key :j
         :action "windows:resize-half-bottom"
         :repeatable true}
        {:key :k
         :action "windows:resize-half-top"
         :repeatable true}
        {:key :l
         :action "windows:resize-half-right"
         :repeatable true}])

(local window-increments
       [{:mods [:alt]
         :key "hjkl"
         :title "Increments"}
        {:mods [:alt]
         :key :h
         :action "windows:resize-inc-left"
         :repeatable true}
        {:mods [:alt]
         :key :j
         :action "windows:resize-inc-bottom"
         :repeatable true}
        {:mods [:alt]
         :key :k
         :action "windows:resize-inc-top"
         :repeatable true}
        {:mods [:alt]
         :key :l
         :action "windows:resize-inc-right"
         :repeatable true}])

(local window-resize
       [{:mods [:shift]
         :key "hjkl"
         :title "Resize"}
        {:mods [:shift]
         :key :h
         :action "windows:resize-left"
         :repeatable true}
        {:mods [:shift]
         :key :j
         :action "windows:resize-down"
         :repeatable true}
        {:mods [:shift]
         :key :k
         :action "windows:resize-up"
         :repeatable true}
        {:mods [:shift]
         :key :l
         :action "windows:resize-right"
         :repeatable true}])

(local window-move-screens
       [{:key "n, p"
         :title "Move next\\previous screen"}
        {:mods [:shift]
         :key "n, p"
         :title "Move up\\down screens"}
        {:key :n
         :action "windows:move-south"
         :repeatable true}
        {:key :p
         :action "windows:move-north"
         :repeatable true}
        {:mods [:shift]
         :key :n
         :action "windows:move-west"
         :repeatable true}
        {:mods [:shift]
         :key :p
         :action "windows:move-east"
         :repeatable true}])

(local window-bindings
       (concat
        [return
         {:key :w
          :title "Last Window"
          :action "windows:jump-to-last-window"}]
        window-jumps
        window-halves
        window-increments
        window-resize
        window-move-screens
        [{:key :m
          :title "Maximize"
          :action "windows:maximize-window-frame"}
         {:key :c
          :title "Center"
          :action "windows:center-window-frame"}
         {:key :g
          :title "Grid"
          :action "windows:show-grid"}
         {:key :u
          :title "Undo"
          :action "windows:undo-action"}]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Apps Menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local app-bindings
       [return
        {:key :k
         :title "Keybase"
         :action (activator "Keybase")}
        {:key :m
         :title music-app
         :action (activator music-app)}])

(local media-bindings
       [return
        {:key :p
         :title "Play or Pause"
         :action "multimedia:play-or-pause"}
        {:key :h
         :title "Prev Track"
         :action "multimedia:prev-track"}
        {:key :l
         :title "Next Track"
         :action "multimedia:next-track"}
        {:key :j
         :title "Volume Down"
         :action "multimedia:volume-down"
         :repeatable true}
        {:key :k
         :title "Volume Up"
         :action "multimedia:volume-up"
         :repeatable true}
        {:key :a
         :title (.. "Launch " music-app)
         :action (activator music-app)}])

(local emacs-bindings
       [return
        {:key :c
         :title "Capture"
         :action (fn [] (emacs.capture))}
        {:key :z
         :title "Note"
         :action (fn [] (emacs.note))}
        {:key :v
         :title "Split"
         :action "emacs:vertical-split-with-emacs"}
        {:key :f
         :title "Full Screen"
         :action "emacs:full-screen"}])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Menu & Config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn browser-org-roam-capture []
  (hs.eventtap.keyStroke ["alt" "shift"] "9")
  (hs.eventtap.keyStroke [] "R"))
(fn browser-newsblur []
  (hs.eventtap.keyStroke ["alt" "shift"] "9")
  (hs.eventtap.keyStroke [] "N"))

(local menu-items
       [ ;; {:key    :space
        ;;  :title  "Alfred"
        ;;  :action (activator "Alfred 4")}
        ;; {:key   :w
        ;;  :title "Window"
        ;;  :enter "windows:enter-window-menu"
        ;;  :exit "windows:exit-window-menu"
        ;;  :items window-bindings}
        {:key "'"
          :title "Edit with Emacs"
          :action "emacs:edit-with-emacs"}
        {:key   :a
         :title "Apps"
         :items app-bindings}
        {:key    :j
         :title  "Jump"
         :action "windows:jump"}
        {:key   :m
         :title "Media"
         :items media-bindings}
        ;; {:key   :x
        ;;  :title "Emacs"
        ;;  :items emacs-bindings}
        ])

(local common-keys
       [{ ;; :mods [:alt]
         :key :F18
         :action "lib.modal:activate-modal"}
        ;; {:mods [:alt]
        ;;  :key :n
        ;;  :action "apps:next-app"}
        ;; {:mods [:alt]
        ;;  :key :p
        ;;  :action "apps:prev-app"}
        {:mods [:cmd :ctrl]
         :key "`"
         :action hs.toggleConsole}
        ;; {:mods [:cmd :ctrl]
        ;;  :key :o
        ;;  :action "emacs:edit-with-emacs"}
        ;; {:mods [:hyper]
        ;;  :key :1
        ;;  :title  "Emacs"
        ;;  :action (activator "Emacs")}
        ;; {:mods [:hyper]
        ;;  :key :2
        ;;  :title  "Alacritty"
        ;;  :action (activator "Alacritty")}
        ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; App Specific Config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local browser-keys
       [{:mods [:cmd :shift]
         :key :l
         :action "chrome:open-location"}])

(local browser-items
       (concat
        menu-items
        [{:key :c
          :title "Capture"
          :items [{:key :r
                   :title "Org Roam"
                   :action browser-org-roam-capture}
                  {:key :b
                   :title "Newsblur"
                   :action browser-newsblur}
                  {:key :n
                   :title "Notion"
                   :action (fn [] (hs.eventtap.keyStroke ["cmd" "shift"] "k"))}
                  {:key :t
                   :title "Todoist"
                   :action (fn [] (hs.eventtap.keyStroke ["alt" "shift"] "i"))}]}]))

(local firefox-items
       (concat
        browser-items
        [{:key "e"
          :title "Extension"
          :items [{:key :b
                   :title "Bitwarden"
                   :items [{:key :b
                            :title "Open sidebar"
                            :action (fn [] (hs.eventtap.keyStroke ["alt" "shift"] "y"))}
                           {:key :space
                            :title "Fill pass"
                            :action (fn [] (hs.eventtap.keyStroke ["cmd" "shift"] "l"))}
                           {:key :g
                            :title "Gen pass"
                            :action (fn [] (hs.eventtap.keyStroke ["cmd" "shift"] "9"))}]}
                  {:key :d
                   :title "Toggle dark mode"
                   :action (fn [] (hs.eventtap.keyStroke ["alt" "shift"] "a"))}
                  {:key :t
                   :title "Toggle tree tab"
                   :action (fn [] (hs.eventtap.keyStroke ["cmd" "ctrl"] "e"))}]}]))

(local clickup-config
       {:key "ClickUp"
        :keys browser-keys
        :items browser-items})

(local safari-config
       {:key "Safari"
        :keys browser-keys
        :items browser-items})

(local brave-config
       {:key "Brave Browser"
        :keys browser-keys
        :items browser-items})

(local chrome-config
       {:key "Google Chrome Canary"
        :keys browser-keys
        :items browser-items})

(local firefox-config
       {:key "Firefox Developer Edition"
        :keys browser-keys
        :items firefox-items})

(local emacs-config
       {:key "Emacs"
        :activate (fn [] (vim.disable))
        :deactivate (fn [] (vim.enable))
        :launch "emacs:maximize"
        :items []
        :keys []})

(local grammarly-config
       {:key "Grammarly"
        :items (concat
                menu-items
                [{:mods [:ctrl]
                  :key :c
                  :title "Return to Emacs"
                  :action "grammarly:back-to-emacs"}])
        :keys ""})

(local hammerspoon-config
       {:key "Hammerspoon"
        :items (concat
                menu-items
                [{:key :r
                  :title "Reload Console"
                  :action hs.reload}
                 {:key :c
                  :title "Clear Console"
                  :action hs.console.clearConsole}])
        :keys []})

(fn slack-thread []
  (hs.eventtap.keyStroke [] :up)
  (hs.eventtap.keyStroke [] :right)
  (hs.eventtap.keyStroke [] :down))

(local slack-config
       {:key "Slack"
        :keys [{:mods [:cmd]
                :key  :l
                :action "slack:scroll-to-bottom"}
               {:mods [:ctrl]
                :key :r
                :action "slack:add-reaction"}

               ;; {:mods [:ctrl]
               ;;  :key :p
               ;;  :action "slack:prev-element"}
               ;; {:mods [:ctrl]
               ;;  :key :n
               ;;  :action "slack:next-element"}

               {:mods [:ctrl]
                :key :t
                :action slack-thread}

               ;; {:mods [:ctrl]
               ;;  :key :p
               ;;  :action "slack:prev-day"}
               ;; {:mods [:ctrl]
               ;;  :key :n
               ;;  :action "slack:next-day"}

               {:mods [:ctrl]
                :key :k
                :action "slack:scroll-up"
                :repeat true}
               {:mods [:ctrl]
                :key :j
                :action "slack:scroll-down"
                :repeat true}

               {:mods [:ctrl]
                :key :i
                :action "slack:next-history"
                :repeat true}
               {:mods [:ctrl]
                :key :o
                :action "slack:prev-history"
                :repeat true}
               ;; {:mods [:ctrl]
               ;;  :key :j
               ;;  :action "slack:down"
               ;;  :repeat true}
               ;; {:mods [:ctrl]
               ;;  :key :k
               ;;  :action "slack:up"
               ;;  :repeat true}
               ]})

(local apps
       [brave-config
        chrome-config
        firefox-config
        emacs-config
        grammarly-config
        hammerspoon-config
        safari-config
        clickup-config
        slack-config])

(local config
       {:title "Main Menu"
        :items menu-items
        :keys  common-keys
        :enter (fn [] (windows.hide-display-numbers))
        :exit  (fn [] (windows.hide-display-numbers))
        :apps  apps
        :hyper {:key :F17}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spoon
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(hs.loadSpoon "SpoonInstall")
(set spoon.SpoonInstall.use_syncinstall true)
(local Install spoon.SpoonInstall)


(local brv "com.brave.Browser")
(local vv "com.vivaldi.Vivaldi")
(local ffd "org.mozilla.firefoxdeveloperedition")
(local orion "com.kagi.kagimacOS")
(local edge "com.microsoft.edgemac.Beta")
(local ff "org.mozilla.firefox")
(local chrm "com.google.Chrome")
(local chrmc "com.google.Chrome.canary")
(local safari "com.apple.Safari")
(local clickup "com.clickup.desktop-app")
(local figma "com.figma.desktop")
(local todoist "com.todoist.mac.Todoist")

;; https://www.hammerspoon.org/Spoons/URLDispatcher.html
(Install:andUse "URLDispatcher"
 {:start true
  :loglevel "error"
  :config
  {:url_patterns [["zoommtg:" "us.zoom.xos"]
                  ["tg:" "ru.keepcoder.Telegram"]
                  ["https://yqrashawn.deta.dev.*" chrm]
                  ;; ["http://localhost:.*" chrm]
                  ["http://localhost:.*" chrm]
                  ["https://app.clickup.com" clickup]
                  ["https://www.figma.com/file" figma]
                  ["https://alidocs.dingtalk.com" chrm]
                  ;; ["https://todoist.com/app" todoist]
                  ;; ["https://todoist.com/showTask" todoist]
                  ]
   :url_redir_decoders [["Zoom URLs"
                         "https?://.*zoom%.us/j/(%d+)%?pwd=(%w)"
                         "zoommtg://zoom.us/join?confno=%1&pwd=%2"
                         true]
                        ["Telegram URLs"
                         "https?://t.me/(.*)"
                         "tg://t.me/%1"
                         true]
                        ["Fix broken Preview anchor URLs"
                         "%%23"
                         "#"
                         false
                         "Preview"]]

   ;; :default_handler ffd
   :default_handler edge}})

(comment
 (local col hs.drawing.color.x11))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

config
