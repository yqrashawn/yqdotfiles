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


;;; top
(require-macros :lib.macros)
(local log (hs.logger.new "\tcore.fnl\t" "debug"))
(local windows (require :windows))
(local emacs (require :emacs))
(local slack (require :slack))
(local vim (require :vim))

(local {:concat concat
        :logf logf} (require :lib.functional))

(hs.application.enableSpotlightForNameSearches true)

;;; Body
;;;; Table of Contents

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


;;;; Initialize


;;;; Actions

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

(fn delay [seconds f]
  (let [timer (hs.timer.delayed.new seconds f)]
    (: timer :start)))

(fn browser-summarize []
  (hs.eventtap.keyStroke ["cmd" "ctrl"] ".")
  (delay 0.2 (fn []
               (hs.eventtap.keyStrokes "Help me read")
               (hs.eventtap.keyStroke [] "return")
               (hs.eventtap.keyStroke ["cmd"] "return"))))

(fn browser-summarize-more-detail []
  (hs.eventtap.keyStrokes "Give me more details with examples using bullet points, make sure all points included.")
  (delay .1 (fn [] (hs.eventtap.keyStroke ["cmd"] "return"))))

(fn test-password []
  (delay 0.001 (fn []
               (hs.eventtap.keyStroke ["shift"] "a")
               (delay 0.001 (fn [] (hs.eventtap.keyStroke [] "b")
                            (delay 0.001 (fn [] (hs.eventtap.keyStroke [] "c")
                                         (delay 0.001 (fn [] (hs.eventtap.keyStroke [] "d")
                                                      (delay 0.001 (fn [] (hs.eventtap.keyStroke [] "1")
                                                                   (delay 0.001 (fn [] (hs.eventtap.keyStroke [] "2")
                                                                                (delay 0.001 (fn [] (hs.eventtap.keyStroke [] "3")
                                                                                             (delay 0.001 (fn [] (hs.eventtap.keyStroke [] "4")
                                                                                                          (delay 0.001 (fn [] (hs.eventtap.keyStroke [] ";")
                                                                                                                       (delay 0.001 (fn [] (hs.eventtap.keyStroke [] ";"))))))))))))))))))))))


;;;; General

;; If you would like to customize this we recommend copying this file to
;; ~/.spacehammer/config.fnl. That will be used in place of the default
;; and will not be overwritten by upstream changes when spacehammer is updated.
(local music-app "NeteaseMusic")

(local return
       {:key :space
        :title "Back"
        :action :previous})


;; keycodes.map
;; https://github.com/Hammerspoon/hammerspoon/blob/master/extensions/keycodes/keycodes.lua#L74
;;;; Rectangle
(local rectangle
       {:key :l
        :title "Rectangle"
        :items [{:key :t
                 :title "Tile"
                 :items [{:key :return
                          :title "Maximize"
                          :action (fn [] (hs.eventtap.keyStroke ["ctrl" "shift" "alt"] "return"))}
                         {:key :space
                          :title "Center Maximize"
                          :action (fn [] (hs.eventtap.keyStroke ["ctrl" "shift" "alt"] "space"))}
                         {:key :h
                          :title "left"
                          :action (fn [] (hs.eventtap.keyStroke ["ctrl" "shift" "alt"] "h"))}
                         {:key :l
                          :title "right"
                          :action (fn [] (hs.eventtap.keyStroke ["ctrl" "shift" "alt"] "l"))}
                         {:key :k
                          :title "top"
                          :action (fn [] (hs.eventtap.keyStroke ["ctrl" "shift" "alt"] "k"))}
                         {:key :j
                          :title "bottom"
                          :action (fn [] (hs.eventtap.keyStroke ["ctrl" "shift" "alt"] "j"))}]}
                {:key :s
                 :title "Stash"
                 :items [{:key :h
                          :title "left"
                          :action (fn [] (hs.eventtap.keyStroke ["ctrl" "shift" "alt"] "7"))}
                         {:key :l
                          :title "right"
                          :action (fn [] (hs.eventtap.keyStroke ["ctrl" "shift" "alt"] "0"))}
                         {:key :k
                          :title "top"
                          :action (fn [] (hs.eventtap.keyStroke ["ctrl" "shift" "alt"] "9"))}
                         {:key :j
                          :title "bottom"
                          :action (fn [] (hs.eventtap.keyStroke ["ctrl" "shift" "alt"] "8"))}
                         {:key :u
                          :title "unstash"
                          :action (fn [] (hs.eventtap.keyStroke ["command" "shift" "alt"] "/"))}
                         {:key :x
                          :title "unstash all"
                          :action (fn [] (hs.eventtap.keyStroke ["command" "shift" "alt"] "."))}]}]})
;;;; Windows
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

;;;; Apps Menu
(local app-bindings
       [return
        {:key :v
         :title "VSCode"
         :action (activator "Visual Studio Code")}
        {:key :c
         :title "Chrome"
         :action (activator "Google Chrome")}
        {:key :r
         :title "Safari"
         :action (activator "Safari")}
        {:key :s
         :title "Spotify"
         :action (activator "Spotify")}
        {:key :n
         :title "Notion"
         :action (activator "Notion")}
        {:key :t
         :title "Telegram"
         :action (activator "Telegram")}
        {:key :m
         :title music-app
         :action (activator music-app)}])

(local j-bindings
       [return
        {:key :m
         :title "Help"
         :action (fn [] (hs.eventtap.keyStroke ["control" "shift" "option"] "6"))}
        {:key :j
         :title "Perplexity Dictation"
         :action (fn [] (hs.eventtap.keyStroke ["command" "control"] "7"))}
        {:key :s
         :title "Perplexity Screenshot"
         :action (fn [] (hs.eventtap.keyStroke ["command" "control"] "6"))}
        {:key :=
         :title "Perplexity"
         :action (fn [] (hs.eventtap.keyStroke ["command" "control"] "8"))}])

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

(local insert-bindings
       [return
        {:key :o
         :title "Org Link"
         :action (fn [] (hs.osascript.applescript "tell application id \"com.runningwithcrayons.Alfred\" to run trigger \"yqlink-org\" in workflow \"com.yqrashawn.yqlink\" with argument \"\""))}
        {:key :m
         :title "MD Link"
         :action (fn [] (hs.osascript.applescript "tell application id \"com.runningwithcrayons.Alfred\" to run trigger \"yqlink-md\" in workflow \"com.yqrashawn.yqlink\" with argument \"\""))}])

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

;;;; Main Menu & Config
(fn browser-org-roam-capture []
  (hs.eventtap.keyStroke ["alt" "shift"] "9")
  (hs.eventtap.keyStroke [] "R"))
(fn browser-newsblur []
  (hs.eventtap.keyStroke ["alt" "shift"] "9")
  (hs.eventtap.keyStroke [] "N"))

(local desktop-binding
       [return
        {:key :t
         :title "Toggle"
         :items [{:key :f
                  :title "float"
                  :action (fn [] (hs.execute "yabai -m window --toggle float"))}
                 {:key :T
                  :mods [:shift]
                  :title "topmost"
                  :action (fn [] (hs.execute "yabai -m window --toggle float"))}
                 {:key :B
                  :mods [:shift]
                  :title "border"
                  :action (fn [] (hs.execute "yabai -m window --toggle float"))}
                 {:key :m
                  :mods []
                  :title "zoom-parent"
                  :action (fn [] (hs.execute "yabai -m window --toggle float"))}
                 {:key :b
                  :mods [:shift]
                  :title "bsp layout"
                  :action (fn [] (hs.execute "yabai -m config layout bsp"))}
                 {:key :s
                  :mods [:shift]
                  :title "stack layout"
                  :action (fn [] (hs.execute "yabai -m config layout stack"))}
                 {:key :f
                  :mods [:shift]
                  :title "float layout"
                  :action (fn [] (hs.execute "yabai -m config layout float"))}]}
        {:key :1
         :title "To display 1"
         :action (fn [] (hs.execute "~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/yabai/move-to-display.sh 1"))}
        {:key :2
         :title "To display 2"
         :action (fn [] (hs.execute "~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/yabai/move-to-display.sh 2"))}
        {:key :3
         :title "To display 3"
         :action (fn [] (hs.execute "~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/yabai/move-to-display.sh 3"))}
        {:key :s
         :title "To display space"
         :items [{:key :1
                  :title "D1"
                  :items [{:key :1
                           :title "S1"
                           :action (fn [] (hs.execute "~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/yabai/move-to-space.sh 1"))}
                          {:key :2
                           :title "S2"
                           :action (fn [] (hs.execute "~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/yabai/move-to-space.sh 2"))}
                          {:key :3
                           :title "S3"
                           :action (fn [] (hs.execute "~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/yabai/move-to-space.sh 3"))}
                          {:key :4
                           :title "S4"
                           :action (fn [] (hs.execute "~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/yabai/move-to-space.sh 4"))}
                          {:key :5
                           :title "S5"
                           :action (fn [] (hs.execute "~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/yabai/move-to-space.sh 5"))}]}
                 {:key :2
                  :title ""
                  :items [{:key :1
                           :title "S1"
                           :action (fn [] (hs.execute "~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/yabai/move-to-space.sh 6"))}
                          {:key :2
                           :title "S2"
                           :action (fn [] (hs.execute "~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/yabai/move-to-space.sh 7"))}
                          {:key :3
                           :title "S3"
                           :action (fn [] (hs.execute "~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/yabai/move-to-space.sh 8"))}
                          {:key :4
                           :title "S4"
                           :action (fn [] (hs.execute "~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/yabai/move-to-space.sh 9"))}
                          {:key :5
                           :title "S5"
                           :action (fn [] (hs.execute "~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/yabai/move-to-space.sh 10"))}]}
                 {:key :3
                  :title ""
                  :items [{:key :1
                           :title "S1"
                           :action (fn [] (hs.execute "~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/yabai/move-to-space.sh 11"))}
                          {:key :2
                           :title "S2"
                           :action (fn [] (hs.execute "~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/yabai/move-to-space.sh 12"))}
                          {:key :3
                           :title "S3"
                           :action (fn [] (hs.execute "~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/yabai/move-to-space.sh 13"))}
                          {:key :4
                           :title "S4"
                           :action (fn [] (hs.execute "~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/yabai/move-to-space.sh 14"))}
                          {:key :5
                           :title "S5"
                           :action (fn [] (hs.execute "~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/yabai/move-to-space.sh 15"))}]}]}])

(local screen-binding
       [return
        {:key :v
         :title "Record"
         :action (fn [] (hs.execute "open cleanshot://record-screen"))}
        {:key :t
         :title "Text"
         :action (fn [] (hs.execute "open cleanshot://capture-text"))}
        {:key :s
         :title "Shot"
         :action (fn [] (hs.execute "open cleanshot://capture-area"))}])

(local text-bindings
       [return
        {:key :p
         :title "Polite"
         :action (fn [] (hs.eventtap.keyStroke ["command" "control" "shift"] "6"))}
        {:key :f
         :title "Friendly"
         :action (fn [] (hs.eventtap.keyStroke ["command" "control" "shift"] "7"))}
        {:key :g
         :title "Grammer"
         :action (fn [] (hs.eventtap.keyStroke ["command" "control" "shift"] "8"))}
        {:key :C
         :title "Copy editor"
         :action (fn [] (hs.eventtap.keyStroke ["command" "control" "shift"] "9"))}])

(local menu-items
       [rectangle
        {:key    :space
         :title  "Alfred Universal Action"
         :action (fn [] (hs.eventtap.keyStroke ["command" "control"] "/"))}
        ;; {:key   :w
        ;;  :title "Window"
        ;;  :enter "windows:enter-window-menu"
        ;;  :exit "windows:exit-window-menu"
        ;;  :items window-bindings}
        {:key "'"
         :title "Edit with Emacs"
         :action "emacs:edit-with-emacs"}
        {:key :a
         :title "Apps"
         :items app-bindings}
        ;; {:key :j
        ;;  :title "Jump"
        ;;  :action "windows:jump"}
        {:key :j
         :title :J
         :items j-bindings}
        {:key :m
         :title "Media"
         :items media-bindings}
        {:key :i
         :title "Insert"
         :items insert-bindings}
        {:key :t
         :title "Text"
         :items text-bindings}
        ;; {:key :y
        ;;  :title "Yabai"
        ;;  :items desktop-binding}
        {:key :s
         :title "Screen"
         :items screen-binding}
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

;;;; App Specific Config
(local simulator-items
       [{:key :.
         :title "App Local"
         :items [{:key :p
                  :title "Test Password"
                  :action test-password}]}])

(local status-desktop-items
       [{:key :.
         :title "App Local"
         :items [{:key :p
                  :title "Test Password"
                  :action test-password}]}])

(local browser-keys
       [{:mods [:cmd :shift]
         :key :l
         :action "chrome:open-location"}])

(local browser-items
       (concat
        menu-items
        [{:key :c
          :title "Capture"
          :items [ ;; {:key :r
                  ;;  :title "Org Roam"
                  ;;  :action browser-org-roam-capture}
                  {:key :b
                   :title "Newsblur"
                   :action browser-newsblur}
                  {:key :s
                   :title "Summary"
                   :action browser-summarize}
                  {:key :m
                   :title "More Detail"
                   :action browser-summarize-more-detail}
                  {:key :r
                   :title "Raindrop"
                   :action (fn [] (hs.eventtap.keyStroke ["alt" "shift"] "s"))}
                  {:key :n
                   :title "Notion"
                   :action (fn [] (hs.eventtap.keyStroke ["cmd" "shift"] "k"))}
                  {:key :t
                   :title "Todoist"
                   :action (fn [] (hs.eventtap.keyStroke ["cmd" "ctrl"] "t"))}]}]))

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
                           {:key :f
                            :title "Fill pass"
                            :action (fn [] (hs.eventtap.keyStroke ["cmd" "shift"] "l"))}
                           {:key :g
                            :title "Gen pass"
                            :action (fn [] (hs.eventtap.keyStroke ["cmd" "shift"] "9"))}]}
                  {:key :d
                   :title "Toggle dark mode"
                   :action (fn [] (hs.eventtap.keyStroke ["alt" "shift"] "o"))}
                  {:key :s
                   :title "Toggle sidebery"
                   :action (fn [] (hs.eventtap.keyStroke ["alt" "shift"] "t"))}
                  ;; {:key :t
                  ;;  :title "Toggle tree tab"
                  ;;  :action (fn [] (hs.eventtap.keyStroke ["cmd" "ctrl"] "e"))}
                  ]}]))

(local sidekick-items
       (concat
        browser-items
        [{:key "e"
          :title "Extension"
          :items [{:key :b
                   :title "Bitwarden"
                   :items [{:key :b
                            :title "Open sidebar"
                            :action (fn [] (hs.eventtap.keyStroke ["cmd" "shift"] "y"))}
                           {:key :f
                            :title "Fill pass"
                            :action (fn [] (hs.eventtap.keyStroke ["cmd" "shift"] "l"))}
                           {:key :g
                            :title "Gen pass"
                            :action (fn [] (hs.eventtap.keyStroke ["cmd" "shift"] "9"))}]}
                  {:key :d
                   :title "Toggle dark mode"
                   :action (fn [] (hs.eventtap.keyStroke ["alt" "shift"] "d"))}
                  {:key :s
                   :title "Toggle sessions"
                   :action (fn [] (hs.eventtap.keyStroke ["alt" "shift"] "t"))}]}]))

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
       {:key "Google Chrome"
        :keys browser-keys
        :items browser-items})

(local edge-config
       {:key "Microsoft Edge Beta"
        :keys browser-keys
        :items browser-items})

(local firefox-config
       {:key "Firefox Developer Edition"
        :keys browser-keys
        :items firefox-items})

(local sidekick-config
       {:key "Sidekick"
        :keys browser-keys
        :items sidekick-items})

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
                :key :l
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

(local ios-simulator-config
       {:key "Simulator"
        :keys []
        :items (concat menu-items simulator-items)})

(local status-desktop-config
       {:key "im.Status.NimStatusClient"
        :keys []
        :items (concat menu-items status-desktop-items)})

(local apps
       [brave-config
        chrome-config
        edge-config
        firefox-config
        sidekick-config
        emacs-config
        grammarly-config
        hammerspoon-config
        safari-config
        clickup-config
        slack-config
        ios-simulator-config
        status-desktop-config])

(local config
       {:title "Main Menu"
        :items menu-items
        :keys  common-keys
        :enter (fn [] (windows.hide-display-numbers))
        :exit  (fn [] (windows.hide-display-numbers))
        :apps  apps
        :hyper {:key :F17}})

;;;; Spoon
(hs.loadSpoon "SpoonInstall")
(set spoon.SpoonInstall.use_syncinstall true)
(local Install spoon.SpoonInstall)


(local brv "com.brave.Browser")
(local vv "com.vivaldi.Vivaldi")
(local ffd "org.mozilla.firefoxdeveloperedition")
(local orion "com.kagi.kagimacOS")
(local edge "com.microsoft.edgemac.Beta")
(local ff "org.mozilla.firefox")
(local chrm "com.google.Chrome")        ; personal
(local chrmc "com.google.Chrome.canary") ; st
(local chrmd "com.google.Chrome.dev")    ; sc
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
                  ;; ["https://www.notion.so/" "notion.id"]
                  ["tg:" "ru.keepcoder.Telegram"]
                  ;; ["https://yqrashawn.deta.dev.*" chrm]
                  ;; ["http://localhost:.*" chrm]
                  ;; ["http://localhost:.*" ffd]
                  ;; ["https://app.clickup.com" clickup]
                  ["https://www.figma.com/file" figma]
                  ["https://alidocs.dingtalk.com" chrm]
                  ["https://leetcode.com" chrm]

                  ;; scroll
                  ;; ["https?://.*scroll.*" chrmd]
                  ;; ["personio" chrmd]

                  ;; status
                  ;; ["https?://.*status-im.*" chrmc]
                  ;; ["https?://.*status.im.*" chrmc]
                  ;; ["peakon.com" chrmc]
                  ;; ["expensify.com" chrmc]
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

   :default_handler ffd
   ;; :default_handler orion
   }})

(comment
 (local col hs.drawing.color.x11))

;;;; Exports

config
