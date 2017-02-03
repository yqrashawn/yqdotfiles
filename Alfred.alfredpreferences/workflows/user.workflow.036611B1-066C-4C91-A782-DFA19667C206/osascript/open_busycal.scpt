on run argv
    tell application "BusyCal"
        set locationUrl to "busycalevent://date/" & (item 1 of argv)
        activate
        open location locationUrl
    end tell
    tell application "System Events"
        tell process "BusyCal"
            tell menu bar 1
                tell menu bar item "View"
                    tell menu "View"
                        click menu item "Week"
                    end tell
                end tell
            end tell
        end tell
    end tell
end run
