on run argv
    tell application "Fantastical 2"
        set locationUrl to "x-fantastical2://show/calendar/" & (item 1 of argv)
        activate
        open location locationUrl
    end tell
    tell application "System Events"
        tell process "Fantastical 2"
            tell menu bar 1
                tell menu bar item "View"
                    tell menu "View"
                        click menu item "By Week"
                    end tell
                end tell
            end tell
        end tell
    end tell
end run
