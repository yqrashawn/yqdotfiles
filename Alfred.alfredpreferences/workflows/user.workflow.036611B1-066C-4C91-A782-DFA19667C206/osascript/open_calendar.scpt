on run argv
    tell application "Calendar"
        set date_str to (item 1 of argv)
        set date_list to every word of date_str
        set show_date to current date

        set year of show_date to (item 1 of date_list) as integer
        set month of show_date to (item 2 of date_list) as integer
        set day of show_date to (item 3 of date_list) as integer

        activate
        switch view to week view
        view calendar at show_date

    end tell
end run
