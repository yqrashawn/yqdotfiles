on run argv
    set localUrl to "https://www.google.com/calendar/render?tab=wc\\&date=" & (item 1 of argv)
    set cmd to "open " & (localUrl)
    do shell script cmd
end run
