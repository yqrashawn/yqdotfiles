on run argv
  set fileToDelete to POSIX file (item 1 of argv)
  tell application "Finder"
    delete fileToDelete
  end tell
end run
