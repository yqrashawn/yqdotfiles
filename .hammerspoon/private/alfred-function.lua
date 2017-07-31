------------------------------------------------------------- Alfred function
alfredM = hs.hotkey.modal.new()
table.insert(modal_list, alfredM)
function alfredM:entered()
  modal_stat('alfred',osx_yellow)
  if hotkeytext then
    hotkeytext:delete()
    hotkeytext=nil
    hotkeybg:delete()
    hotkeybg=nil
  end
  if not show_applauncher_tips then show_applauncher_tips = true end
  if show_applauncher_tips == true then showavailableHotkey() end --app mode show hotkey hint
end
function alfredM:exited()
  if dock_launched then
    modal_stat('dock',black)
  else
    modal_bg:hide()
    modal_show:hide()
  end
  if idle_to_which == "hide" then
    modal_bg:hide()
    modal_show:hide()
  end
  if hotkeytext then
    hotkeytext:delete()
    hotkeytext=nil
    hotkeybg:delete()
    hotkeybg=nil
  end
end
alfredM:bind('', 'escape', function() alfredM:exit() end)
alfredM:bind('', 'Q', function() alfredM:exit() end)
alfredM:bind('', 'tab', function() showavailableHotkey() end)

if not scriptList then
  scriptList = {
    {name = 'Open in Marta',shortcut = 'f',source = 'tell application "Alfred 3" to run trigger "openInMarta" in workflow "com.yqrashawn.open.in.marta" with argument ""'},
    {name = 'Browser Tabs',shortcut = 'b',source = 'tell application "Alfred 3" to run trigger "browserTabs" in workflow "com.clintonstrong.SearchTabs" with argument "" '},
    {name = 'Tower repo',shortcut = 't',source = 'tell application "Alfred 3" to run trigger "towerRepo" in workflow "net.cjlucas.alfred.tower" with argument ""'},
    {name = 'Kill process',shortcut = 'k',source = 'tell application "Alfred 3" to run trigger "killProcess" in workflow "com.ngreenstein.alfred-process-killer" with argument ""'},
    {name = 'Search Github',shortcut = 'g',source = 'tell application "Alfred 3" to run trigger "searchGithub" in workflow "nikivi.search-the-web" with argument ""'},
    {name = 'Recent Download',shortcut = 'r',source = 'tell application "Alfred 3" to run trigger "recentDownloads" in workflow "com.vitorgalvao.alfred.recentdownloads" with argument ""'},
    {name = 'Color',shortcut = 'c',source = 'tell application "Alfred 3" to run trigger "feedback" in workflow "tylereich.colors" with argument ""'},
    -- {name = 'Files',shortcut = 'f',source = 'tell application "System Events"\
    --   key code 49 using {command down}\
    --   delay 0.1\
    --   keystroke "f "\
    --   end tell '},
    -- {name = 'Edit in Emacs',shortcut = 'e',source = 'tell application "Alfred 3" to run trigger "editInEmacs" in workflow "com.sztoltz.editwith" with argument ""'},
    {name = 'Finder to Iterm',shortcut = 'i',source = ' tell application "Alfred 3" to run trigger "finderToIterm" in workflow "de.leenno.terminalfinder" with argument ""'},
    {name = 'Github Stars',shortcut = 'm',source = 'tell application "Alfred 3" to run trigger "ghmystars" in workflow "de.gh01.alfred.github" with argument " my stars"'},
    {name = 'YouDao',shortcut = 'F17',source = 'tell application "Alfred 3" to run trigger "youdao" in workflow "whyliam.workflows.youdao" with argument ""'},
    {name = 'BitBar todo',shortcut = 'd',source = 'tell application "Alfred 3" to run trigger "addtodo" in workflow "com.alfredapp.vero.examplelistfilter" with argument ""'},
  }
end

for i = 1, #scriptList do
  alfredM:bind('', scriptList[i].shortcut, scriptList[i].name, function()
                 hs.osascript.applescript(scriptList[i].source)
                 alfredM:exit()
                 if hotkeytext then
                   hotkeytext:delete()
                   hotkeytext=nil
                   hotkeybg:delete()
                   hotkeybg=nil
                 end
  end)
end
