-------------------
-- Launcher Mode --
-------------------

--
-- This feature assumes you have instructed Karabiner-Elements to map right_command to f19:
-- {
--     "profiles": [
--         {
--             "name": "Default profile",
--             "selected": true,
--             "simple_modifications": {
--                 "right_command": "f19"
--             }
--         }
--     ]
-- }
--

local mod={}

local launcherModeBindings = {
  c = "Google Chrome",
  r = "Reeder",
  a = "Activity Monitor",
  m = "Mail",
  e = "Emacs",
  t = "Tower",
  b = "BearyChat",
  p = "mpv",
  f = "Finder",
  u = "Sublime Text",
  t = "iTerm2",
}

local hints = nil

local inLauncherMode = false

rcmd_down_listener = hs.eventtap.new({ hs.eventtap.event.types.keyDown }, function(event)
    local keyCode = event:getKeyCode()
    local characters = event:getCharacters()

    local isRepeat = event:getProperty(hs.eventtap.event.properties.keyboardEventAutorepeat)
    if keyCode == 80 and isRepeat == 0 and inLauncherMode == false then
      inLauncherMode = true
    end

    if inLauncherMode then
      return true
    end
end)

rcmd_up_listener = hs.eventtap.new({ hs.eventtap.event.types.keyUp }, function(event)
    local keyCode = event:getKeyCode()
    local characters = event:getCharacters()

    if keyCode == 80 then
      inLauncherMode = false
    end

    local appToLaunch = nil

    if inLauncherMode then
      appToLaunch = launcherModeBindings[characters]

      if appToLaunch ~= nil then
        toggleApp(appToLaunch)
      end
    end

    return appToLaunch ~= nil
end)

function toggleApp(hint)
  local app = hs.application.get(hint)
  if not app then
    hs.application.open(hint)
    return
  end
  local mainwin = app:mainWindow()
  if mainwin then
    if mainwin == hs.window.focusedWindow() then
      mainwin:application():hide()
    else
      hs.application.launchOrFocus(hint)
    end
  else
    hs.application.launchOrFocus(hint)
  end
end

function mod.init()
  rcmd_down_listener:start()
  rcmd_up_listener:start()
end

return mod

-------------------
--/Launcher Mode/--
-------------------
