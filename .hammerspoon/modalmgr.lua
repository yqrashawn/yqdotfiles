modalmgr_keys = modalmgr_keys or {{"alt"}, "space"}
modalmgr = hs.hotkey.modal.new(modalmgr_keys[1], modalmgr_keys[2], 'Toggle Modal Supervisor')
function modalmgr:entered()
  dock_launched = true
  modal_stat('dock',black)
  if resizeM then
    if launch_resizeM == nil then launch_resizeM = false end
    if launch_resizeM == true then resizeM:enter() end
  end
  if idle_to_which == nil then idle_to_which = "netspeed" end
  if idle_to_which == "netspeed" then
    idletimer = hs.timer.doEvery(5,function()
                                   if modal_text == 'DOCK MODE' and netspeedM then
                                     netspeedM:enter()
                                   end
    end)
  elseif idle_to_which == "hide" then
    modal_show:hide()
    modal_bg:hide()
  end
end

function modalmgr:exited()
  exit_others(nil)
  dock_launched = nil
  modal_bg:hide()
  modal_show:hide()
  if idletimer ~= nil then idletimer:stop() end
  if hotkeytext then
    hotkeytext:delete()
    hotkeytext=nil
    hotkeybg:delete()
    hotkeybg=nil
  end
end

-- alt space toggle mode
-- modalmgr:bind(modalmgr_keys[1], modalmgr_keys[2], "Toggle Modal Supervisor", function() modalmgr:exit() end)

if appM then
  modalmgr:bind("", "F13", 'APPLICAON', function() exit_others(appM) appM:enter() end)
end

if alfredM then
  alfredM_keys = alfredM_keys or {"", "F17"}
  if string.len(alfredM_keys[2]) > 0 then
    modalmgr:bind(alfredM_keys[1], alfredM_keys[2], 'ALFRED', function() exit_others(alfredM) alfredM:enter() end)
  end
end
-- if resizeM then
--   resizeM_keys = resizeM_keys or {"alt", "R"}
--   if string.len(resizeM_keys[2]) > 0 then
--     modalmgr:bind(resizeM_keys[1], resizeM_keys[2], 'Enter Resize Mode', function() exit_others(resizeM) resizeM:enter() end)
--   end
-- end
-- toggleconsole_keys = toggleconsole_keys or {"alt", "Z"}
-- if string.len(toggleconsole_keys[2]) > 0 then
--     modalmgr:bind(toggleconsole_keys[1], toggleconsole_keys[2], 'Toggle Hammerspoon Console', function() hs.toggleConsole() end)
-- end

if modalmgr then
  if launch_modalmgr == nil then launch_modalmgr = true end
  if launch_modalmgr == true then modalmgr:enter() end
end
