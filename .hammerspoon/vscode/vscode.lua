k = hs.hotkey.modal.new('cmd-shift', 'd')
function k:entered()
  hs.alert'Entered mode'
end
function k:exited()
  hs.alert'Exited mode'
end
k:bind('', 'escape', function() k:exit() end)
k:bind('', 'J', 'Pressed J', function() print'let the record show that J was pressed' end)
