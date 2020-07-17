print('==================================================')
-- Spoons
ReloadConfiguration = hs.loadSpoon('ReloadConfiguration')
ReloadConfiguration:start()
ReloadConfiguration:bindHotkeys({reloadConfiguration = {{"cmd", "ctrl"}, "r"} })

require "hotkey.hotkey"
require("autoscript.autoscript")
require "vscode.vscode"
-- require "ime.ime"
-- require "wifi.wifi"
-- require "window.window"
-- require "clipboard.clipboard"
-- require "statuslets.statuslets"