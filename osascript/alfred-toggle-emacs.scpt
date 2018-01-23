set appName to "Emacs"

set appID to bundle identifier of (info for (path to application appName))
tell application "System Events"
	if not (exists process appName) then
		tell application appID to activate
	else
		if frontmost of process appName then
			set visible of process appName to false
		else
			set frontmost of process appName to true
		end if
	end if
end tell