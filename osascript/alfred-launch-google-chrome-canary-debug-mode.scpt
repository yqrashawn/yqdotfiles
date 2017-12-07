set appName to "Google Chrome Canary"
set appID to (bundle identifier of (info for (path to application appName)))
tell application "System Events"
	if not (exists process appName) then
		do shell script "/Applications/Google Chrome Canary.app/Contents/MacOS/Google Chrome Canary --remote-debugging-port=9222"
	else
		if not (frontmost of process appName) then
			set frontmost of process appName to true
		end if
	end if
end tell
