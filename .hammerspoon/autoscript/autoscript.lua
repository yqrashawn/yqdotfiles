local cmdArr = {
    "cd /Users/yqrashawn/workspace/conflux-portal && git fetch -all",
    "cd /Users/yqrashawn/workspace/react-ui && git fetch -all"
}

function shell(cmd)
    result = hs.osascript.applescript(string.format('do shell script "%s"', cmd))
end

function runAutoScripts()
    for key, cmd in ipairs(cmdArr) do
        shell(cmd)
    end
end


hs.timer.doEvery(3600, runAutoScripts)
