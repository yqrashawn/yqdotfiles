source includes.sh

#Ask for folder (via Applescript GUI)
user_path=$(osascript <<-EOF

tell application "System Events"
	activate
	set folderName to POSIX path of (choose folder with prompt "Where would you like to store your DeskDrawer folder?")
end tell

EOF)

if [ -z $user_path ]; then
	exit
fi

#Move DeskDrawer folder
mv "$deskdrawer" "$user_path"

#If file operation successful
if [ $? = 0 ]; then
	#Update config file with new path
	echo "$user_path/DeskDrawer" > "$DDPREFS/config"
	#Notify
	echo -n "DeskDrawer successfully moved to \"$user_path\"."
else
	#Notify
	echo -n "Sorry, unable to move to that location."
fi

exit