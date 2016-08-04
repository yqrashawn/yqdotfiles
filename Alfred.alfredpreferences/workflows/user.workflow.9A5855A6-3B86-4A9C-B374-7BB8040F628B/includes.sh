##includes for DeskDrawer scripts

#Enable aliases for this script
shopt -s expand_aliases

#Case-insensitive matching
shopt -s nocasematch

#define aliasesD
alias growlnotify='/usr/local/bin/growlnotify DeskDrawer --image icon.png -m '

desktop="$HOME/Desktop"

#Working Directories
DDWD="$HOME/Library/Caches/com.runningwithcrayons.Alfred-2/Workflow Data/carlosnz.deskdrawer"
DDPREFS="$HOME/Library/Application Support/Alfred 2/Workflow Data/carlosnz.deskdrawer"

#First run check
if [ ! -e "$DDPREFS/config" ]; then
	mkdir "$DDPREFS"
	mkdir "$DDWD"
	ditto -xk DeskDrawer.zip . #Extracts DeskDrawer folder (with custom icon)
	cp -R -p DeskDrawer "$HOME"
	echo "$HOME/DeskDrawer" > "$DDPREFS/config"
	growlnotify "Welcome to DeskDrawer! (v1.5)"
fi

#Load path to the user's DeskDrawer folder.
deskdrawer=$(cat "$DDPREFS/config")

#DeskDrawer exist?
if [ ! -e "$deskdrawer" ]; then
	#If not, recreate it from defaults
	mkdir "$DDWD"
	cp -R -p DeskDrawer "$DDWD"
	echo "$DDWD/DeskDrawer" > "$DDPREFS/config"
fi

#Upgrade to v1.5 Check (by testing and moving hidden Icon file)
if [ ! -e "$deskdrawer"/Icon? ]; then
	ditto -xk DeskDrawer.zip . #Extracts DeskDrawer folder (with custom icon)
	mkdir "$desktop/temp_dd"
	mv "$deskdrawer"/* "$desktop/temp_dd"
	rm -f -d "$deskdrawer"
	cp -R -p DeskDrawer "$(dirname "$deskdrawer")"
	rm -f "$deskdrawer"/Sample.txt
	mv "$desktop/temp_dd"/* "$deskdrawer"
	rmdir "$desktop/temp_dd"
	#If drawer in old default location (Workflow Data), move DD to Home folder.
	if [ "$deskdrawer" = "$DDWD/DeskDrawer" ]; then
		mv "$deskdrawer" "$HOME"
		echo "$HOME/DeskDrawer" > "$DDPREFS/config"
		growlnotify "Welcome to DeskDrawer v1.5!"$'\n'"DeskDrawer has been moved to your Home folder."
	else	
		growlnotify -n "Welcome to DeskDrawer v1.5!"
	fi
fi