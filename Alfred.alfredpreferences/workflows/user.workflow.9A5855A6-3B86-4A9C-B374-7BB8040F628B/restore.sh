source includes.sh

if [[ $(ls "$deskdrawer") = Icon? ]] || [ ! $(ls "$deskdrawer") ]; then
	echo -n "Your DeskDrawer is empty."$'\n'"Nothing to restore."
	exit
else
	ITEMS="$deskdrawer"/*
	for f in $ITEMS
	do
		if [[ "$f" = "$deskdrawer"/Icon? ]]; then #Don't move Icon file
			continue
		fi
		mv -n "$f" "$desktop"
	done
	rm -f "$DDWD/undo.log" #Delete Undo log
	echo -n "Desktop items restored."$'\n'"Enjoy the clutter!"
fi