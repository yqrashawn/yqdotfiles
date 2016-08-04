source includes.sh

#Load contents of protect.log
if [ -e "$DDWD/protect.log" ]; then
	protected=$(cat "$DDWD/protect.log")
fi

#Clean existing protect.log and delete dead entries
if [ "$protected" ]; then
OLD_IFS=$IFS
IFS=$'\n'
for line in $protected
do
	if [ -e "$desktop/$line" ]; then
		echo "$line" >> "$DDWD/temp_protect.log"
	fi
done
mv -f "$DDWD/temp_protect.log" "$DDWD/protect.log"
fi
IFS=$OLD_IFS

if [ ! $(ls "$desktop") ]; then
	echo "Your Desktop is already clean."
	exit
else
	#Delete old UNDO log
	rm -f "$DDWD/undo.log"
	
	#Load Desktop items
	ITEMS="$desktop"/*
	for f in $ITEMS
	do
		if [ "$f" = "$HOME/Desktop/DeskDrawer" ]; then #Don't try and move DeskDrawer itself
			continue
		fi
		#Loop through protect.log and don't move if match
		if [ "$protected" ]; then
			OLD_IFS=$IFS
			IFS=$'\n'
			for line in $protected
			do
				if [ "$line" =  "$(basename "$f")" ]; then
					match=1
					continue
				fi
			done
			IFS=$OLD_IFS
		fi
		if [ $match = 1 ]; then
			match=
			continue
		else
			mv -n "$f" "$deskdrawer"
			latest_item=$(basename "$f")
			echo "$latest_item" >> "$DDWD/undo.log"
		fi
	done
	echo -n "Whoosh! All Desktop items moved to your DeskDrawer."
fi