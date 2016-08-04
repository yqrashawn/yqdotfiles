source includes.sh

#Put incoming into proper tab-delimited variable (for the Finder Selection string)
array=$(echo -e "$1")

##Put multiple files into array

#Set IFS variable to only separate on tabs
IFS=$'\t'

#Create an array with the tab-delimited files passed from Alfred
filesFromAlfred=("$array")

#Delete old UNDO log
rm -f "$DDWD/undo.log"

#Loop through each file in the array
for file in ${filesFromAlfred[@]}; do
	#Skip if DeskDrawer itself is on the Desktop
	if [ "$file" = "$desktop/DeskDrawer" ]; then
		continue
	fi
	#Skip if file with same name already exists
	if [ -e "$deskdrawer/$(basename "$file")" ]; then
		continue
	fi
	mv -n $file $deskdrawer
	if [ $? = 0 ]; then	#If successful moving
		latest_item="$file"
		echo "$latest_item" >> "$DDWD/undo.log"	#Record item to Undo log
		let count++
	fi
done

#Reset IFS to its original state
unset IFS

if [ $count = ]; then
	echo -n "No items moved."$'\n'"Check for duplicates."
	exit
fi
if [ $count = 1 ]; then
	echo -n "$(basename "$latest_item") moved to DeskDrawer."
else
	echo -n "$count items moved to DeskDrawer."
fi

#TO THINK ABOUT: Do we accept non-Desktop files for moving? By default, yes, but might add option to only move Desktop files.