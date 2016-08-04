source includes.sh

#Put incoming into proper tab-delimited variable (for the Finder Selection string)
array=$(echo -e "$1")

#Clean existing protect.log and delete dead entries
if [ -e "$DDWD/protect.log" ]; then
OLD_IFS=$IFS
IFS=$'\n'
for line in $(cat "$DDWD/protect.log")
do
	if [ -e "$desktop/$line" ]; then
		echo "$line" >> "$DDWD/temp_protect.log"
	fi
done
mv -f "$DDWD/temp_protect.log" "$DDWD/protect.log"
fi
IFS=$OLD_IFS


##Put multiple files into array

#Set IFS variable to only separate on tabs
IFS=$'\t'

#Create an array with the tab-delimited files passed from Alfred
filesFromAlfred=("$array")

#Loop through each file in the array
for file in ${filesFromAlfred[@]}; do
	#Skip if DeskDrawer itself is selected
	if [ "$file" = "$desktop/DeskDrawer" ]; then
		continue
	fi
	#Check item is not already in protect.log
	if [[ "$(cat "$DDWD/protect.log")" = *"$(basename "$file")"* ]]; then
		#Count it but don't write it again
		latest_item=$(basename "$file")
		let count++
		continue
	fi	
	if [ "$file" = "$desktop/$(basename "$file")" ]; then	#Only protect if actually a Desktop item
		latest_item=$(basename "$file")
		echo "$latest_item" >> "$DDWD/protect.log"
		let count++
	else
		echo "Sorry, protected items must be on the Desktop. (See \"deskdrawer help\".)"
		exit
	fi
done

if [ $count = ]; then
	echo "No items protected."$'\n'"Maybe try \"deskdrawer help\"?"
	exit
fi
if [ $count = 1 ]; then
	echo "$latest_item protected from DeskDrawer sweeps."
else
	echo "$count items protected from DeskDrawer sweeps."
fi