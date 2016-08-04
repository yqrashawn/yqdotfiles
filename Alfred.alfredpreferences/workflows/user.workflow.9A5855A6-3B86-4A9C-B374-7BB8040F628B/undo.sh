source includes.sh

#If undo.log doesn't exist
if [ ! -e "$DDWD/undo.log" ]; then
	echo "Nothing to Undo."
	exit
fi


#Process lines in undo.log
OLD_IFS=$IFS
IFS=$'\n' #Use newline as delimiter for text file parsing
items=($(cat "$DDWD/undo.log"))
for line in "${items[@]}"
	do
		filename="$(basename "$line")"	#Get filename from full path
		#Skip if file with same name already exists
		if [ -e "$desktop/$filename" ]; then
			continue
		fi
		if [ -e "$deskdrawer/$filename" ]; then
			mv -n "$deskdrawer/$filename" "$line"
			if [ $? = 0 ]; then	#If successful moving
				latest_item="$filename"
				let count++
			fi
		fi
	done
IFS=$OLD_IFS

#Delete undo.log
rm -f "$DDWD/undo.log"

if [ $count = ]; then
	echo -n "No items moved."$'\n'"Check for duplicates."
	exit
fi
if [ $count = 1 ]; then
	echo -n "$latest_item moved back to Desktop."
else
	echo -n "$count items moved back to Desktop."
fi
