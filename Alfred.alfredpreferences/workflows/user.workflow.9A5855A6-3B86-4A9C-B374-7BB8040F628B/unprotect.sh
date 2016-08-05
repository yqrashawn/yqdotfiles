source includes.sh

#Put incoming into proper tab-delimited variable (for the Finder Selection string)
array=$(echo -e "$1")


##Put multiple files into array

#Set IFS variable to only separate on tabs
#IFS=$'\t'

#Create an array with the tab-delimited files passed from Alfred
filesFromAlfred=("$array")

#Loop through existing protection list (if it exists)
if [ -e "$DDWD/protect.log" ]; then
	OLD_IFS=$IFS
	IFS=$'\n'
	for line in $(cat "$DDWD/protect.log")
	do	
		if [ -e "$desktop/$line" ]; then	#if file still exists
			#Loop through each file in the array
			IFS=$'\t'
			for file in ${filesFromAlfred[@]}
			do
				#Is there a match?
				if [ "$line" =  $(basename "$file") ]; then
					match=1
					let count++
					last_match="$line"
				fi
			done
			if [[ "$match" != 1 ]]; then
				echo "$line" >> "$DDWD/temp_protect.log"
			else
				match=
			fi		
			match=
		fi
	done
	IFS=$OLD_IFS
	#Replace protection list (or delete if it's empty)
	if [ ! -e "$DDWD/temp_protect.log" ]; then
		rm -f "$DDWD/protect.log"
	else
		mv -f "$DDWD/temp_protect.log" "$DDWD/protect.log"
	fi
fi

if [ $count = ]; then
	echo "No items unprotected."$'\n'"Maybe try \"deskdrawer help\"?"
	exit
fi
if [ $count = 1 ]; then
	echo -n "$last_match no longer protected from DeskDrawer sweeps."
else
	echo -n "$count items no longer protected from DeskDrawer sweeps."
fi