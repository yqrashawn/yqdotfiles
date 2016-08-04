source includes.sh

#Run automator workflow to get Finder selection
selected=$(automator get_selection.workflow)

if [ -z "$selected" ]; then
	echo "Nothing selected."
	exit
fi


#parse workflow output

IFS=$'\n'

files=($selected)

for file in ${files[@]}; do
	if [ $file = "(" ] || [ $file = ")" ]; then	#remove 1st and last "wrap" lines.
		continue
	else
		item="$file"
		if [ ${item#${item%?}} = , ]; then
			item=${item:3:$((${#item}-5))}	#Remove initial padding and final punctuation from paths
		else
			item=${item:3:$((${#item}-4))}	#Remove initial padding and final punctuation from paths
		fi
		#Start building string to pass to next script
		if [ -z "$array_output" ]; then
			array_output="$item"
		else
			array_output="$array_output"'\t'"$item"
		fi
	fi
done

#Send array string to main script
./move_files.sh "$array_output"