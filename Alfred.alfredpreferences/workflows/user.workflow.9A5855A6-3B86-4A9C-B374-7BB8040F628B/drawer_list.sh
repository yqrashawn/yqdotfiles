source includes.sh

#Populate Alfred results with DeskDrawer files
echo '<?xml version="1.0"?>
	<items>'

search="$1"	#Alfred argument

if [[ $(ls "$deskdrawer") = Icon? ]] || [ ! $(ls "$deskdrawer") ]; then
	if [ ! $(ls "$desktop") ]; then
		echo  '  <item uid="empty" arg="" valid="no">
					<title>DeskDrawer empty</title>
					<subtitle>Your Desktop is pretty clean, too. Impressive!</subtitle>
					<icon>icon.png</icon>
				  </item>
				</items>'
	else
		echo  '  <item uid="sweep" arg="sweep">
					<title>DeskDrawer empty</title>
					<subtitle>Would you like to sweep your Desktop clean now?   ([ENTER], or [ESC] to cancel.)</subtitle>
					<icon>4B5F2BCF-1815-4F7E-9F91-9793AB9A8A5F.png</icon>
				  </item>
				</items>'
		exit
	fi
fi	


for files in "$deskdrawer"/*
	do
		if [ "$files" = "$deskdrawer"/Icon? ]; then #Ignore ICON file
			continue
		fi
		filepath="$files"
		filename=$(basename "$filepath")
	
		#Substring replacement to remove illegal XML characters
		filepath="${filepath//&/&amp;}" filepath="${filepath//</&lt;}" filepath="${filepath//>/&gt;}" filepath="${filepath//\"/&quot;}" filepath="${filepath//\'/&apos;}"
		filename="${filename//&/&amp;}" filename="${filename//</&lt;}" filename="${filename//>/&gt;}" filename="${filename//\"/&quot;}" filename="${filename//\'/&apos;}"
		
		if [[ "$filename" = *"$search"* ]]; then
			if [ -d "$filepath" ]; then	#If it's a directory
				echo  '  <item uid="'$filename'" arg="'$filepath'" type="file">
							<title>'$filename'</title>
							<subtitle>Open folder</subtitle>
							<icon type="fileicon">'$filepath'</icon>
						  </item>'
			else	#It's just a file
				echo  '  <item uid="'$filename'" arg="'$filepath'">
							<title>'$filename'</title>
							<subtitle>Open file</subtitle>
							<icon type="fileicon">'$filepath'</icon>
						  </item>'
			fi
		fi		
	done
echo '</items>'