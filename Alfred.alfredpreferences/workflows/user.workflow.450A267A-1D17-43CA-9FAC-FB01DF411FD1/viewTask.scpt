on run argv
	set q to argv as text
	-- sample: set q to "my task name with spaces #tag1 #tag2 @myarea :mynote with spaces >20150101"
	
	set taskTags to ""
	set taskDesc to ""
	set taskGroup to ""
	set taskGroupType to ""
	set taskNote to ""
	set taskDate to ""

	set myPath to POSIX path of ((path to me as text) & "::")
	set lib to load script POSIX file (myPath & "lib.scpt")
	
	set tps to lib's getTaskParameters(q, false) -- {taskTags, taskDesc, taskGroup, taskGroupType, taskNote, taskDate}
	set taskTags to (item 1 of tps) as text
	set taskDesc to (item 2 of tps) as text
	set taskGroup to (item 3 of tps) as text
	set taskGroupType to (item 4 of tps) as text
	set taskNote to (item 5 of tps) as text
	set taskDate to (item 6 of tps) as text
	if taskDate is not "" then
		set d to lib's getDateFromString(taskDate)
		set iso_d to getISODate(d)
	end if

	-- xml header
	set xml to "<?xml version=\"1.0\"?>" & linefeed
	set xml to xml & "<items>" & linefeed
	
	set c to 0
	
	set xml to xml & linefeed & "<item uid=\"new_task" & c & "\" arg=\"" & q & "\" valid=\"yes\">" & linefeed
	set xml to xml & "<title>"
	set xml to xml & q_encode(taskDesc)
	set xml to xml & "</title>" & linefeed
	set xml to xml & "<subtitle>Description (=)</subtitle>" & linefeed
	set xml to xml & "<icon>icons/task.png</icon>" & linefeed
	set xml to xml & "</item>" & linefeed
	
	
	set c to c + 1
	set xml to xml & linefeed & "<item uid=\"new_task" & c & "\" arg=\"" & q & "\" valid=\"yes\">" & linefeed
	set xml to xml & "<title>"
	if taskGroupType = "" then set xml to xml & "Inbox"

	-- if taskGroupType = "a" or taskGroupType = "p" or taskGroupType = "s" then 
	set xml to xml & q_encode(taskGroup)

	set xml to xml & "</title>" & linefeed
	set xml to xml & "<subtitle>"

	set taskGroupType to lib's check_group(taskGroup) 
	
	if taskGroupType = "" or taskGroupType = "s" then set xml to xml & "Folder (@)"
	if taskGroupType = "a" then set xml to xml & "Area (@)"
	if taskGroupType = "p" then set xml to xml & "Project (@)"
	set xml to xml & "</subtitle>" & linefeed
	set xml to xml & "<icon>icons/group.png</icon>" & linefeed
	set xml to xml & "</item>" & linefeed
	
	set c to c + 1
	set xml to xml & linefeed & "<item uid=\"new_task" & c & "\" arg=\"" & q & "\" valid=\"yes\">" & linefeed
	set xml to xml & "<title>"
	if taskTags is not "" then set xml to xml & q_encode(taskTags)
	set xml to xml & "</title>" & linefeed
	set xml to xml & "<subtitle>Tags (#)</subtitle>" & linefeed
	set xml to xml & "<icon>icons/tag.png</icon>" & linefeed
	set xml to xml & "</item>" & linefeed
	
	set c to c + 1
	set xml to xml & linefeed & "<item uid=\"new_task" & c & "\" arg=\"" & q & "\" valid=\"yes\">" & linefeed
	set xml to xml & "<title>"
	if taskNote is not "" then set xml to xml & q_encode(taskNote)
	set xml to xml & "</title>" & linefeed
	set xml to xml & "<subtitle>Note (:)</subtitle>" & linefeed
	set xml to xml & "<icon>icons/note.png</icon>" & linefeed
	set xml to xml & "</item>" & linefeed
	
	set c to c + 1
	set xml to xml & linefeed & "<item uid=\"new_task" & c & "\" arg=\"" & q & "\" valid=\"yes\">" & linefeed
	set xml to xml & "<title>"
	if taskDate is not "" then set xml to xml & q_encode(iso_d)
	set xml to xml & "</title>" & linefeed
	set xml to xml & "<subtitle>Due date (>)</subtitle>" & linefeed
	set xml to xml & "<icon>icons/date.png</icon>" & linefeed
	set xml to xml & "</item>" & linefeed
	
	return xml & "</items>" & linefeed
	
end run

on getISODate(myDate)
	set y to text -4 thru -1 of ("0000" & (year of myDate))
	set m to text -2 thru -1 of ("00" & ((month of myDate) as integer))
	set d to text -2 thru -1 of ("00" & (day of myDate))
	return y & "-" & m & "-" & d
end getISODate

on q_encode(str)
	-- source: https://github.com/qlassiqa/qWorkflow/blob/master/documentation/Documentation.md
	if class of str is not text or my q_is_empty(str) then return str
	set s to ""
	repeat with sRef in str
		set c to contents of sRef
		if c is in {"&", "'", "\"", "<", ">", tab} then
			if c is "&" then
				set s to s & "&amp;"
			else if c is "'" then
				set s to s & "&apos;"
			else if c is "\"" then
				set s to s & "&quot;"
			else if c is "<" then
				set s to s & "&lt;"
			else if c is ">" then
				set s to s & "&gt;"
			else if c is tab then
				set s to s & "&#009;"
			end if
		else
			set s to s & c
		end if
	end repeat
	return s
end q_encode

on q_is_empty(str)
	-- source: https://github.com/qlassiqa/qWorkflow/blob/master/documentation/Documentation.md
	if str is missing value then return true
	return length of (my q_trim(str)) is 0
end q_is_empty

on q_trim(str)
	-- source: https://github.com/qlassiqa/qWorkflow/blob/master/documentation/Documentation.md
	if class of str is not text or class of str is not string or str is missing value then return str
	if str is "" then return str
	
	repeat while str begins with " "
		try
			set str to items 2 thru -1 of str as text
		on error msg
			return ""
		end try
	end repeat
	repeat while str ends with " "
		try
			set str to items 1 thru -2 of str as text
		on error
			return ""
		end try
	end repeat
	
	return str
end q_trim