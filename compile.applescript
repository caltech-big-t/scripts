#!/usr/bin/osascript
display notification "Build started."
set root to "macOS Root:Users:kisaayano:Library:CloudStorage:Box-Box:Big T Yearbook:2021-2022:spreads:"
use scripting additions
use framework "Foundation"

on getItemsIn:posixPathOfFolder searchedExtensions:theList
	-- make URL
	set theNSURL to current application's class "NSURL"'s fileURLWithPath:posixPathOfFolder
	-- make file manager
	set theFileManager to current application's NSFileManager's new()
	-- get all except hidden items and items within packages
	set allURLs to (theFileManager's enumeratorAtURL:theNSURL includingPropertiesForKeys:{"path"} options:((current application's NSDirectoryEnumerationSkipsPackageDescendants) + (current application's NSDirectoryEnumerationSkipsHiddenFiles as integer)) errorHandler:(missing value))'s allObjects()
	set thePred to current application's NSPredicate's predicateWithFormat:"pathExtension IN[c] %@" argumentArray:{theList}
	set theURLs to allURLs's filteredArrayUsingPredicate:thePred
	-- convert URLs to POSIX paths 
	return (theURLs's valueForKey:"path") as list
end getItemsIn:searchedExtensions:

set posixPathOfFolder to POSIX path of root
set listOfPaths to my getItemsIn:posixPathOfFolder searchedExtensions:{"indd"}

repeat with fil in listOfPaths
	try
		tell application "Finder"
			set fInfo to info for fil
			set fExt to name of fInfo
			set fDir to name of container of (fil as POSIX file as alias)
			set fName to text 1 thru ((fExt's length) - (offset of "." in ¬
				(the reverse of every character of fExt) as text)) of fExt
		end tell
		
		with timeout of 3600 seconds
			tell application "Adobe InDesign 2022"
				set user interaction level of script preferences to never interact
				set buildDoc to open fil without showing window
				# set buildDoc to open fil
				set saveDir to "macOS Root:Users:kisaayano:Documents:builds:" & fDir & ":"
				set saveLoc to saveDir & fName & ".pdf"
				set makeDir to POSIX path of saveDir
				do shell script "mkdir -p " & quoted form of makeDir
				
				tell buildDoc
					export format PDF type to saveLoc using "BalfourExport"
				end tell
				close buildDoc saving no
				set user interaction level of script preferences to interact with all
			end tell
		end timeout
	on error e
		display notification "Document " & fName & " Got Error " & e
	end try
end repeat
display notification "Spread compile completed."
