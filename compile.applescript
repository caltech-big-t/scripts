#!/usr/bin/osascript
display notification "Build started."
set root to "MAC DISK:Users:kisaayano:Box:Big T Yearbook:2018-2019:Spreads:"
 
tell application "Finder"
    set fileList to ((every file in the entire contents of folder root whose name ends with ".indd") as alias list)
end tell
 
repeat with fil in fileList
    tell application "Finder"
        set fInfo to info for fil
        set fExt to name of fInfo
        set fDir to name of container of fil
        set fName to text 1 thru ((fExt's length) - (offset of "." in ¬
            (the reverse of every character of fExt) as text)) of fExt
    end tell
   
    tell application "Adobe InDesign CC 2019"
        set buildDoc to open fil without showing window
        set saveDir to "MAC DISK:srv:builds:" & fDir & ":"
        set saveLoc to saveDir & fName & ".pdf"
        set makeDir to POSIX path of saveDir
        do shell script "mkdir -p " & quoted form of makeDir
       
        set user interaction level of script preferences to never interact
        tell buildDoc
            export format PDF type to saveLoc using "YearbookBuild" without showing options
        end tell
        close buildDoc saving no
    end tell
end repeat
 
display notification "Spread compile completed."
