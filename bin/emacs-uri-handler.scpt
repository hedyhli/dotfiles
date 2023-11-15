on open location this_URL
	set thefile to (text 9 thru (count this_URL) of this_URL)
	do shell script "/usr/local/bin/emacsclient -c " & thefile
	return
end open location
