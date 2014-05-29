#!/bin/bash

# create variables
while read L; do
	k="`echo "$L" | cut -d '=' -f 1`"
	v="`echo "$L" | cut -d '=' -f 2`"
	export "$k=$v"
done < <(grep -e '^\(title\|artist\|album\|stationName\|songStationName\|pRet\|pRetStr\|wRet\|wRetStr\|songDuration\|songPlayed\|rating\|coverArt\|stationCount\|station[0-9]*\)=' /dev/stdin) # don't overwrite $1...

FIFO="$HOME/.sawfish/scripts/myfifo"
case "$1" in
	songstart)
		echo "PIANOBAR:True" > "$FIFO"
		echo "PIANOARTIST:$artist" > "$FIFO"
		echo "PIANOTITLE:$title" > "$FIFO"
		echo "UPDATE: please" > "$FIFO"
		;;
esac
