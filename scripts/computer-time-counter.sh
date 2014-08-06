#!/bin/bash
# This is a a small script made to track and limit how much time I
# spend on my computer each day. If I exceed a threshold for the day, the computer goes to sleep.
POLLTIME=25
if [ -n "$1" ]; then
    POLLTIME=$1
fi
COUNTER=0
if [ -n "$2" ]; then
    COUNTER=$(($1 * 60))
fi
THRESHHOURS=5
THRESHOLD=$((THRESHHOURS * 60 * 60))

function getdate {
    date +"%Y-%m-%d"
}

DATE=`getdate`

while :
do
    if pgrep i3lock ||
       pgrep xtrlock
    then
	echo "Screen locked"
    else
	NEWDATE=`getdate`
	if [ "$NEWDATE" == "$DATE" ]
	then
	    COUNTER=$((COUNTER + POLLTIME))
	else
	    echo "Resetting counter"
	    echo "Write to CSV"
	    echo "\"$DATE\",$COUNTER" >> ~/.sawfish/data/computertime.csv
	    COUNTER=0
	    DATE=`getdate`
	fi
	if ((COUNTER > THRESHOLD))
	then
	    echo "THRESHOLD EXCEEDED"
	    sudo pm-suspend
	    sleep 300		# If we re-awake, give 5 minutes time?
	fi
    fi
    echo "COMPUTERTIME:$COUNTER" | tee -a ~/.sawfish/scripts/myfifo
    UNIXTIME=`date +"%s"`
    echo "COMPUTERVIRTUALSTART:"$((UNIXTIME - COUNTER)) | tee -a ~/.sawfish/scripts/myfifo
    echo "COMPUTERTIMELEFT:"$((THRESHOLD - COUNTER)) | tee -a ~/.sawfish/scripts/myfifo
    echo "UPDATE: please" >> ~/.sawfish/scripts/myfifo
    sleep $POLLTIME
done

