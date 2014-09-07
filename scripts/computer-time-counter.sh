#!/bin/bash
# This is a a small script made to track and limit how much time I
# spend on my computer each day. If I exceed a threshold for the day, the computer goes to sleep.
POLLTIME=25
if [ -n "$2" ]; then
    POLLTIME=$1
fi
COUNTER=0
if [ -n "$1" ]; then
    COUNTER=$(($1 * 60))
fi
THRESHHOURS=12
THRESHOLD=$((THRESHHOURS * 60 * 60))

function getdate {
    date +"%Y-%m-%d"
}

DATE=`getdate`
LOCKED="FALSE"
SAVEDTIME=`date +"%Y-%m-%d %H:%M"`

while :
do
    if pgrep i3lock ||
       pgrep xtrlock
    then
	echo "Screen locked"
	LOCKED="TRUE"
    else
	NEWDATE=`getdate`
	LOCKED="FALSE"
	if [ "$NEWDATE" == "$DATE" ]
	then
	    COUNTER=$((COUNTER + POLLTIME))
	else
	    echo "Resetting counter"
	    echo "Write to CSV"
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
    
    UNIXTIME=`date +"%s"`
    THETIME=`date +"%Y-%m-%d %H:%M"`
    if [ "$SAVEDTIME" != "$THETIME" ]
    then
	SAVEDTIME="$THETIME"
	echo "\"$DATE\"","\"$THETIME\",$UNIXTIME,$COUNTER,$LOCKED,$POLLTIME" >> ~/.sawfish/data/computertime.csv
    fi

    echo "TIME IS $THETIME"
    echo $((UNIXTIME - COUNTER)) > ~/.sawfish/data/computer_virtual_start.txt
    echo "COMPUTERTIME:$COUNTER"
    echo "COMPUTERTIMELEFT:"$((THRESHOLD - COUNTER)) | tee -a ~/.sawfish/scripts/myfifo
    sleep $POLLTIME
done

