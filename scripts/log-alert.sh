#!/bin/bash
lasttime=0
function alert {
    for i in `seq 1 4`; do
	sleep 0.2
	xbacklight -dec 20
	sleep 0.2
	xbacklight -inc 20
    done
}


(tail --follow=name /var/log/syslog &
 tail --follow=name /var/log/auth.log )| grep --line-buffered -e "scanlogd" -e "sshd" | grep --line-buffered -v -e "signal 15" | while read line; do
    now=`date +%s`
    if (( $now - $lasttime > 5 )); then # Only blink once every 5 seconds
	lasttime=$now
	echo "ALERT:$line"
	alert
    fi
done
