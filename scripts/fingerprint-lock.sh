#!/bin/bash
# Sometimes I don't want to enter in my password if I'm leaving for that long.
MYPID=$$
EXTRAARGS=""
if [ ! -z $1 ]
then
    EXTRAARGS="-i $1"
fi
( i3lock -d -n $EXTRAARGS && pkill fprintd-verify && kill $MYPID) &
until fprintd-verify | grep verify-match
do
    # Ah ah ah, you didn't say the magic word!
    # mplayer -fs ~/video/magicword.mp4
    sleep 1
done
xset dpms force on
pkill i3lock
