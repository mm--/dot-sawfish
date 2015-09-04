#!/bin/bash
# Sometimes I don't want to enter in my password if I'm leaving for that long.
MYPID=$$
( xtrlock && pkill fprintd-verify && kill $MYPID) &
until fprintd-verify | tee /dev/stderr | grep verify-match
do
    # Ah ah ah, you didn't say the magic word!
    # mplayer -fs ~/video/magicword.mp4
    sleep 1
done
xset dpms force on
pkill xtrlock
