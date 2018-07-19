#!/bin/bash

LOCKFILE="$HOME/.sawfish/pipes/lockscreen"
# Mute sound
# See if we're currently muted
# From https://unix.stackexchange.com/questions/61337/testing-from-a-script-if-audio-devices-are-in-silent
MUTED=$(pacmd dump | awk '
  $1 == "set-sink-mute" {m[$2] = $3}
  $1 == "set-default-sink" {s = $2}
  END {print m[s]}')
if echo "$MUTED" | grep "no"; then
    pactl set-sink-mute 0 toggle
fi
pkill -USR1 dunst 		# Pause notifications
touch $LOCKFILE
"$@"
rm -f $LOCKFILE
pkill -USR2 dunst 		# Resume notifications
if echo "$MUTED" | grep "no"; then
    pactl set-sink-mute 0 toggle
fi
