#!/bin/bash

# May need to fix this to only stop pianobar when I'M playing it.
if pgrep -u `whoami` -x pianobar
then
    echo -n "$1" > ~/.config/pianobar/ctl
elif pgrep -u `whoami` -f "mplayerfifo"
then
    echo "PAUSING"
    echo "${*:2}" > ~/.sawfish/pipes/mplayerfifo
else
    exit 1
fi
