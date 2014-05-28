#!/bin/bash

# May need to fix this to only stop pianobar when I'M playing it.
if pgrep -u `whoami` -x pianobar
then
    echo -n "$@" > ~/.config/pianobar/ctl
else
    exit 1
fi
