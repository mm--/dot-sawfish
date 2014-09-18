#!/bin/bash

# May need to fix this to only stop pianobar when I'M playing it.
if pgrep -u `whoami` -x espeak
then
    pkill espeak
else
    TMPWAV=`mktemp ${TMPDIR:-/tmp}/espeak.XXXXXX.wav`
    xclip -o | espeak -a 200 -v english-us -s "$@" --stdout > "$TMPWAV"
    mpv --input-file=$HOME/.sawfish/pipes/mplayerfifo -af scaletempo "$TMPWAV"
    rm "$TMPWAV"
fi
