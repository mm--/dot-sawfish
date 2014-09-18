#!/bin/bash

TMPWAV=`mktemp ${TMPDIR:-/tmp}/espeak.XXXXXX.wav`
espeak -a 200 -v english-us -s "$@" --stdout > "$TMPWAV"
mpv --input-file=$HOME/.sawfish/pipes/mplayerfifo -af scaletempo "$TMPWAV"
rm "$TMPWAV"
