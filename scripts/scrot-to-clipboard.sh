#!/bin/bash

TMPPNG=`mktemp scrot-XXXXXX.png`

scrot -s "$TMPPNG"
xclip -selection clipboard -t image/png -i < "$TMPPNG"
rm "$TMPPNG"
