#!/bin/bash
# Player the current song in MPD with Mplayer
# I only do this because I like to player songs quicker

MUSICDIR="$HOME/music/"
SONGLOCATION=$(mpc -f "%file%" | head -n1)

mpc pause
# mplayer -slave -input file=~/.sawfish/pipes/mplayerfifo -vo null "$MUSICDIR/$SONGLOCATION"

POSITION=$(mpc -f "%position%" | head -n1)

cd $MUSICDIR
mpc playlist -f "%file%" | sed -n "$POSITION"',$p' | mplayer -slave -input file=~/.sawfish/pipes/mplayerfifo -vo null -playlist -

