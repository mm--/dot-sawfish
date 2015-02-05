#!/bin/bash

stalonetray -bg "black" --geometry 10x1-0+0 --grow-gravity W --icon-gravity NE --icon-size 16 --dockapp-mode simple &
synclient VertEdgeScroll=1 HorizTwoFingerScroll=1
nm-applet &
/usr/lib/notification-daemon/notification-daemon &
~/code/sh/lockscreen-demon.sh &
xset r rate 200 60 &
emacs &
