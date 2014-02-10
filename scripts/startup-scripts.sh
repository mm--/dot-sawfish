#!/bin/bash

# conky -c ~/.sawfish/conky/conky_dzen_left.conf | dzen2 -x 63 -w 300 -h 16 -ta 'l' -fn 'fixed:pixelsize=10' -title-name dzenLEFT -dock &
# conky -c ~/.sawfish/conky/conky_dzen_right.conf | dzen2 -x 63 -w 1378 -h 16 -ta 'r' -fn 'fixed:pixelsize=10' -title-name dzenRIGHT -dock -e '' &
~/.sawfish/scripts/dzen_multiplexer.py | dzen2 -x 63 -w 1400 -h 16 -ta 'l' -fn 'fixed:pixelsize=10' -title-name dzenRIGHT -dock -fn 'terminus' -e ''
stalonetray -bg "black" --geometry 10x1-0+0 --grow-gravity W --icon-gravity NE --icon-size 16 --dockapp-mode simple &
conky -c ~/.sawfish/conky/conky_cpu.conf &
fvwm-root -r ~/.fvwm/wallpaper/bridge.png &
synclient VertEdgeScroll=1 HorizTwoFingerScroll=1
nm-applet &
/usr/lib/notification-daemon/notification-daemon &
~/code/sh/log-alert.sh &
xset r rate 200 60 &
