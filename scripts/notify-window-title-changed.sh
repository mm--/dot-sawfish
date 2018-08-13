#!/bin/bash
# Check if a window title changes. Handy to see when a process is done in a terminal.

set -e

# The window PID
WIN="$1"
get_win_name() {
    xprop -id "$1" | grep WM_NAME | sed 's/WM_NAME.* = "\(.*\)"/\1/'
}
notify() {
    # For breaking up newlines
    NOTIFICATION=$(echo -e "$1")
    notify-send "$NOTIFICATION"
}
ORIGNAME=$(get_win_name "$WIN")

notify "Now monitoring:\n\"$ORIGNAME\""
NEWNAME="$ORIGNAME"
while [ "$ORIGNAME" = "$NEWNAME" ];
do
    sleep 1
    NEWNAME=$(get_win_name "$WIN")
done

notify "\"$ORIGNAME\" finished.\n(Now \"$NEWNAME\")"
