#!/bin/bash

STRTEMP="$HOME/video/youtube/%(title)s-%(id)s.%(ext)s"

OTHERARGS=""
SAVELOC="/dev/null"
while getopts "md" arg; do
case $arg in
    m) OTHERARGS="--max-quality 18"
       ;;
    d) DOWNLOAD="1"
       ;;
    esac
done

YTURL=$(xclip -o)

if [ -n "$DOWNLOAD" ]; then
   SAVELOC=$(youtube-dl -o "$STRTEMP" $OTHERARGS --get-filename "$YTURL")
fi

USERAGENT=$(youtube-dl --dump-user-agent)
URL=$(youtube-dl --cookies=/tmp/ytcookie.txt $OTHERARGS -g "$YTURL")
if [ -n "$DOWNLOAD" ] || echo "$URL" | grep "https"; then
    wget --load-cookies /tmp/ytcookie.txt -U "$USERAGENT" "$URL" -O - | tee "$SAVELOC" | mplayer -fixed-vo -geometry -0-0 -cache 8192 -
else
    mplayer -fixed-vo -geometry -0-0 -cookies -cookies-file /tmp/ytcookie.txt -user-agent "$USERAGENT" "$URL"
fi
