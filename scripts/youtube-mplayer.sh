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
echo Downloading from url $YTURL

OUTPUT=$(youtube-dl -g -e -o "$STRTEMP" --cookies=/tmp/ytcookie.txt $OTHERARGS --get-filename -v "$YTURL")
if [ -n "$DOWNLOAD" ]; then
   SAVELOC=$(echo "$OUTPUT" | sed -n '3p')
   echo Saving in location $SAVELOC
fi

echo Getting user agent
USERAGENT=$(youtube-dl --dump-user-agent)
echo User agent is $USERAGENT
echo Getting title
TITLE=$(echo "$OUTPUT" | sed -n '1p')
echo Title: $TITLE
echo Getting download URL
URL=$(echo "$OUTPUT" | sed -n '2p')
echo URL: $URL
if [ -n "$DOWNLOAD" ] || echo "$URL" | grep "https"; then
    echo "Using wget"
    wget --load-cookies /tmp/ytcookie.txt -U "$USERAGENT" "$URL" -O - | tee "$SAVELOC" | mplayer -fixed-vo -geometry -0-0 -cache 8192 -title "$TITLE" -
else
    echo "Mplayer direct stream"
    mplayer -fixed-vo -geometry -0-0 -cookies -cookies-file /tmp/ytcookie.txt -user-agent "$USERAGENT" -title "$TITLE" "$URL"
fi
sleep 10

