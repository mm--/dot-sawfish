#!/bin/bash

STRTEMP="$HOME/video/youtube/%(title)s-%(id)s.%(ext)s"

OTHERARGS=""
MPVFORMAT="bestvideo+bestaudio/webm/mp4/flv/best"
SAVELOC="/dev/null"
while getopts "mdfj" arg; do
case $arg in
    m) MPVFORMAT="webm[height=480]+bestaudio"
       ;;
    f) DOWNLOADFIRST="1"
       DOWNLOAD="1"
       ;;
    d) DOWNLOAD="1"
       ;;
    j) DOWNLOADFIRST="1"
       DOWNLOAD="1"
       JUSTDOWNLOAD="1"
       ;;
    esac
done

YTURL=$(xclip -o)
echo Downloading from url $YTURL

OUTPUT=$(youtube-dl --no-playlist -g -e -o "$STRTEMP" --format webm/mp4/flv/best --cookies=/tmp/ytcookie.txt $OTHERARGS --get-filename -v "$YTURL")
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
if [ -n "$DOWNLOADFIRST" ]; then
    echo "Using wget to download first"
    wget --load-cookies /tmp/ytcookie.txt -U "$USERAGENT" "$URL" --no-use-server-timestamps -c -O "$SAVELOC"
    if [ ! -n "$JUSTDOWNLOAD" ]; then
	mpv --geometry=-0-0 -title "$TITLE" "$SAVELOC"
    fi
elif [ -n "$DOWNLOAD" ]; then
    echo "Using wget"
    wget --load-cookies /tmp/ytcookie.txt -U "$USERAGENT" "$URL" -O - | tee "$SAVELOC" | mpv --geometry=-0-0 --cache=8192 -title "$TITLE" -
else
    echo "Mplayer direct stream"
    mpv --geometry=-0-0 --ytdl --ytdl-format="$MPVFORMAT" "$YTURL"
    # mpv --geometry=-0-0 --cookies --cookies-file=/tmp/ytcookie.txt --user-agent="$USERAGENT" -title "$TITLE" "$URL"
fi
sleep 10

