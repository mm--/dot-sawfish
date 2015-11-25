#!/bin/bash
# Get url from clipboard
# Download to temp directory
# Open with dragon (which I have renamed to xdragon)
# (https://github.com/mwh/dragon)

SAVEDDIR=`pwd`
echo SAVEDDIR IS $SAVEDDIR
TMPDIR=`mktemp -d dragon-XXXXXX`

function finish {
    cd "$SAVEDDIR"
    rm -rf "$TMPDIR"
}
trap finish EXIT

cd "$TMPDIR"

thefile=$(curl -O `xclip -o` -w "%{filename_effective}")
xdragon --and-exit "$thefile"
sleep 30
