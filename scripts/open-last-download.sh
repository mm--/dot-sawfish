#!/bin/bash

dir=$HOME/Downloads
lastfile=$(find "$dir" -maxdepth 1 -type f -printf '%T@ %p\n' | sort -n | tail -1 | cut -f2- -d" ")
echo last file is $lastfile
xdg-open "$lastfile" >/dev/null 2>&1 &
