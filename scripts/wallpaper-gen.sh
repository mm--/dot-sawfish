#!/bin/bash

filename="$1"
filewoext="${filename%.*}"
resolution=$(xrandr | grep \* | cut -d' ' -f4)
convert $filename -resize ${resolution}^ -gravity Center -crop ${resolution}+0+0 $filewoext.png
