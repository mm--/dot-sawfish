#!/bin/bash
# Resize to my wallpaper size

filename=$(basename "$1")
filename="${filename%.*}"

convert "$1" -resize 1600x900^ -gravity center -crop 1600x900+0+0 +repage "$filename-wallpaper.png"
