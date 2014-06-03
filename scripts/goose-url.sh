#!/bin/bash

source `which virtualenvwrapper.sh`
workon goose
URL=`xclip -o`

~/.sawfish/scripts/goose-url.py "$URL"
