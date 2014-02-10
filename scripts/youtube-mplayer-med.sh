#!/bin/bash

mplayer -fixed-vo -geometry -0-0 $(youtube-dl --max-quality 18 -g $(xclip -o))
