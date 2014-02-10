#!/bin/bash

mplayer -fixed-vo -geometry -0-0 $(youtube-dl -g $(xclip -o))
