#!/bin/bash

if pgrep -u `whoami` -f "mplayerfifo"
then
    echo "${*:1}" > ~/.sawfish/pipes/mplayerfifo
else
    exit 1
fi
