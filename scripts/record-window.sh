#!/bin/bash

if pgrep -u `whoami` -f "recordmydesktop"
then
    pkill --signal SIGINT -u `whoami` -f "recordmydesktop"
else
    recordmydesktop --windowid $1 --device pulse -o $2 &
fi

