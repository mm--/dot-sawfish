#!/bin/bash

pkill -u `whoami` -f "dictator_start"
~/.sawfish/scripts/goose-url.sh | /home/jm3/Downloads/dictator-0.9.7/dictator_start.py -n -i
