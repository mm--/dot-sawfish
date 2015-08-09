#!/bin/bash

LOCKFILE="$HOME/.sawfish/pipes/lockscreen"
touch $LOCKFILE
"$@"
rm -f $LOCKFILE
