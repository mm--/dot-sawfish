#!/bin/bash

pipe="$HOME/.sawfish/scripts/myfifo"

if [ ! -p "$pipe" ]
then
    mkfifo $pipe
fi

while [ -p "$pipe" ]
do
    cat $pipe
done
