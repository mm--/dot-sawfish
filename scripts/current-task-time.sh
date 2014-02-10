#!/bin/bash

[ -f ~/.currenttask ] || exit

date1=$(sed -n '2p' ~/.currenttask)
date2=$(date +%s)
diff=$(($date2-$date1))
h=$(( $diff / 3600 ))
m=$(( ( $diff / 60 ) % 60 ))
s=$(( $diff % 60 ))
taskname=$(sed -n '1p' ~/.currenttask)
printf "[%s %02d:%02d]" "${taskname:0:10}" $h $m
