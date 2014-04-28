#!/bin/bash

date1=$(date +%s)
taskname="GAP"
if [ -f ~/.currenttask ]
then
date1=$(sed -n '2p' ~/.currenttask)
taskname=$(sed -n '1p' ~/.currenttask)
fi

echo "TASKNAME:${taskname:0:10}" >> ~/.sawfish/scripts/myfifo
echo "TASKTIME:$date1" >> ~/.sawfish/scripts/myfifo
echo "UPDATE: please" >> ~/.sawfish/scripts/myfifo
