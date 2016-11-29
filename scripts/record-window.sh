#!/bin/bash

function pgrep_live {
  pids=$(pgrep -u `whoami` -f "$1");
  [ "$pids" ] || return;
  ps -o s= -o pid= $pids | sed -n 's/^[^ZT][[:space:]]\+//p';
}

if [ $(pgrep_live "recordmydesktop" | wc -l) -gt 0 ]
then
    echo "Stopping session"
    pkill --signal SIGINT -u `whoami` -f "recordmydesktop"
    xterm -e "tmux attach -t recorddesktop" &
else
    echo "Starting new session"
    tmux new-session -d -s "recorddesktop" "recordmydesktop --device default --windowid $1 -o $2"
    # recordmydesktop --device default --windowid $1 -o $2
fi

