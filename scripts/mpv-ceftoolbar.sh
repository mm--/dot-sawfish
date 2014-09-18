#!/bin/bash

FIFO="$HOME/.sawfish/scripts/myfifo"
echo "PIANOBAR:True" > "$FIFO"
echo "PIANOARTIST:$1" > "$FIFO"
echo "PIANOTITLE:$2" > "$FIFO"
echo "UPDATE: please" > "$FIFO"
