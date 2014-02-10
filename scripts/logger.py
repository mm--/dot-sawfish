#!/usr/bin/env python
# Take in a line containing a window name, and log it into CSV format 

import sys, datetime, csv, signal

minTime = 2

writer = csv.writer(sys.stdout, quoting=csv.QUOTE_ALL)
lastTime = None
window = None

def log_it(line):
    "Log the window"
    global lastTime, window, writer
    now = datetime.datetime.now()
    now_str = now.strftime("%Y-%m-%d %H:%M:%S")
    if lastTime:
        lastTime_str = lastTime.strftime("%Y-%m-%d %H:%M:%S")
        duration = (now - lastTime).seconds
        if duration > minTime:
            writer.writerow([lastTime_str, now_str, duration, window])
            sys.stdout.flush()
    lastTime = now
    window = line

def signal_handler(signal, frame):
    log_it(None)
    sys.exit(0)

signal.signal(signal.SIGINT, signal_handler)

line = sys.stdin.readline()
while line:
    win = line.rstrip()
    log_it(win)
    line = sys.stdin.readline()
