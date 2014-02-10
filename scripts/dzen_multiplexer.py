#!/usr/bin/env python
import subprocess, select, re, sys

dic = { "ORANGE" : "#ee9a00",
        "GREEN" : "#00ff00",
        "_LEFT" : "0",
        "_RIGHT" : "1000",
        "_MPDALIGN" : "920",
        "_MPDALIGN2" : "1194",
        "_DEFFONT" : "Terminus:size=8",
        "_TASKALIGN" : "805",
        "_DESKALIGN" : "805",
        "_DATEALIGN" : "1280"}                        # For replacements
layout = "LEFT RIGHT"

conky = subprocess.Popen(['/home/jm3/.sawfish/scripts/conky-configs.sh'],stdout=subprocess.PIPE,stderr=subprocess.PIPE)
fifo = subprocess.Popen(['/home/jm3/.sawfish/scripts/replace-listen-fifo.sh', '0.1'],stdout=subprocess.PIPE,stderr=subprocess.PIPE)

processes = [conky, fifo]
fdtoprocess = dict([(x.stdout.fileno(), x.stdout) for x in processes])
poll = select.poll()
for x in processes:
    poll.register(x.stdout,select.POLLIN | select.POLLHUP)
pollc = len(processes)

remember = {}
events = poll.poll()

def replace_all(text, dic):
    for i, j in dic.iteritems():
        text = text.replace(i, j)
    return text

def line_handler(line):
    """Take a line. If it's not a REPLACE then remember it. Output to
stdout if it's a FLUSH"""
    m = re.match(r"^(.*?):(.*)$", line)
    if m:
        command, rest = m.group(1,2)
        if command == "REPLACE":
            print >> sys.stderr, "REPLACE GOT:", rest
            m2 = re.match(r"^(.*?):(.*)$", rest)
            if m2:
                oldtext, newtext = m2.group(1,2)
                dic[oldtext] = newtext
        else:
            remember[command] = rest
        sys.stdout.write(replace_all(replace_all(layout, remember),
                                     dic) + "\n")
        sys.stdout.flush()

while pollc > 0 and len(events) > 0:
  for event in events:
    (rfd,event) = event
    if event & select.POLLIN:
        line = fdtoprocess[rfd].readline().rstrip()
        line_handler(line)
    if event & select.POLLHUP:
      poll.unregister(rfd)
      pollc = pollc - 1
    if pollc > 0: events = poll.poll()
for x in processes:
    x.wait()
