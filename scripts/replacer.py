#!/usr/bin/env python
import threading, sys

running = True
dic = { "ORANGE" : "ee9a00" }

pipename = str(sys.argv[1]) if len(sys.argv) > 1 else "./myfifo"
print >> sys.stderr, "Using file", pipename

def getSubstitutes():
    global running, dic
    while running:
        with open(pipename, 'rb') as f:
            print >> sys.stderr, "File opened"
            line = f.readline()
            while line:
                print >> sys.stderr, "Line is ", line
                separated = [x.strip() for x in line.split(',')]
                print >> sys.stderr, "Separated is", separated
                if(len(separated) >= 2):
                    dic[separated[0]] = separated[1]
                    print >> sys.stderr, separated[0], "is", separated[1]
                line = f.readline()
            print >> sys.stderr, "File closed"

t = threading.Thread(target=getSubstitutes, args = ())
t.start()

def replace_all(text, dic):
    for i, j in dic.iteritems():
        text = text.replace(i, j)
    return text

try:
    line = sys.stdin.readline()
    while line:
        sys.stdout.write(replace_all(line, dic))
        sys.stdout.flush()
        line = sys.stdin.readline()
except KeyboardInterrupt:
    running = False
