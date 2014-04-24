#!/usr/bin/env python

import os, re, time

pattern = re.compile("(repos-poll|repos-ping)")

for line in os.popen("ps -eo pid,lstart,args"):
    if pattern.search(line):
        tok = line.split()
        pid = int(tok[0])
        start_time = time.mktime(time.strptime(' '.join(tok[1:6])))
	runtime = time.time() - start_time

        if runtime > 600:
            os.kill(pid, 9)

try:
    os.remove("/home/cia/cia/incoming.lock")
except:
    # if it's not there, fine.
    pass
