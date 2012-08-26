#!/usr/bin/env python
#
# incoming-q.py
#
# Process a mail file queue directed at the CIA bot. This script is supposed
# to run as a daemon, waiting for new email to arrive.
#
# Doesn't do much, just tosses the email to the RPC server and lets that take
# care of everything.
#
# Emails are stored in /home/cia/cia/data/queue/commit, we are notified of new mail
# by a SIGUSR1 to speed the process up.

import xmlrpclib, time, logging, signal, os
from cia.lib import emailqueue, simplelog

def deliver_loop():
    while 1:
        # Getting this during processing may break syscalls
        signal.signal(signal.SIGUSR1, signal.SIG_IGN)
        for message in queue.get():
            try:
                # Apparently, xmlrpclib fails to send a call to itself
                # if a string argument contains non-utf8 chars.
                # This probably breaks some stuff, but may fix that
                s.mail.deliver(message.decode('ascii', 'replace').encode('ascii', 'replace'))

                # Hack to prevent us killing ourself too much
                if os.getloadavg()[0] > 4:
                    time.sleep(10)
            except:
                logging.exception("delivery error")
                time.sleep(10)
        signal.signal(signal.SIGUSR1, emailqueue.sig_noop)
        time.sleep(10)

if __name__ == '__main__':
    simplelog.init("incoming-q")
    queue = emailqueue.EmailQueue("commit")
    queue.register()
    s = xmlrpclib.ServerProxy("http://localhost:3910")
    deliver_loop()
