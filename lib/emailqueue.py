import os, glob, signal

BASE_DIR = "/home/cia/cia/data/queue/"

# A dummy signal handler, to make SIGUSR1 wake us up from sleep and stuff.
def sig_noop(signum, frame):
    pass

class EmailQueue(object):
    def __init__(self, name):
        self.directory = BASE_DIR + name

    def register(self):
        signal.signal(signal.SIGUSR1, sig_noop)
        fh = open(self.directory + "/queue.pid", 'w')
        fh.write("%d\n" % os.getpid())
        fh.close()

    def get(self):
        mails = glob.glob("%s/mail.*" % self.directory)
        mails.sort()
        for filename in mails:
            fh = open(filename)
            result = fh.read()
            fh.close()
            os.remove(filename)
            yield result
