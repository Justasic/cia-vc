import logging, logging.handlers, sys

def uncaught_hook(type, value, tb):
    logging.error("Uncaught exception somewhere:", exc_info=(type, value, tb))

def init(name, threaded=False):
    root_log = logging.getLogger("")
    hand = logging.handlers.RotatingFileHandler("/var/log/cia/%s.log" % name,
                                                'a', 100000, 4)
    handErr = logging.handlers.RotatingFileHandler("/var/log/cia/%s.err" % name,
                                                   'a', 100000, 4)
    handErr.setLevel(logging.ERROR)
    if threaded:
        hand.createLock()
    form = logging.Formatter("%(levelname)-10s %(asctime)s %(message)s")
    hand.setFormatter(form)
    handErr.setFormatter(form)
    root_log.addHandler(hand)
    root_log.addHandler(handErr)
    root_log.setLevel(logging.INFO)
    sys.excepthook = uncaught_hook
