#!/usr/bin/env python

import thread, email, logging, signal, time, re

from cia.lib import blockingqueue, simplelog, emailqueue
from cia.apps.repos.models import Repository

# These are needed for ping email forwarding
from cia.apps.accounts.authplus import send_mail_to_user
from cia.apps.accounts.models import Project, UserAsset
from django.contrib.contenttypes.models import ContentType

from cia.apps.repos.svn import SvnClient

ping_re = re.compile(r"ping\+([a-zA-Z0-9]+)@")

NUM_WORKERS = 10

# XXX - hack - we leak memory like a sieve. die twice a day, get restarted by cron.
LIFETIME = 3600 * 12
# If we don't manage any successfull poll within this*10 seconds, die.
WATCHDOG_TIMEOUT = 30

# Queue contains pinger names, to save on memory
pollerqueue = blockingqueue.TwoLevelQueue()
queuedmails = emailqueue.EmailQueue("ping")
running = True
# List of all repositories we are currently querying - if we get two
# ping emails for the same repo at nearly the same time, we may be getting
# two checking threads getting the same old state from the db and calling in
# the same changes.
processing = set()
penalties = {}
# Lock to ensure consistency of that
process_lock = thread.allocate_lock()

def isPowerOfTwo(number):
    if number == 1:
        return True
    i = 1
    while number > i:
        i <<= 1
        if i == number:
            return True
    return False

def svn_loop(pollerqueue):
    # Persistent client instance, because pysvn has a sucking competition with black holes
    # and a leaking competition with neutrino radiation
    global watchdog
    client = SvnClient(None)
    try:
      while 1:
        # This call manages the entire thread:
        # If the queue has been shut down, this raises SystemExit.
        # Normally, it blocks until a request is available, and returns it.
        pinger, prio = pollerqueue.pop_with_priority()

        # Ensure nobody else is pinging this at the same time as us
        process_lock.acquire()
        while pinger in processing:
            process_lock.release()
            time.sleep(5)
            process_lock.acquire()
        # Okay, nobody else is processing this, so we are.

        # Exponential backoff for nonresponsive repositories
        if prio != "high":
            penalties[pinger] = penalties.get(pinger, 0) + 1
            if not isPowerOfTwo(penalties[pinger]):
                process_lock.release()
                continue

        processing.add(pinger)
        process_lock.release()

        # big try-catch to prevent errors on a repository from killing off
        # our workers.
        try:
            try:
                repos = Repository.objects.get(is_active=True,
                                           pinger_name=pinger)
            except Repository.DoesNotExist:
                repos = None
            if repos:
                logging.info("%03d Polling %s..." % (thread.get_ident(), repos.location))
                watchdog = WATCHDOG_TIMEOUT
                client.model = repos
                client._pathRegexes = None
                client.poll()
                penalties[pinger] = 0

        except:
            logging.info("Poller for %s threw exception.", pinger, exc_info=1)

        process_lock.acquire()
        processing.discard(pinger)
        process_lock.release()
    except Exception:
        logging.exception("ACK! Pollerthread %03d going down." % thread.get_ident())
        raise


def get_repository_for_message(msg):
    match = ping_re.search(msg['to'] or "") or ping_re.search(msg['x-original-to'] or "")
    if match:
        name = match.group(1)
        try:
            return Repository.objects.get(is_active=True, pinger_name=name)
        except Repository.DoesNotExist:
            return None


def process_mail(text):
    msg = email.message_from_string(text)
    repos = get_repository_for_message(msg)
    if not repos:
        return

    pollerqueue.pushHigh(repos.pinger_name)

    if repos.forward_pinger_mail:
        # Sigh. Yay for features that tear through abstractions.
        # Would be so wonderful to not even bother with Repository
        # objects up here, but no... ;)
        project = Project.objects.get(repos=repos)
        ct = ContentType.objects.get_for_model(Project)
        for user_asset in UserAsset.objects.filter(content_type=ct, object_id=project.id):

            send_mail_to_user(user_asset.user, "repos/pinger-fwd.txt",
                              msg['from'], msg=msg, repos=repos,
                              user_asset=user_asset)



def process_mailqueue():
    for text in queuedmails.get():
        try:
            process_mail(text)
        except:
            logging.exception("mail threw exception")


def process_cron(lasttime):
    now = int(time.time())

    # yay for lazy sql ;)
    polled_reposes = Repository.objects.filter(enable_polling=True,
                                               is_active=True)

    # Normally, this loop will run 0 or 1 time[s].
    # But if we got stuck, we may need to catch up some.
    for minute in xrange(lasttime // 60, now // 60):
        for repos in polled_reposes:
            # Stagger to avoid floods at full quarter hours
            if repos.poll_frequency < 1 or (minute + repos.id) % repos.poll_frequency == 0:
                pollerqueue.pushLow(repos.pinger_name)

    return now

def main_loop():
    logging.info("starting main loop.")
    lasttime = int(time.time())
    deadline = lasttime + LIFETIME
    global watchdog
    watchdog = WATCHDOG_TIMEOUT
    while running and (time.time() < deadline):
        process_mailqueue()
        lasttime = process_cron(lasttime)
        time.sleep(10)
        fh = open("/home/cia/cia/data/queue/ping/pollq.length", 'w')
        fh.write("High: %2d\nLow: %3d\n"  % (len(pollerqueue.high), len(pollerqueue.low)))
        fh.close()
        watchdog -= 1
        if watchdog < 0:
            logging.error("watchdog timeout")
            break
    logging.info("main loop terminating.") # planned death on %d." % deadline)


def shutdown(*args):
    global running
    pollerqueue.shutdown()
    running = 0


def main():
    simplelog.init("polld")
    signal.signal(signal.SIGTERM, shutdown)
    queuedmails.register()

    for i in xrange(NUM_WORKERS):
        thread.start_new_thread(svn_loop, (pollerqueue,))

    try:
        main_loop()
    finally:
        # Ensure eventual termination if the queue loop exits for some reason
        pollerqueue.shutdown()


if __name__ == '__main__':
    main()
