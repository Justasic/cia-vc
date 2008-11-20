import orderedset
import thread

class TwoLevelQueue(object):
    """
    A blocking queue with uniqueness and two priorities.

    The basic idea is to use as scheduling queue for cronned jobs (low priority)
    and spontaneously-triggered jobs (high priority). A job still scheduled
    will not be scheduled twice if, say, his cron timer expires while the last
    one didn't get serviced yet, though a trigger will move a job from the low-
    to the high-priority queue if neccessary.

    Methods defined here:

    pushLow(elem)  push onto low queue, or no-op if already in a queue
    pushHigh(elem) push onto high queue, potentially removing from low first
    pop()          grab an element from the high queue, or low queue if empty,
                   or block until one of the queues is nonempty
    shutdown()     makes any call to pop() raise SystemExit, and unblocks any
                   threads blocking on pop()
    """
    def __init__(self):
        self.low = orderedset.OrderedSet()
        self.high = orderedset.OrderedSet()
        # A lock to make everything thread-safe
        self.lock = thread.allocate_lock()
        # A lock to make us block when empty
        self.emptylock = thread.allocate_lock()
        self.emptylock.acquire()
        self.dead = False

    def __len__(self):
        return len(self.low) + len(self.high)

    def pushLow(self, elem):
        self.lock.acquire()
        try:
            if elem in self.low or elem in self.high:
                pass # Nothing to do.
            else:
                # Does it become nonempty now?
                if len(self) == 0:
                    self.emptylock.release()
                self.low.append(elem)
        finally:
            self.lock.release()

    def pushHigh(self, elem):
        # Three cases:
        #   elem in self.high: nothing to do
        #   elem in self.low : add to high, remove from there
        #   neither          : add to high, check emptylock
        self.lock.acquire()
        try:
            if elem in self.high:
                pass
            else:
                # Does it become nonempty now?
                if len(self) == 0:
                    self.emptylock.release()
                if elem in self.low:
                    self.low.remove(elem)
                self.high.append(elem)
        finally:
            self.lock.release()

    def pop(self):
        return self.pop_with_priority()[0]

    def pop_with_priority(self):
        # As long as we're empty, block here.
        self.emptylock.acquire()
        if self.dead:
            # Allow other threads to notice, too
            self.emptylock.release()
            raise SystemExit

        self.lock.acquire()
        try:
            # At least one is guaranteed to be nonempty as per emptylock
            if len(self.high) > 0:
                result = (self.high.popleft(), "high")
            else:
                result = (self.low.popleft(), "low")
            # If there's at least one entry remaining, allow next pop()
            if len(self) > 0:
                self.emptylock.release()
        finally:
            self.lock.release()
        return result

    def shutdown(self):
        """
        Shuts the queue down, any pop() will raise SystemExit afterwards.

        This may be called from a signal handler, and will not wait for any lock
        """

        self.dead = True
        try:
            self.emptylock.release()
        except thread.error:
            pass # Was unlocked already
