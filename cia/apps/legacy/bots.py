from django.conf import settings
from twisted.internet import defer, reactor, protocol
from twisted.protocols import basic
from twisted.python import failure
import time

class TimeoutError(Exception):
    pass

def block(d, timeout=60.0):
    """Block on a deferred, resolving it into a value or an exception.
       If the entire operation doesn't complete within a specified
       timeout, in seconds, this raises TimeoutError.
       """
    deadline = time.time() + timeout
    while not d.called:
        remaining = deadline - time.time()
        if remaining < 0:
            raise TimeoutError()
        reactor.iterate(delay = min(1, remaining))
    if isinstance(d.result, failure.Failure):
        d.result.raiseException()
    return d.result

class BotServer(basic.LineOnlyReceiver):
    def __init__(self):
        self.deferred = None
        self.handlers = {
          'BEGIN_STATUS': self.ignore,
          'STATUS': self.collect_status,
          'END_STATUS': self.finish,
          'REPORT': self.handle_report,
          'TOTALS': self.handle_totals,
          'BEGIN_MSGLOG': self.ignore,
          'MSGLOG': self.collect_msglog,
          'END_MSGLOG': self.finish,
        }

    def lineReceived(self, line):
        try:
            parts = line.split(None, 2)
            command = parts[0]
            handler = self.handlers[command]
            handler(*parts[1:])
        except StandardError, e:
            self.deferred.errback(e)

    def disconnect(self):
        self.transport.loseConnection()

    def defer(self):
        self.deferred = defer.Deferred()
        return self.deferred

    def doFinish(self, result):
        self.deferred.callback(result)

    def ignore(self, *args):
        pass

    def finish(self, *args):
        self.doFinish(self.result)

    def collect_status(self, target, countAtNick):
        server, channel = target.split('/', 1)
        count, nick = countAtNick.split('@', 1)
        self.result.append({
          'server': server,
          'channel': channel,
          'user_count': int(count),
          'botnick': nick,
        })

    def handle_report(self, target, countAtNick):
        server, channel = target.split('/', 1)
        count, nick = countAtNick.split('@', 1)
        self.doFinish( {
          'server': server,
          'channel': channel,
          'user_count': int(count),
          'botnick': nick,
          })

    def handle_totals(self, asterisk, dictstr):
        self.doFinish(dictstr) # XXX - improve

    def collect_msglog(self, timestr, logstr):
        self.result.append((timestr, logstr))


    def status(self):
        self.sendLine("STATUS")
        self.result = []
        self.deferred = defer.Deferred()
        return self.deferred

    def report(self, target):
        self.sendLine("REPORT " + target)
        self.deferred = defer.Deferred()
        return self.deferred

    def totals(self):
        self.sendLine("TOTALS")
        self.deferred = defer.Deferred()
        return self.deferred

    def msglog(self):
        self.sendLine("MSGLOG")
        self.result = []
        self.deferred = defer.Deferred()
        return self.deferred


class BotConnFactory(protocol.ClientFactory):
    protocol = BotServer
    def __init__(self, deferred):
        self.deferred = deferred

    def buildProtocol(self, addr):
        p = protocol.Factory.buildProtocol(self, addr)
        reactor.callLater(0, self.deferred.callback, p)
        return p

def connect(timeout = 30):
    deferred = defer.Deferred()
    factory = BotConnFactory(deferred)
    reactor.connectUNIX(settings.CIA_BOT_SOCKET, factory, timeout = timeout)
    return deferred

def needs_bot_server(view_func):
    def _wrapper(request, *args, **kwargs):
        server = block(connect())
        try:
            return view_func(request, server, *args, **kwargs)
        finally:
            server.disconnect()

    _wrapper.__doc__ = view_func.__doc__
    _wrapper.__dict__ = view_func.__dict__
    return _wrapper

def status():
    server = block(connect())
    try:
        return block(server.status())
    finally:
        server.disconnect()

def report(target):
    server = block(connect())
    try:
        return block(server.report(target.encode('utf-8')))
    finally:
        server.disconnect()

def totals():
    server = block(connect())
    try:
        return block(server.totals())
    finally:
        server.disconnect()

def msglog():
    server = block(connect())
    try:
        return block(server.msglog())
    finally:
        server.disconnect()
