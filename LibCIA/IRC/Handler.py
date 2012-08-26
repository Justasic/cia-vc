""" LibCIA.IRC.Handler

The irc:// URI handler, acting as a frontend to our network of bots
"""
#
# CIA open source notification system
# Copyright (C) 2003-2007 Micah Dowty <micah@navi.cx>
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#

from twisted.internet import reactor, protocol
from twisted.protocols import basic
from twisted.python import log
from LibCIA import Ruleset

class IrcURIHandler(Ruleset.RegexURIHandler):
    """Handles irc:// URIs in rulesets. This creates a message queue
       for each URI, and delivers formatted messages to the proper
       message queue instance.

       This follows a subset of the internet draft at:

          http://www.w3.org/Addressing/draft-mirashi-url-irc-01.txt

       In short, the following sorts of URIs are valid:

          irc://irc.foo.net/boing
            Refers to the channel #boing at irc.foo.net on the default port

          irc://irc.foo.net:1234/boing
            Refers to the channel #boing at irc.foo.net, on port 1234

          irc://irc.foo.net/muffin,isnick
            Refers to a users with the nick 'muffin' on irc.foo.net's default port

       The full hostname may also be replaced with a network name, as defined
       in Network.py. This implies a random choice from multiple servers, and possibly
       other network-specific behaviour.

       This is starting to be updated to a newer irc:// draft that recommends
       including the #, but still supports the above URIs.
       """
    scheme = 'irc'
    regex = r"""
       ^irc://(?P<host>(  ([a-zA-Z]([a-zA-Z0-9.-]*[a-zA-Z0-9])?) |
                          ([0-9]+(\.[0-9]+){3})
                       ))
       (:(?P<port>[0-9]+))?/(?P<target>[^\s,]+)(?P<isnick>,isnick)?$
       """

    def __init__(self, remoteBots):
        self.remoteBots = remoteBots
        self.trans = {}
        Ruleset.RegexURIHandler.__init__(self)

    def transformUri(self, uri):
        d = self.parseURI(uri)
        if d['port']:
            server = d['host'] + ':' + d['port']
        else:
            server = d['host']

        if d['isnick']:
            # future: Use different seperation char or something?
            return server + "/" + d['target']
        else:
            # Compatibility: Add initial # if not given
            target = d['target']
            if not target[0] in "#!+&":
                target = '#' + target
            return server + "/" + target

    def assigned(self, uri, newRuleset):
        transformed = self.transformUri(uri)
        self.trans[uri] = transformed
        self.remoteBots.add(transformed)

    def unassigned(self, uri):
        self.remoteBots.delete(self.trans[uri])
        self.trans.pop(uri)

    def message(self, uri, message, content):
        self.remoteBots.msg(self.trans[uri], unicode(content).encode('utf-8'))

    def rulesetsRefreshed(self):
        """Synchronize our requests with the server's only once rulesets have
           refreshed, so we don't inadvertently delete a request that hasn't
           finished loading yet.
           """
        self.remoteBots.allowSync = True
        self.remoteBots.syncRequests()


class ReconnectingBotServerClient(protocol.ClientFactory):
    """A PBClientFactory that automatically tries to reconnect
       using a supplied method if the connection fails.
       """
    def __init__(self, reconnector, connectCallback, discoCallback, delay=4):
        self.reconnector = reconnector
        self.connectCallback = connectCallback
        self.discoCallback = discoCallback
        self.delay = delay

    def protocol(self):
        return BotControlProtocol(self.discoCallback)

    def clientConnectionFailed(self, connector, reason):
        reactor.callLater(self.delay, self.reconnector)

    def clientConnectionLost(self, connector, reason):
        reactor.callLater(self.delay, self.reconnector)

    def buildProtocol(self, addr):
        p = protocol.ClientFactory.buildProtocol(self, addr)
        reactor.callLater(0, self.connectCallback, p)
        return p


class RemoteBots:
    """Represents a bot server running elsewhere, that we connect to
       using Plain Old Commandlines. This ensures that the remote request list
       matches the local one even when the bot server restarts.
       """
    def __init__(self, socketName):
        self.socketName = socketName
        self.factory = ReconnectingBotServerClient(self.connect, self._gotConnection, self._lostConnection)
        self.allowSync = False

        # Collects URLs that ought to exist on the remote side
        self.requests = set()

        self.connect()

    def connect(self):
        log.msg("Trying to connect to bots on socket %r..." % self.socketName)
        self.client = None
        reactor.connectUNIX(self.socketName, self.factory)

    def _gotConnection(self, client):
        log.msg("Connected to bot server")
        self.client = client

        if self.allowSync:
            # Sync on connect only if we can be sure we've loaded all requests
            self.syncRequests()

    def _lostConnection(self, reason):
        log.msg("lost connection to bot server")
        self.client = None

    def add(self, uri):
        self.requests.add(uri)
        if self.client:
            self.client.send_add(uri)

    def delete(self, uri):
        self.requests.discard(uri)
        if self.client:
            self.client.send_del(uri)

    def msg(self, uri, message):
        if not uri in self.requests:
            log.msg("IRC handler desync: Request for %s not known here" % uri)
            return
        if self.client:
            self.client.send_msg(uri, message)

    def syncRequests(self):
        """If a connection to the bot server exists,
           this ensures it tracks the same URIs we do,
           adding and removing as neccessary
           """
        log.msg("Synchronizing bot requests")

        if self.client:
            self.client.send_sync(self.requests)

class BotControlProtocol(basic.LineOnlyReceiver):
    """The Protocol that communicates with the Bot server.

       See corresponding class in Bots.py for a description of the protocol.
    """

    def __init__(self, disconnectCallback):
        self.discoCallback = disconnectCallback

    def lineReceived(self, line):
        log.msg("IRC Handler received unexpected line from bot server: %s" % line)

    def connectionLost(self, reason):
        log.msg("IRC Handler lost connection.")
        self.discoCallback(reason)
        self.discoCallback = None

    def send_add(self, uri):
        self.sendLine("ADD " + uri)

    def send_del(self, uri):
        self.sendLine("DEL " + uri)

    def send_msg(self, uri, msg):
        msg = msg.replace('\r', '\n')
        for line in msg.split('\n'):
            if line:
                self.sendLine("MSG %s %s" % (uri, line))

    def send_sync(self, uris):
        self.sendLine("START_SYNC")
        for uri in uris:
            self.sendLine("SYNC " + uri)
        self.sendLine("END_SYNC")

    def sendLine(self, line):
        """Sends a line, asserting it does not contain newlines"""
        if line.count('\r') or line.count('\n'):
            raise ValueError, "line to send contained newlines!"
        if self.transport is None:
            self.connectionLost("Detected by sendLine")
        basic.LineOnlyReceiver.sendLine(self, line)

### The End ###
