""" LibCIA.IRC.Network

IRC Networks define a collection of servers, and optionally
define special behaviour relevant to that IRC network.
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

# XXX: This is a py3 hack, please fix :(
def cmp(a, b):
    return (a > b) - (a < b)

class BaseNetwork:
    """Base class for IRC networks. A network may consist of multiple servers-
       it provides a 'connect' method that picks one to use. Networks may also
       define other IRC-network-specific behaviour.
       """
    alias = None
    defaultPort = 6667
    currentServer = 0

    # XXX: This should be filled by the classes below
    servers = (("", defaultPort), )

    # PASS to send on connect, or None
    password = None

    # Maximum number of IRC channels a single bot is allowed in
    maxChannels = 18

    # Timeout, in seconds, for creating new bots. By default this is 16
    # minutes, since most of the smaller networks we're on get annoyed
    # if we retry too quickly. We override this in the larger networks below.
    newBotTimeout = 60 * 16

    # Maximum amount of data to buffer server-side, in bytes. Larger values improve
    # speed, but if this is larger than a fixed-size buffer on the server we could be
    # flood-kicked.
    bufferSize = 1024

    # How often to ping the server, in seconds
    pingInterval = 60

    def __str__(self):
        return "%s:%s" % self.servers[0] if not self.alias else self.alias

    def getIdentity(self):
        """Return a (host, port) tuple that can be used to return this network
           from a search.
           """
        return (self.alias, None)

    def __repr__(self):
        return "<IRC.Network.%s>" % self.__class__.__name__

    def __cmp__(self, other):
        if type(self) is type(other) and (
            isinstance(self, BaseNetwork) and isinstance(other, BaseNetwork)):
            return cmp(self.alias or self.servers,
                       other.alias or other.servers)
        else:
            return cmp(self, other)

    def __hash__(self):
        return hash(self.alias)

    def getNextServer(self):
        """Return the next server, as a (host, port) tuple, to use for this network."""
        # By default, just try them all round-robin style
        self.currentServer = self.currentServer % len(self.servers)
        host, port = self.servers[self.currentServer]
        self.currentServer += 1
        if port is None:
            port = self.defaultPort
        return (host, port)


class GenericNetwork(BaseNetwork):
    """A generic IRC network has no alias, refers to only one server, and
       uses defaults for all other parameters. This is used when a particular
       network isn't specified, and it acts as the base class for all other
       networks.
       """

    def __init__(self, host, port=None):
        if port is not None:
            port = int(port)
        self.servers = ((host, port), )

    def getIdentity(self):
        return self.servers[0]

    def __str__(self):
        host, port = self.servers[0]
        if port is None:
            return host
        else:
            return "%s:%d" % (host, port)

    def __repr__(self):
        return "<IRC.Network.%s %s>" % (self.__class__.__name__, self)

    def __hash__(self):
        return hash(self.servers)


class Freenode(BaseNetwork):
    alias = 'freenode'
    # Really short timeout- there are a lot of freenode servers and they can
    # take it. Reconnecting all hosts to Freenode just takes way too long with
    # the default value.
    newBotTimeout = 20
    servers = (
        ('irc.freenode.net', None),
        ('chat.freenode.net', None),
        ('anthony.freenode.net', None),
        ('calvino.freenode.net', None),
        ('holmes.freenode.net', None),
        ('hubbard.freenode.net', None),
        ('kornbluth.freenode.net', None),
        ('lindbohm.freenode.net', None),
        ('niven.freenode.net', None),
        ('verne.freenode.net', None),
        ('zelazny.freenode.net', None),
    )


class Undernet(BaseNetwork):
    alias = 'undernet'
    # They don't have a consistent MAXCHANNELS. Since they also don't have useful DNS, use lowest seen, minus 2
    maxChannels = 8
    servers = (
        ('irc.undernet.org', None),
    )


class Worldforge(BaseNetwork):
    alias = 'worldforge'
    servers = (
        ('lester.mithis.com', None),
        #('irc.worldforge.org', None),
        #('purple.worldforge.org', None),
    )


class IRCNet(BaseNetwork):
    alias = 'ircnet'
    servers = (
        #('irc.osanet.cz', None),
        #('irc.felk.cvut.cz', 6666),

        # Bert Hubert claims either of these should be fine
        ('us.ircnet.org', None),
        ('ircnet.choopa.net', None),
    )


class EFNet(BaseNetwork):
    alias = 'efnet'
    newBotTimeout = 5*60
    servers = (
        ('irc.shoutcast.com', None),
        ('efnet.xs4all.nl', None),
        ('irc.mzima.net', None),
    )


class SpartaIRC(BaseNetwork):
    alias = 'spartairc'
    password = 'REPLACEME'
    servers = (
        ('irc.spartairc.co.cc', 6667),
    )


class StackSmash(BaseNetwork):
    alias = 'StackSmash'
    servers = (
        ('irc.stacksmash.net', 6667),
    )


_aliasCache = None
_instCache = {}


def find(host, port=None):
    """Find a network corresponding to the given host (or alias) and port"""
    global _aliasCache
    global _instCache

    if _aliasCache is None:
        # Cache a dict mapping aliases to classes
        _aliasCache = {}
        for name, obj in globals().items():
            if type(obj) is type(BaseNetwork) and issubclass(obj, BaseNetwork):
                _aliasCache[obj.alias] = obj

    if host in _aliasCache:
        # It's a known alias, return a cached instance of that network class
        try:
            return _instCache[host]
        except KeyError:
            inst = _aliasCache[host]()
            _instCache[host] = inst
            return inst

    else:
        # Create a new GenericNetwork for this host and port
        return GenericNetwork(host, port)

### The End ###
