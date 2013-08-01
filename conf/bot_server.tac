# -*- mode: python; -*-
#
# This is a .tac configuration file for the bot server, a separate
# daemon that maintains connections to IRC servers according to a
# list of requests.
#
# Start the server from the 'cia' directory by running:
#   twistd -oy conf/bot_server.tac -l bot_server.log --pidfile=bot_server.pid
#

from twisted.application import service, internet
#from twisted.conch.manhole_tap import makeService 
from LibCIA.IRC import Bots

# This shit is all for the stupid console authentication
from twisted.conch.manhole import ColoredManhole
from twisted.conch.insults import insults
from twisted.conch.telnet import TelnetTransport, TelnetBootstrapProtocol
from twisted.conch.manhole_ssh import ConchFactory, TerminalRealm

from twisted.internet import protocol
from twisted.application import internet, service
from twisted.cred import checkers, portal

application = service.Application("bot_server")

#
# The BotNetwork is the root object responsible for managing our
# collection of IRC bots. It is constructed with a NickAllocator
# object that specifies how it generates IRC nicks. Other styles
# of nick generation are available.
#
botNet = Bots.BotNetwork(Bots.SequentialNickAllocator("CIA-"))

# The bot server listens on a UNIX socket rather than TCP/IP, for security
botSocketName = "bots.socket"

internet.UNIXServer(botSocketName, Bots.CommandHandlerFactory(botNet)).setServiceParent(application)

# For maintaining the bot server without restarting, if necessary, run
# a twisted.conch.manhole_tap telnet/ssh console.
# This server starts and you should be able to login with any user on
# the running box. Allows for remote use of the daemon and changes without
# restarting the system

def makeService(args):
    checker = checkers.InMemoryUsernamePasswordDatabaseDontUse(cia="letmein")

    f = protocol.ServerFactory()
    f.protocol = lambda: TelnetTransport(TelnetBootstrapProtocol,
                                         insults.ServerProtocol,
                                         args['protocolFactory'],
                                         *args.get('protocolArgs', ()),
                                         **args.get('protocolKwArgs', {}))
    tsvc = internet.TCPServer(args['telnet'], f)

    def chainProtocolFactory():
        return insults.ServerProtocol(
            args['protocolFactory'],
            *args.get('protocolArgs', ()),
            **args.get('protocolKwArgs', {}))

    rlm = TerminalRealm()
    rlm.chainedProtocolFactory = chainProtocolFactory
    ptl = portal.Portal(rlm, [checker])
    f = ConchFactory(ptl)
    csvc = internet.TCPServer(args['ssh'], f)

    m = service.MultiService()
    tsvc.setServiceParent(m)
    csvc.setServiceParent(m)
    return m

#application = service.Application("Interactive Python Interpreter")
namespace = {"botNet": botNet}

makeService({'protocolFactory': ColoredManhole,
             'protocolArgs': (namespace,),
             'telnet': 6023,
             'ssh': 6022}).setServiceParent(application)


# Though we could have made something much simpler like this;
# twisted's makeService does not work like it should in that
# it's password files do not authenticate correctly
# and therefore we have to override it with the above pile of crap
# all for one line:
# checker = checkers.InMemoryUsernamePasswordDatabaseDontUse(cia="letmein")
#

# options = {
# 	# for some reason, these must
# 	# all exist, even if None
# 	'namespace'  : botNet,
# 	'passwd'     : '~/.bots.passwd',
# 	'sshPort'    : 'tcp:2231',
# 	'telnetPort' : 'tcp:2230',
# 	'interface'  : 'localhost',
# }

# shell_service = makeService(options)
# shell_service.setServiceParent(application)

### The End ###
