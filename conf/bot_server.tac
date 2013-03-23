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
from twisted.conch.manhole_tap import makeService
from LibCIA.IRC import Bots

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
options = {
	# for some reason, these must
	# all exist, even if None
	'namespace'  : botNet,
	'passwd'     : '~/.daemon_passwds',
	'sshPort'    : 'tcp:2231',
	'telnetPort' : 'tcp:2230',
	'interface'  : 'localhost',
}

shell_service = makeService(options)
shell_service.setServiceParent(application)

### The End ###
