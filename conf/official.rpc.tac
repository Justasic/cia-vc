# -*- mode: python; -*-
#
# Runs a server that only handles RPC requests. It will be responsible
# for message filtering and delivery, as well as other administrative
# tasks that run over RPC. It must be combined with a separate web
# server process.
#
# This is the configuration for the official CIA host.
# It can be started via http-cluster.sh.
#

import os
port = int(os.getenv("PORT"))

import LibCIA.IRC.Handler
from twisted.application import service, internet
from LibCIA import Database, Message, Ruleset, IRC, Stats, IncomingMail, Cron
from LibCIA import Debug, Security, RpcServer, RpcClient, Web, Cache, Files

Database.init()

application = service.Application("rpc_server")
hub = Message.Hub()

# Save all messages in a permanent MessageArchive. This uses an
# append-only SAX buffer, sorted by date. We don't do anything with
# this data yet, but it will be the foundation for a new message
# filtering architecture.
hub.addClient(Stats.Messages.MessageArchive(
    Files.getDir(Files.dbDir, 'archive')).deliver)

# Connect to IRC bots running in a separate process
remoteBots = IRC.Handler.RemoteBots("bots.socket")

# Set up periodic maintenance tasks
Cron.Scheduler(
    Cron.Event(Cron.hourly, Stats.Target.Maintenance().run, "stats maintenance"),
    Cron.Event(Cron.hourly, Cache.Maintenance().run, "cache maintenance"),
    )

# A list of URI handlers that can be used as targets for rulesets
uriRegistry = Ruleset.URIRegistry(
    IRC.Handler.IrcURIHandler(remoteBots),
    Stats.Handler.StatsURIHandler(),
    RpcClient.XmlrpcURIHandler(),
    )

# Use a persistent set of rulesets to filter and format messages.
# We keep the rulesets in a database table, updating them according
# to requests from the ruleset editor.
rulesetStorage = Ruleset.DatabaseRulesetStorage(hub, uriRegistry)

# Give the default user a 'universe' capability and save its key,
# so it can be used later by the administrative tools. This effectively
# bootstraps our security system by creating a powerful user.
Security.User().saveKey('~/.cia_key', 'universe')

# Create a root XML-RPC object, with interfaces attached for each subsystem
rpc = RpcServer.getRootInterface()
rpc.putSubHandler('hub', Message.HubInterface(hub))
rpc.putSubHandler('mail', IncomingMail.MailInterface(hub))
rpc.putSubHandler('ruleset', Ruleset.RulesetInterface(rulesetStorage))
rpc.putSubHandler('stats', Stats.Interface.StatsInterface())
rpc.putSubHandler('security', Security.SecurityInterface())
rpc.putSubHandler('debug', Debug.DebugInterface())

internet.TCPServer(port, Web.Server.Site(rpc), interface='localhost').setServiceParent(application)

### The End ###
