# -*- mode: python; -*-
#
# This is a .tac configuration file that sets up a full CIA
# server with all the bells and whistles. It requires a MySQL
# database for rulesets and stats, and provides a full web
# interface.
#
# Start the server by running 'twistd -oy' on this file.
#

from twisted.application import service, internet
from twisted.web import vhost
from twisted.internet import ssl
from LibCIA import Database, Message, Ruleset, IRC, Stats, IncomingMail, Cron
from LibCIA import Debug, Security, RpcServer, RpcClient, Web, Cache
from twisted.internet import tcp
from Nouvelle import tag

Database.init()

# Only bind on one interface
tcp.Port.interface = "10.2.0.2"

# A donation box, in the form of a section we add to the page template
class DonationSection(Web.Template.Section):
    title = "Donate to CIA"
    rows = [
        "Want to help improve CIA's reliability?",
        tag('form', action='https://www.paypal.com/cgi-bin/webscr', method='post')[
            tag('input', type='hidden', _name='cmd', value='_xclick'),
            tag('input', type='hidden', _name='business', value='micahjd@users.sourceforge.net'),
            tag('input', type='hidden', _name='item_name', value='CIA Open Source Notification System'),
            tag('input', type='hidden', _name='no_note', value='1'),
            tag('input', type='hidden', _name='currency_code', value='USD'),
            tag('input', type='hidden', _name='tax', value='0'),
            tag('input', type='image', _name='submit',
                src='http://www.paypal.com/en_US/i/btn/x-click-but21.gif',
                alt='Make a donation via PayPal'),
        ],
    ]

# Add some extras to the web template- add a notice from our wonderfully
# reliable hosting provider, remove the non-main CIA server notice,
# since this is in fact the main server.
#
Web.Template.Page.site_hostingNotice = [
        "Hosting provided by:",
        tag('a', href="http://hyperreal.info/")[
            tag('img', src="http://hyperreal.info/sys/ads/adview.php.gif",
                alt="Hosting provider", width=88, height=31, _class="footer"),
        ],
    ]
Web.Template.Page.site_mainServerNotice = []
Web.Template.Page.site_belowFirstSidebox = [DonationSection()]


application = service.Application("cia_server")
hub = Message.Hub()

# A network of IRC bots used to handle irc:// URIs
botNet = IRC.Bots.BotNetwork(IRC.Bots.SequentialNickAllocator("CIA-"))
#botNet = IRC.Bots.BotNetwork(IRC.Bots.RandomAcronymNickAllocator())

# Set up periodic maintenance tasks
Cron.Scheduler(
    Cron.Event(Cron.hourly, Stats.Target.Maintenance().run, "stats maintenance"),
    Cron.Event(Cron.hourly, Cache.Maintenance().run, "cache maintenance"),
    )

# A list of URI handlers that can be used as targets for rulesets
uriRegistry = Ruleset.URIRegistry(
    IRC.Handler.IrcURIHandler(botNet),
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

# Create components we'll need in multiple places later
doc   = Web.Doc.Component('doc')
stats = Web.Stats.Component()

# Present an overview page at the top of our web site, using the documentation
# system's default sidebar to give us an easy way to navigate the site.
frontPage = Web.Overview.OverviewPage(doc.resource, stats)

# Our front page gives us an overview of CIA, but unless otherwise specified
# we load other pages from the 'htdocs' directory, as static files.
webRoot = Web.Server.StaticJoiner('htdocs', frontPage)
site = Web.Server.Site(webRoot)

# Add a VHostMonster we can use to safely proxy requests from Apache running on a different port
webRoot.putChild('vhost', vhost.VHostMonsterResource())

# debuggative cruft
webRoot.putChild('graph', Web.Stats.Graph.GraphPage(
    Stats.Graph.PrefixSelector('project/', color='#FF0000', shape='box'),
    Stats.Graph.PrefixSelector('author/', color='#0000FF'),
    ))

# Create a root XML-RPC object, with interfaces attached for each subsystem
rpc = RpcServer.getRootInterface()
rpc.putSubHandler('hub', Message.HubInterface(hub))
rpc.putSubHandler('mail', IncomingMail.MailInterface(hub))
rpc.putSubHandler('ruleset', Ruleset.RulesetInterface(rulesetStorage))
rpc.putSubHandler('stats', Stats.Interface.StatsInterface())
rpc.putSubHandler('security', Security.SecurityInterface())
rpc.putSubHandler('debug', Debug.DebugInterface())
webRoot.putChild('RPC2', rpc)

# The user-navigable areas of our site are all Component instances
site.putComponent('stats', stats)
site.putComponent('doc', doc)
site.putComponent('irc', Web.BotStatus.Component(botNet))
site.putComponent('rulesets', Web.RulesetBrowser.Component(rulesetStorage))
site.putComponent('info', Web.Info.Component())

# Now create an HTTP server holding both our XML-RPC and web interfaces
internet.TCPServer(3910, site).setServiceParent(application)

# ...and an HTTPS server, which we'll refer to for web logins.
# XML-RPC clients should also use the secure server when they're sending keys.
sslContext = ssl.DefaultOpenSSLContextFactory("conf/server.key", "conf/server.crt")
internet.SSLServer(3914, site, sslContext).setServiceParent(application)
Web.Keyring.setSecureServer(port=3914)

### The End ###
