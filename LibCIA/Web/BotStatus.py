""" LibCIA.Web.BotStatus

A web interface showing the current status of the BotNetwork
"""
#
# CIA open source notification system
# Copyright (C) 2003-2004 Micah Dowty <micah@navi.cx>
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

import Template, Server, Info
from LibCIA import TimeUtil
from Nouvelle import tag, place
import Nouvelle
import time


class Component(Server.Component):
    """A server component for the IRC bot status section"""
    name = 'IRC Bots'

    def __init__(self, botNet):
        self.resource = IRCBotPage(botNet)

    def __contains__(self, page):
        return isinstance(page, IRCBotPage)


class TotalsSection(Template.Section):
    """A Section that displays fun-filled facts about our BotNetwork"""
    def __init__(self, botNet):
        self.totalServers = 0
        self.totalBots = 0
        self.totalChannels = 0
        for server in botNet.servers.iterkeys():
            self.totalServers += 1
            for bot in botNet.servers[server]:
                self.totalBots += 1
                self.totalChannels += len(bot.channels)

        self.numRequests = 0
        self.numUnfulfilled = 0
        for request in botNet.requests:
            self.numRequests += 1
            if not request.isFulfilled():
                self.numUnfulfilled += 1

    title = "totals"
    rows = [
               [
                   Template.value[ place('totalBots') ],
                   ' total IRC bots across ',
                   Template.value[ place('totalServers') ],
                   ' servers, inhabiting ',
                   Template.value[ place('totalChannels') ],
                   ' channels.',
               ],
               [
                   Template.value[ place('numRequests') ],
                   ' requests are currently registered with the bot network. ',
                   Template.value[ place('numUnfulfilled') ],
                   ' of these are unfulfilled.',
               ],
           ]


class ListAttributeColumn(Nouvelle.AttributeColumn):
    """An AttributeColumn that formats lists nicely, and sorts by list length"""
    def render_data(self, context, obj):
        l = getattr(obj, self.attribute)
        return ", ".join(l.iterkeys())

    def isVisible(self, context):
        return context['table'].reduceColumn(self, max)

    def getValue(self, obj):
        return len(getattr(obj, self.attribute))


class BotStatusColumn(Nouvelle.Column):
    """Show the fullness status of the bot"""
    heading = 'status'

    def __init__(self, botNet):
        self.botNet = botNet

    def getValue(self, bot):
        indicators = []

        if bot.isFull():
            indicators.append('full')

        if (not bot.channels) and (not bot.requestedChannels):
            indicators.append('empty')

        if bot in self.botNet.inactiveBots:
            timer = self.botNet.inactiveBots[bot]
            indicators.append('GC in %s' % TimeUtil.formatDuration(timer.getTime() - time.time()))

        return ', '.join(indicators)


class LagColumn(Nouvelle.Column):
    heading = 'lag'

    def getValue(self, bot):
        return bot.getLag()

    def render_data(self, context, bot):
        lag = bot.getLag()
        if lag is None:
            return Template.error["unknown"]
        else:
            return "%.2fs" % lag


class BotsSection(Template.Section):
    """A section holding a table listing all bots"""
    title = 'bots'

    def __init__(self, botNet):
        self.botNet = botNet

        self.columns = [
            Nouvelle.AttributeColumn('server', 'server'),
            Nouvelle.AttributeColumn('nickname', 'nickname'),
            ListAttributeColumn('channels', 'channels'),
            ListAttributeColumn('requested', 'requestedChannels'),
            LagColumn(),
            BotStatusColumn(botNet),
            ]

    def render_rows(self, context):
        bots = []
        for serverBots in self.botNet.servers.itervalues():
            bots.extend(serverBots)
        if bots:
            return [Template.Table(bots, self.columns, id='bots')]
        else:
            return []


class OptionalAttributeStringColumn(Nouvelle.AttributeColumn):
    """A column displaying an object's attribute that converts the
       result into a string and allows it to be blank if the attribute
       doesn't exist in a particular object.
       """
    def getValue(self, obj):
        value = getattr(obj, self.attribute, '')
        if value is None:
            return ()
        else:
            return str(value)


class RequestBotsColumn(Nouvelle.Column):
    """A column showing the bots assigned to a particular request"""
    heading = 'current bots'

    def getValue(self, req):
        return ", ".join([ bot.nickname for bot in req.bots ])


class RequestStatusColumn(Nouvelle.Column):
    """A column that gives a quick indication of a request's fulfillment status"""
    heading = 'status'

    def getValue(self, req):
        return req.isFulfilled()

    def render_data(self, context, req):
        if req.isFulfilled():
            return 'ok'
        else:
            return Template.error["not fulfilled"]


class RequestUsersColumn(Nouvelle.Column):
    """A column that displays the number of users a request serves"""
    heading = '# of users'

    def getValue(self, req):
        return req.getUserCount()

    def render_data(self, context, req):
        uc = req.getUserCount()
        if uc is None:
            return Template.error["unavailable"]
        else:
            return uc

    def cmp(self, a, b):
        # Reverse sort
        return cmp(b.getUserCount(), a.getUserCount())


class DateColumn(Nouvelle.AttributeColumn):
    """An AttributeColumn that formats unix-style timestamps as dates"""
    def render_data(self, context, message):
        return TimeUtil.formatDate(self.getValue(message))


class MessageBotColumn(Nouvelle.Column):
    """Shows the name of the bot that received a particular message in the message log"""
    heading = 'bot'

    def getValue(self, msg):
        return msg.bot.nickname


class MessageServerColumn(Nouvelle.Column):
    """Shows the name of the server a bot is on"""
    heading = 'server'

    def getValue(self, msg):
        return str(msg.bot.factory.server)


class MessageParamsColumn(Nouvelle.Column):
    """Shows the parameters associated with one logged message"""
    heading = 'parameters'

    def getValue(self, msg):
        return repr(msg.params)[1:-1]


class MessageLogSection(Template.Section):
    """A section listing recent unknown IRC messages, including errors"""
    title = 'recent unhandled messages'

    columns = [
        DateColumn('time', 'timestamp'),
        MessageBotColumn(),
        MessageServerColumn(),
        Nouvelle.AttributeColumn('code', 'command'),
        MessageParamsColumn(),
       ]

    def __init__(self, botNet):
        self.botNet = botNet

    def render_rows(self, context):
        messages = self.botNet.unknownMessageLog.buffer
        if messages:
            return [Template.Table(messages, self.columns, id='msglog')]
        else:
            return []


class RequestsSection(Template.Section):
    """A section listing all requests being asked of the BotNetwork"""
    title = 'requests'

    columns = [
        OptionalAttributeStringColumn('channel', 'channel'),
        OptionalAttributeStringColumn('server', 'server'),
        OptionalAttributeStringColumn('# of bots', 'numBots'),
        RequestStatusColumn(),
        RequestUsersColumn(),
        RequestBotsColumn(),
       ]

    def __init__(self, botNet):
        self.botNet = botNet

    def render_rows(self, context):
        if self.botNet.requests:
            return [Template.Table(self.botNet.requests, self.columns, id='requests')]
        else:
            return []


class NewBotsSection(Template.Section):
    """A section listing new bots we're waiting on connections from"""
    title = 'new bots'

    columns = [
        Nouvelle.IndexedColumn('connecting to...', 0),
        Nouvelle.IndexedColumn('time remaining', 1),
       ]

    def __init__(self, botNet):
        self.botNet = botNet

    def render_rows(self, context):
        newBots = []
        for server, timer in self.botNet.newBotServers.iteritems():
            newBots.append((server, TimeUtil.formatDuration(timer.getTime() - time.time())))
        if newBots:
            return [Template.Table(newBots, self.columns, id='newBots')]
        else:
            return []


class IRCBotPage(Template.Page):
    """A web page showing the status of the BotNetwork"""
    mainTitle = "IRC Bot Status"
    subTitle = "When you give a mouse a cookie, everything looks like a nail"

    def __init__(self, botNet):
        self.botNet = botNet

    def render_mainColumn(self, context):
        return [
            BotsSection(self.botNet),
            NewBotsSection(self.botNet),
            MessageLogSection(self.botNet),
            RequestsSection(self.botNet),
            ]

    def render_leftColumn(self, context):
        return [
            TotalsSection(self.botNet),
            Info.Clock(),
            ]

### The End ###
