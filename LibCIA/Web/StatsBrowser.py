""" LibCIA.Web.StatsBrowser

A web interface for CIA's stats:// namespace
"""
#
# CIA open source notification system
# Copyright (C) 2003-2004 Micah Dowty <micahjd@users.sourceforge.net>
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

from __future__ import division
import time, math
import Template, Nouvelle
import Nouvelle.Twisted
from LibCIA import TimeUtil, Message, Stats, Database
from twisted.web import resource, error, server
from twisted.internet import defer
from Nouvelle import tag, place, subcontext


class Clock(Template.Section):
    title = "UTC clock"

    def render_rows(self, context):
        return [TimeUtil.formatDate(time.time())]


class Counters(Template.Section):
    """A Section displaying the counters from a StatsTarget"""
    title = "event counters"
    rows = []

#     rows = [
#                [
#                    'The last message was received ',
#                    Template.value[ place('relativeDate', 'forever', 'lastEventTime') ],
#                    ' ago at ',
#                    Template.value[ place('date', 'forever', 'lastEventTime') ],
#                ],
#                [
#                    Template.value[ place('value', 'today', 'eventCount') ],
#                    ' messages so far today, ',
#                    Template.value[ place('value', 'yesterday', 'eventCount') ],
#                    ' messages yesterday',
#                ],
#                [
#                    Template.value[ place('value', 'thisWeek', 'eventCount') ],
#                    ' messages so far this week, ',
#                    Template.value[ place('value', 'lastWeek', 'eventCount') ],
#                    ' messages last week',
#                ],
#                [
#                    Template.value[ place('value', 'thisMonth', 'eventCount') ],
#                    ' messages so far this month, ',
#                    Template.value[ place('value', 'lastMonth', 'eventCount') ],
#                    ' messages last month',
#                ],
#                [
#                    Template.value[ place('value', 'forever', 'eventCount') ],
#                    ' messages since the first one, ',
#                    Template.value[ place('relativeDate', 'forever', 'firstEventTime') ],
#                    ' ago',
#                    place('averagePeriod', 'forever'),
#                ],
#         ]

    def __init__(self, target):
        self.counters = target.counters

    def isVisible(self, context):
        """Hide the counters if this target has never received an event"""
        return self.counters.getCounter('forever').get('eventCount', 0) != 0

    def render_value(self, context, counterName, valueName):
        return self.counters.getCounter(counterName).get(valueName, 0)

    def render_date(self, context, counterName, valueName):
        value = self.counters.getCounter(counterName).get(valueName)
        if value is not None:
            return TimeUtil.formatDate(value)

    def render_duration(self, context, counterName, valueName):
        value = self.counters.getCounter(counterName).get(valueName)
        if value is not None:
            return TimeUtil.formatDuration(value)

    def render_relativeDate(self, context, counterName, valueName):
        value = self.counters.getCounter(counterName).get(valueName, 0)
        if value is not None:
            return TimeUtil.formatDuration(time.time() - value)

    def render_averagePeriod(self, context, counterName):
        counter = self.counters.getCounter(counterName)
        events = counter.get('eventCount', 0)
        first = counter.get('firstEventTime')
        last = counter.get('lastEventTime')
        if events < 2 or (not first) or (not last) or first == last:
            return ''
        return [
            ', for an average of ',
            Template.value[ TimeUtil.formatDuration( (last - first) / events ) ],
            ' between messages',
            ]


class TargetRelativeLink:
    """Abstract base class for a link to a stats target or something relative to it"""
    def __init__(self, target, relativePathSegments=()):
        self.target = target
        self.relativePathSegments = tuple(relativePathSegments)

    def getURL(self, context):
        return context['statsRootPath'] + '/'.join(tuple(self.target.pathSegments) + self.relativePathSegments)


class StatsLink(TargetRelativeLink):
    """An anchor tag linking to the given stats target.
       Text for the link may be specified, but by default the
       target's title is used.
       """
    def __init__(self, target, tagFactory=tag('a'), text=None):
        TargetRelativeLink.__init__(self, target)
        self.tagFactory = tagFactory
        self.text = text

    def render(self, context):
        text = self.text
        if text is None:
            text = self.target.getTitle()
        return self.tagFactory(href=self.getURL(context))[text]


class MetadataLink(TargetRelativeLink):
    """An anchor tag linking to an item in the given stats target's metadata.
       Text for the link may be specified, but by default the key is used.

       This class only works for keys that are strings. A key of None links
       to the metadata index.
       """
    def __init__(self, target, key=None, tagFactory=tag('a'), text=None):
        segments = ['.metadata']
        if key:
            segments.append(key)
        TargetRelativeLink.__init__(self, target, segments)

        self.tagFactory = tagFactory
        self.key = key
        self.text = text

    def render(self, context):
        text = self.text
        if text is None:
            if self.key is None:
                text = "View/Edit Metadata"
            else:
                text = self.key
        return self.tagFactory(href=self.getURL(context))[text]


class RSSLink(TargetRelativeLink):
    """An anchor tag linking to the RSS feed for a given stats target"""
    def __init__(self, target, tagFactory=tag('a'), text=None):
        TargetRelativeLink.__init__(self, target, ('.rss',))
        self.tagFactory = tagFactory
        self.text = text

    def render(self, context):
        text = self.text
        if text is None:
            text = "RSS 2.0 Feed"
        return self.tagFactory(href=self.getURL(context))[text]


class TargetTitleColumn(Nouvelle.Column):
    """A Column displaying a target's title as a StatsLink. To avoid having to make
       a query for every row in the table, this looks for the title to use in the
       indicated SQL query column number.
       """
    heading = 'title'
    def __init__(self, pathIndex, titleIndex):
        self.pathIndex = pathIndex
        self.titleIndex = titleIndex

    def _findTitle(self, row):
        """Use our titleIndex to return the title according to the metadata if that
           exists, otherwise make up a title based on the path.
           """
        title = row[self.titleIndex]
        if title is None:
            return Stats.StatsTarget(row[self.pathIndex]).name
        else:
            return title
        # Note that we don't have to worry about the case (name is None)
        # because this is only used for listing children of some target,
        # and the root target has no parent.

    def getValue(self, row):
        # For case-insensitive sorting by title
        return self._findTitle(row).lower()

    def render_data(self, context, row):
        # Create a StatsLink. Note that we could leave it up to the StatsLink
        # to look up the title, but that would end up creating an SQL query
        # for each row in the table- not good when we're trying to view a page
        # with 1000 projects without making our server explode.
        target = Stats.StatsTarget(row[self.pathIndex])
        return StatsLink(target, text=self._findTitle(row))


class IndexedUnitColumn(Nouvelle.IndexedColumn):
    """A Nouvelle.IndexedColumn that appends a fixed unit qualifier to each rendered item.
       Hides individual cells when zero, hides the column when all cells are zero.
       """
    def __init__(self, heading, index, singularUnit='item', pluralUnit='items'):
        self.singularUnit = singularUnit
        self.pluralUnit = pluralUnit
        Nouvelle.IndexedColumn.__init__(self, heading, index)

    def isVisible(self, context):
        return context['table'].reduceColumn(self, self._visibilityTest)

    def _visibilityTest(self, seq):
        """Visibility testing operator for this column's data"""
        for item in seq:
            if item != 0:
                return True
        return False

    def render_data(self, context, row):
        value = row[self.index]
        if value == 0:
            return ''
        elif value == 1:
            unit = self.singularUnit
        else:
            unit = self.pluralUnit
        return "%s %s" % (row[self.index], unit)


class RankIndexedColumn(Nouvelle.IndexedColumn):
    """An IndexedColumn tweaked for numeric rankings.
       None is treated as zero, sorting is reversed, the column
       is hidden if all values are zero or less.
       """
    def cmp(self, a, b):
        """Reverse sort"""
        return -cmp(a[self.index],b[self.index])

    def getValue(self, row):
        return row[self.index] or 0

    def isVisible(self, context):
        return context['table'].reduceColumn(self, self._visibilityTest)

    def _visibilityTest(self, seq):
        """Visibility testing operator for this column's data"""
        for item in seq:
            if item != 0:
                return True
        return False


class IndexedBargraphColumn(RankIndexedColumn):
    """An IndexedColumn that renders as a logarithmic bar chart"""
    def render_data(self, context, row):
        value = row[self.index]
        if not value:
            return ''
        logMax = math.log(context['table'].reduceColumn(self, max))
        if logMax > 0:
            fraction = math.log(value) / logMax
        else:
            fraction = 1
        return Template.Bargraph(fraction)[ value ]


class IndexedPercentColumn(RankIndexedColumn):
    """An IndexedColumn that renders itself as a percent of the column's total"""
    def render_data(self, context, row):
        value = row[self.index]
        if not value:
            return ''
        total = context['table'].reduceColumn(self, sum)
        return "%.03f" % (value / total * 100)


class TargetLastEventColumn(Nouvelle.IndexedColumn):
    """A Column displaying the amount of time since the last message was delivered to
       each target, given a column containing the last event timestamp.
       """
    def getValue(self, row):
        """Returns the number of seconds since the last event"""
        lastTime = row[self.index]
        if lastTime:
            return time.time() - lastTime

    def isVisible(self, context):
        return context['table'].reduceColumn(self, self._visibilityTest)

    def _visibilityTest(self, seq):
        """Visibility testing operator for this column's data"""
        for item in seq:
            if item:
                return True
        return False

    def render_data(self, context, target):
        value = self.getValue(target)
        if value is None:
            return ''
        else:
            return "%s ago" % TimeUtil.formatDuration(self.getValue(target))


class Catalog(Template.Section):
    """A Section displaying links to all children of a StatsTarget, with
       other information about the children displayed as applicable.
       """
    title = "catalog"

    query = """
    SELECT
        T.target_path,
        M_TITLE.value,
        C_TODAY.event_count,
        C_YESTERDAY.event_count,
        C_FOREVER.event_count,
        C_FOREVER.last_time,
        COUNT(CHILD.target_path)
    FROM stats_catalog T
        LEFT OUTER JOIN stats_catalog  CHILD       ON (CHILD.parent_path = T.target_path)
        LEFT OUTER JOIN stats_metadata M_TITLE     ON (T.target_path = M_TITLE.target_path     AND M_TITLE.name     = 'title')
        LEFT OUTER JOIN stats_counters C_TODAY     ON (T.target_path = C_TODAY.target_path     AND C_TODAY.name     = 'today')
        LEFT OUTER JOIN stats_counters C_YESTERDAY ON (T.target_path = C_YESTERDAY.target_path AND C_YESTERDAY.name = 'yesterday')
        LEFT OUTER JOIN stats_counters C_FOREVER   ON (T.target_path = C_FOREVER.target_path   AND C_FOREVER.name   = 'forever')
        WHERE T.parent_path = %(path)s
    GROUP BY
        T.target_path,
        M_TITLE.value,
        C_TODAY.event_count,
        C_YESTERDAY.event_count,
        C_FOREVER.event_count,
        C_FOREVER.last_time
    """

    columns = [
        TargetTitleColumn(pathIndex=0, titleIndex=1),
        IndexedBargraphColumn('events today', 2),
        IndexedBargraphColumn('events yesterday', 3),
        IndexedBargraphColumn('total events', 4),
        IndexedPercentColumn('% total', 4),
        TargetLastEventColumn('last event', 5),
        IndexedUnitColumn('contents', 6),
        ]

    def __init__(self, target):
        self.target = target

    def render_rows(self, context):
        # First we run a big SQL query to gather all the data for this catalog.
        # Control is passed to _render_rows once we have the query results.
        result = defer.Deferred()
        Database.pool.runQuery(self.query % {
            'path': Database.quote(self.target.path, 'varchar'),
            }).addCallback(
            self._render_rows, context, result
            ).addErrback(result.errback)
        return result

    def _render_rows(self, queryResults, context, result):
        if queryResults:
            result.callback([Template.Table(list(queryResults), self.columns, id='catalog')])
        else:
            result.callback(None)


class Info(Template.Section):
    """A section that displays a StatsTarget's miscellaneous metadata"""
    title = "information"

    def __init__(self, target):
        self.target = target
        self.metadata = target.metadata

    def render_rows(self, context):
        # Grab the metadata keys we'll need and wait for them to become available
        result = defer.Deferred()
        defer.gatherResults([
            self.metadata.getValue('url'),
            self.metadata.getValue('description'),
            self.metadata.has_key('photo'),
            ]).addCallback(self._render_rows, context, result).addErrback(result.errback)
        return result

    def _render_rows(self, metadata, context, result):
        url, description, hasPhoto = metadata
        rows = []
        if url:
            rows.append(tag('a', href=url)[url])
        if hasPhoto:
            rows.append(Template.Photo(MetadataLink(self.target, 'photo').getURL(context)))
        if description:
            rows.append(description)
        result.callback(rows)


class MessageDateColumn(Nouvelle.Column):
    """A column that displays a message's date"""
    heading = 'date'

    def getValue(self, message):
        return int(str(message.xml.timestamp))

    def render_data(self, context, message):
        return TimeUtil.formatDate(self.getValue(message))


class MessageProjectColumn(Nouvelle.Column):
    """A column that displays the project a message originated from"""
    heading = 'project'

    def getValue(self, message):
        if message.xml.source:
            return str(message.xml.source.project)


class MessageContentColumn(Nouvelle.Column):
    """A column that displays a message, formatted in XHTML"""
    heading = 'content'

    def getValue(self, message):
        return Message.AutoFormatter('xhtml').format(message)


class MessageList(Template.Section):
    """A list of messages, with metadata and hyperlinks. Must be
       constructed with a list of Message instances.
       """
    title = "messages"

    columns = [
        MessageDateColumn(),
        MessageProjectColumn(),
        MessageContentColumn(),
        ]

    def renderMessages(self, context, messages):
        return [
            Template.Table(messages, self.columns,
                           defaultSortReversed = True,
                           id = 'message'),
            ]


class RecentMessages(MessageList):
    """A section displaying recent messages from a given stats target"""
    title = "recent messages"

    def __init__(self, target, limit=20):
        self.target = target
        self.limit = limit

    def render_rows(self, context):
        result = defer.Deferred()
        self.target.messages.getLatest(self.limit).addCallback(
            self._render_rows, context, result).addErrback(result.errback)
        return result

    def _render_rows(self, messages, context, result):
        """Actually render the rows, called after the message list has been retrieved"""
        if messages:
            result.callback(self.renderMessages(context, [Message.Message(m) for m in messages]))
        else:
            result.callback(None)


class MetadataKeyColumn(Nouvelle.Column):
    """A column that displays a metadata key as a hyperlink to the proper MetadataValuePage.
       Rows are expected to be (name, (value, type)) tuples.
       """
    heading = 'key'

    def getValue(self, item):
        return item[0]

    def render_data(self, context, item):
        return MetadataLink(context['target'], item[0])


class MetadataValueColumn(Nouvelle.Column):
    """A column that displays a metadata key's associated value, formatted appropriately.
       Rows are expected to be (name, (value, type)) tuples.
       """
    heading = 'value'

    def getValue(self, item):
        """This is used for sorting and such, so don't bother
           returning anything if we're not dealing with text.
           """
        value, mime = item[1]
        if mime.startswith("text/"):
            return value

    def render_data(self, context, item):
        """Farm this off to an appropriate handler for the data's MIME type"""
        value, mime = item[1]
        try:
            # First look for a handler for one particular MIME type after
            # replacing the / with an underscore
            f = getattr(self, 'renderData_' + mime.replace('/', '_'))
        except AttributeError:
            try:
                # Nope, try looking for a handler for only the content type,
                # not the subtype
                f = getattr(self, 'renderData_' + mime.split('/')[0])
            except AttributeError:
                # Nothing, use a generic handler
                f = self.renderData_other
        return f(context, item[0], value, mime)

    def renderData_text(self, context, name, value, mime):
        return value

    def renderData_image(self, context, name, value, mime):
        """Return an <img> tag linking to the key's value"""
        return tag('img', src=MetadataLink(context['target'], name).getURL(context))

    def renderData_other(self, context, name, value, mime):
        return tag('i')[ "Unable to format data" ]


class MetadataTypeColumn(Nouvelle.Column):
    """A column that displays a metadata key's MIME type.
       Rows are expected to be (name, (value, type)) tuples.
       """
    heading = 'type'

    def getValue(self, item):
        return item[1][1]


class MetadataSection(Template.Section):
    """A section displaying a table of metadata keys for one stats target"""
    title = "metadata"

    columns = [
        MetadataKeyColumn(),
        MetadataTypeColumn(),
        MetadataValueColumn(),
        ]

    def __init__(self, target):
        self.target = target

    def render_rows(self, context):
        # Look up all the metadata first
        result = defer.Deferred()
        self.target.metadata.dict().addCallback(
            self._render_rows, context, result).addErrback(result.errback)
        return result

    def _render_rows(self, metadict, context, result):
        # Each 'row' in our table is a (name, (value, type)) tuple
        rows = metadict.items()
        result.callback([
            subcontext(target=self.target)[
                Template.Table(rows, self.columns, id='metadata')
            ]])


class MetadataValuePage(resource.Resource):
    """A web resource that returns the raw value of a metadata key, with the proper MIME type"""
    def __init__(self, target, key):
        self.target = target
        self.key = key
        resource.Resource.__init__(self)

    def render(self, request):
        # Retrieve the metadata value, rendering the page once it arrives
        self.target.metadata.get(self.key).addCallback(self._render, request).addErrback(request.processingFailed)
        return server.NOT_DONE_YET

    def _render(self, t, request):
        if t:
            value, mimeType = t
            request.setHeader('content-type', mimeType)
            request.write(value)
            request.finish()
        else:
            request.write(error.NoResource("No such metadata key %r" % self.key).render(request))
            request.finish()


class RSSFeed(Nouvelle.Twisted.Page):
    """A web resource representing an RSS feed for a particular stats target.
       We use Nouvelle here to generate RSS rather than XHTML :)
       """
    def __init__(self, statsPage):
        self.statsPage = statsPage
        self.target = statsPage.target
        Nouvelle.Twisted.Page.__init__(self)

    def preRender(self, context):
        context['request'].setHeader('content-type', 'text/xml')
        context['statsRootPath'] = self.statsPage.findRootPath(context['request'], 1, absolute=True)

    def render_title(self, context):
        return self.target.getTitle()

    def render_link(self, context):
        return self.target.metadata.get('url', StatsLink(self.target).getURL(context))

    def render_description(self, context):
        return self.target.metadata.get('description', 'CIA Stats')

    def render_photo(self, context):
        # First figure out if we have a photo. Actually render it in the Deferred if we do.
        result = defer.Deferred()
        self.target.metadata.has_key('photo').addCallback(
            self._render_photo, context, result).addErrback(result.errback)
        return result

    def _render_photo(self, hasPhoto, context, result):
        if hasPhoto:
            result.callback(tag('image')[
                tag('url')[ MetadataLink(self.target, 'photo').getURL(context) ],
                tag('title')[ place('title') ],
                tag('link')[ place('link') ],
                ])
        else:
            result.callback([])

    def render_items(self, context, limit=15):
        """Renders the most recent commits as items in the RSS feed"""
        # Get the messages, render them in our Deferred
        result = defer.Deferred()
        self.target.messages.getLatest(limit).addCallback(
            self._render_items, context, result).addErrback(result.errback)
        return result

    def _render_items(self, messages, context, result):
        formatter = Message.AutoFormatter('rss')
        items = []
        for m in messages:
            i = formatter.format(Message.Message(m))
            if i:
                items.append(i)
            else:
                # We can't find a formatter, stick in a placeholder noting this fact
                items.append(tag('item')[ tag('description')[
                    "(Unable to format message)"
                    ]])
        result.callback(items)

    document = tag('rss', version='2.0')[ tag('channel')[
        tag('title')[ place('title') ],
        tag('link')[ place('link') ],
        tag('description')[ place('description') ],
        place('photo'),
        place('items'),
        ]]


class StatsLinksSection(Template.Section):
    """A section displaying useful links for a particular stats target"""
    title = 'links'

    def __init__(self, target):
        self.target = target

    def render_rows(self, context):
        return [
            MetadataLink(self.target),
            RSSLink(self.target),
            ]


class MetadataPage(Template.Page):
    """A web page providing an interface for viewing and editing a StatsTarget's
       metadata. Children of this page are pages that render individual metadata keys
       with no extra formatting.
       """
    def __init__(self, statsPage):
        self.statsPage = statsPage

    def preRender(self, context):
        context['statsRootPath'] = self.statsPage.findRootPath(context['request'], 1)

    def render_mainTitle(self, context):
        return "Metadata for stats://%s" % "/".join(self.statsPage.target.pathSegments)

    def render_mainColumn(self, context):
        return [
            MetadataSection(self.statsPage.target),
            ]

    def getChildWithDefault(self, name, request):
        """Part of IResource, called by twisted.web to retrieve pages for URIs
           below this one. Children of a MetadataPage are MetadataValuePages
           """
        if not name:
            # Ignore empty path sections
            return self
        else:
            return MetadataValuePage(self.statsPage.target, name)

    def render_headingTabs(self, context):
        tabs = self.statsPage.render_headingTabs(context)
        tabs.append(StatsLink(self.statsPage.target, Template.headingTab))
        return tabs

    leftColumn = [
        Template.StaticSection('information', [
        "This page lists all the metadata associated with a particular stats target.",
        "A stats target is anything that can keep track of a particular category of "
        "messages and/or holds other stats targets. Metadata for these stats targets "
        "control how they are displayed in the stats browser.",
        ])]


class StatsPage(Template.Page):
    """A web page providing an interface to one StatsTarget.
       The root of the stats namespace should be created with the
       capabilities database and StatsStorage. Children will
       be automatically created with child targets.
       """
    def __init__(self, target=None):
        if target is None:
            target = Stats.StatsTarget()
        self.target = target

    def getChildWithDefault(self, name, request):
        """Part of IResource, called by twisted.web to retrieve pages for URIs
           below this one. This just creates a StatsPage instance for our StatsTarget's child,
           with a few special cases used for metadata and editing.
           """
        if not name:
            # Ignore empty path sections
            return self
        elif name == '.metadata':
            # Return a special page that accesses this stats target's metadata
            return MetadataPage(self)
        elif name == '.rss':
            # Return an RSS feed with this stats target's most recent commits in XML
            return RSSFeed(self)
        else:
            # Return the stats page for a child
            return StatsPage(self.target.child(name))

    def findRootPath(self, request, additionalDepth=0, absolute=False):
        """Find the URL path referring to the root of the current stats tree
           The returned path begins and ends with a slash.

           additionalDepth can be set to a nonzero number if the caller
           knows the request is actually for a page below this one in the URL tree.

           If 'absolute' is True, a full absolute URL is returned.
           """
        pathSegments = [s for s in request.path.split('/') if s]
        treeDepth = len(self.target.pathSegments) + additionalDepth
        if treeDepth:
            pathSegments = pathSegments[:-treeDepth]
        path = '/' + '/'.join(pathSegments) + '/'

        # Cheesily stick together an absolute URL
        if absolute:
            ns, host, port = request.host
            if port != 80:
                host += ':' + str(port)
            path = 'http://' + host + path

        return path

    def preRender(self, context):
        context['statsRootPath'] = self.findRootPath(context['request'])

    def render_mainTitle(self, context):
        return self.target.getTitle()

    def render_subTitle(self, context):
        return self.target.metadata.get('subtitle', 'Real-time open source activity stats')

    def render_mainColumn(self, context):
        return [
            Counters(self.target),
            Catalog(self.target),
            RecentMessages(self.target),
            ]

    def render_leftColumn(self, context):
        return [
            Info(self.target),
            StatsLinksSection(self.target),
            Clock(),
            ]

    def render_headingTabs(self, context):
        """Create tabs linking to all our parent stats targets and to the CIA root"""
        tabs = []
        node = self.target.parent()
        while node:
            tabs.insert(0, StatsLink(node, Template.headingTab))
            node = node.parent()
        tabs.insert(0, Template.headingTab(href='/')['CIA'])
        return tabs

### The End ###
