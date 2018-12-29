""" LibCIA.Web.Overview

A front page for CIA showing a concise overview of the day's open
source development activity.

Note that this is currently the *only* module in CIA that makes
assumptions about the way the stats hierarchy is organized. If you
have CIA set up differently, you will need to modify this page
or choose not to use it in your .tac file.
"""
#
# CIA open source notification system
# Copyright (C) 2003-2007 Micah Dowty <micah@navi.cx>
# Copyright (C) 2013-2019 Justin Crawford <Justin@stacksmash.net>
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

from twisted.internet import defer

from . import Template
from cia.LibCIA import Database
from cia.LibCIA.Web.Stats import Columns
import Nouvelle


class ActivitySection(Template.Section):
    """A Section displaying links to the most or least active items within
       a given stats target.
       """
    query = """
    SELECT
        STAT.target_path,
        ST.title,
        ICO.path,
        STAT.%(counter_attrib)s,
        ICO.width,
        ICO.height
    FROM stats_counters STAT FORCE INDEX (%(counter_attrib)s)
        LEFT OUTER JOIN stats_catalog  T           ON (STAT.target_path = T.target_path)
        LEFT OUTER JOIN stats_statstarget ST       ON (T.target_path = ST.path)
        LEFT OUTER JOIN images_imageinstance ICO   ON (ICO.source_id = IF(ST.icon_id IS NOT NULL, ST.icon_id, ST.photo_id) AND ICO.thumbnail_size = 32)
        WHERE STAT.name = %(counter)s AND T.parent_path = %(path)s
    ORDER BY STAT.%(counter_attrib)s %(sort)s
    LIMIT %(limit)d
    """
    query_data = None

    def __init__(self, targetPath, title,
                 numItems      = 15,
                 counter       = 'today',
                 counterAttrib = 'event_count',
                 sort          = 'DESC',
                 columnTitle   = 'events today',
                 hint          = '',
                 ):
        self.targetPath = targetPath
        self.title = title
        self.numItems = numItems
        self.counter = counter
        self.counterAttrib = counterAttrib
        self.sort = sort
        self.columnTitle = columnTitle
        self.title = title
        self.hint = hint
        self.initColumns()

    def initColumns(self):
        self.columns = [
            Columns.IndexedIconColumn(iconIndex=2, widthIndex=4, heightIndex=5),
            Columns.TargetTitleColumn(pathIndex=0, titleIndex=1),
            Columns.IndexedBargraphColumn(self.columnTitle, 3),
            ]

    def render_rows(self, context):
        if not self.query_data:
            self.query_data = {
                'path': self.targetPath,
                'limit': self.numItems,
                'counter': self.counter,
                'counter_attrib': self.counterAttrib,
                'sort': self.sort,
                'hint': self.hint
            }
        # First we run a big SQL query to gather all the data for this catalog.
        # Control is passed to _render_rows once we have the query results.
        result = defer.Deferred()
        Database.pool.runQuery(self.query, self.query_data).addCallback(self._render_rows, context, result).addErrback(result.errback)
        return result

    def _render_rows(self, queryResults, context, result):
        # Use a non-resortable table for this, since we've already dictated
        # how our data is sorted in the SQL query and the user can't do much about it yet.
        result.callback([Nouvelle.Table.BaseTable(list(queryResults), self.columns)])


class TimestampSection(ActivitySection):
    """A section showing the newest or oldest items within a stats target"""

    def __init__(self, targetPath, title,
                 numItems      = 15,
                 counter       = 'forever',
                 counterAttrib = 'first_time',
                 sort          = 'DESC',
                 columnTitle   = 'first event',
                 ):
        ActivitySection.__init__(self, targetPath, title, numItems,
                                 counter, counterAttrib, sort, columnTitle,
				 hint='USE INDEX (first_time)')
                                 #hint='USE INDEX (recent_name)')

    def initColumns(self):
        self.columns = [
            Columns.IndexedIconColumn(iconIndex=2, widthIndex=4, heightIndex=5),
            Columns.TargetTitleColumn(pathIndex=0, titleIndex=1),
            Columns.TargetLastEventColumn(self.columnTitle, 3),
            ]


class OverviewPage(Template.Page):
    """A web page showing an overview of open source activity, meant to
       be used as a front page for the CIA web site. We want to act as
       a jumping-off point for the rest of the site, so this page doesn't
       include its own sidebar- it will copy the sidebar from a given page.
       """

    titleElements = [
        Nouvelle.tag('img', _class='banner', src='/media/img/banner-70-nb.png', width=329, height=52,
            alt='CIA.vc: The open source version control informant.'),
    ]

    logoElements = []

    heading = Template.pageBody[
        "This is a brief overview of the information collected recently. ",
        Nouvelle.tag("a", href="/doc")[ "Learn more about CIA" ],
        ]

    def __init__(self, sidebarPath='doc/.default.sidebar'):
        self.leftColumn = self._loadSidebar(sidebarPath)

    def _loadSidebar(self, path):
        """Load sidebar links from a simple text file format.
           Lines beginning with a dash specify a new section heading,
           links are made by lines of the form 'title :: URL'.
           Other lines are ignored.
           """
        sections = []
        for line in open(path):
            line = line.strip()
            if not line:
                continue

            if line[0] == '-':
                sections.append(Template.StaticSection(line[1:].strip()))
                continue

            pieces = line.split("::", 1)
            if len(pieces) > 1:
                title, url = pieces
                sections[-1].rows.append( Nouvelle.tag('a', href=url.strip())[title.strip()] )

        return sections

    def render_mainColumn(self, context):
        return [
            self.heading,
            [
                Template.SectionGrid(
                    [
                        ActivitySection("project", "Most active projects today"),
                        ActivitySection("author", "Most active authors today"),
                    ],
                    [
                        TimestampSection("project", "Newest projects"),
                        TimestampSection("author", "Newest authors"),
                    ],
                ),
            ]
        ]

### The End ###
