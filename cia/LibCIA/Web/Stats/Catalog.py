""" LibCIA.Web.Stats.Catalog

Implements the 'catalog' section in stats, pages, a fast way to
get an overview of all stats targets directly under the current one.
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

from twisted.internet import defer

from cia.LibCIA.Web import Template
from cia.LibCIA import Database
from . import Columns


class CatalogSection(Template.Section):
    """A Section displaying links to all children of a StatsTarget, with
       other information about the children displayed as applicable.
       """
    title = "catalog"
    limit = 100

    query = """
    SELECT
        T.target_path,
        ST.title,
        ICO.path,
        C_TODAY.event_count,
        C_YESTERDAY.event_count,
        C_FOREVER.event_count,
        C_FOREVER.last_time,
        (SELECT COUNT(*) FROM stats_catalog CHILD WHERE CHILD.parent_path = T.target_path),
        ICO.width,
        ICO.height
    FROM stats_catalog T
        LEFT OUTER JOIN stats_statstarget ST       ON (T.target_path = ST.path)
        LEFT OUTER JOIN images_imageinstance ICO   ON (ICO.source_id = IF(ST.icon_id IS NOT NULL, ST.icon_id, ST.photo_id) AND ICO.thumbnail_size = 32)
        LEFT OUTER JOIN stats_counters C_TODAY     ON (T.target_path = C_TODAY.target_path     AND C_TODAY.name     = 'today')
        LEFT OUTER JOIN stats_counters C_YESTERDAY ON (T.target_path = C_YESTERDAY.target_path AND C_YESTERDAY.name = 'yesterday')
        LEFT OUTER JOIN stats_counters C_FOREVER   ON (T.target_path = C_FOREVER.target_path   AND C_FOREVER.name   = 'forever')
        WHERE T.parent_path = %(path)s
    ORDER BY NULL LIMIT %(limit)s
    """

    columns = [
        Columns.IndexedIconColumn(iconIndex=2, widthIndex=8, heightIndex=9),
        Columns.TargetTitleColumn(pathIndex=0, titleIndex=1),
        Columns.IndexedBargraphColumn('events today', 3),
        Columns.IndexedBargraphColumn('events yesterday', 4),
        Columns.IndexedBargraphColumn('total events', 5),
        Columns.IndexedPercentColumn('% total', 5),
        Columns.TargetLastEventColumn('last event', 6),
        Columns.IndexedUnitColumn('contents', 7),
    ]

    def __init__(self, target):
        self.target = target

    def render_rows(self, context):
        # First we run a big SQL query to gather all the data for this catalog.
        # Control is passed to _render_rows once we have the query results.
        result = defer.Deferred()
        Database.pool.runQuery(self.query % {
            'path': Database.quote(self.target.path, 'varchar'),
            'limit': self.limit,
        }).addCallback(
            self._render_rows, context, result
        ).addErrback(result.errback)
        return result

    def _render_rows(self, queryResults, context, result):
        if queryResults:
            content = [Template.Table(list(queryResults), self.columns,
                                      id='catalog',
                                      defaultSortColumnIndex=1)]
            if len(queryResults) == self.limit:
                content.insert(0, Template.longError[
                    "This page has a very large number of child items, "
                    "and CIA can not yet display them all or browse them "
                    "incrementally. Below is an arbitrary set of %d "
                    "items. Sorry for the inconvenience, we're working "
                    "on resolving this issue." % self.limit
                ])
            result.callback(content)
        else:
            result.callback(None)

### The End ###
