""" LibCIA.Stats.Handler

The stats:// URI handler
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

from cia.LibCIA.Stats.Target import StatsTarget
from cia.LibCIA.Ruleset import RegexURIHandler
from cia.LibCIA.Stats.Graph import Relation
from pathlib import PurePath


class StatsURIHandler(RegexURIHandler):
    """Handles stats:// URIs. A stats target is chosen,
       and the message is delivered to it.
       """
    scheme = 'stats'
    regex = r"^stats://(?P<path>([^/]+(/[^/]+)*)?)$"

    def __init__(self):
        self.lastMessage = None
        self.messageTargets = []

    def message(self, uri, message, content):
        """Appends 'content' to the path represented by the given URI
           and delivers a message to its associated stats target.

           This includes a bit of a hack for tracking associations
           between stats targets. We assume that messages are delivered
           one at a time- if we get a duplicate message (presumably to a
           different stats target) we add that stats target to the list of
           targets this message has been delivered to, and reinforce the
           new relations this forms.
           """
        path = PurePath(self.parseURI(uri)['path']).joinpath(content).as_posix()
        target = StatsTarget(path)
        target.deliver(message)

        if message == self.lastMessage:
            for prevTarget in self.messageTargets:
                Relation(prevTarget, target).reinforce()
        else:
            self.messageTargets = []
        self.messageTargets.append(target)
        self.lastMessage = message

### The End ###
