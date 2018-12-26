""" LibCIA

This is a Python package providing modules used to implement the CIA
open source notification system.

CIA provides a way for projects to send messages from their version
control and bug tracking systems to anyone interested- mainly a
network of IRC bots and a web site.

Where does the name CIA come from? It was originally designed to
monitor commits from PicoGUI's Subversion repository, and Lalo came
up with the name CIA: it was a brainless entity designed to keep
an eye on Subversion ;)
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

# Open our git HEAD file and find the revision, copy it, cut it to about 10 chars, append it to the version
import os

def rel_path(p):
    return os.path.join(os.path.abspath(os.path.split(__file__)[0]), p)

f = open(rel_path('../../.git/HEAD'), 'r')
ref = f.read()[5:].strip()
g = open(rel_path('../../.git/'+ref), 'r')
commit = g.read().strip()
branch = ref[11:]
shortcommit = commit[:13]
f.close()
g.close()

__version__ = "1.0-" + branch + "-" + shortcommit


# Check the python version here before we proceed further
requiredPythonVersion = (2,6,0)
import sys, string
if sys.version_info < requiredPythonVersion:
    raise Exception("%s requires at least Python %s, found %s instead." % (
        name,
        string.join(list(map(str, requiredPythonVersion)), "."),
        string.join(list(map(str, sys.version_info)), ".")))
del sys
del string

### The End ###
