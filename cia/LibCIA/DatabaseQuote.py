""" LibCIA.DatabaseQuote

A quote compatibility utility for the now deprecated and removed twisted.enterprise.util.quote
which is used all over the CIA core

Note: This will be going away very soon!
"""
#
# CIA open source notification system
# Copyright (C) 2013 Justin Crawford <justasic@gmail.com>
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

import types

NOQUOTE = 1
USEQUOTE = 2

dbTypeMap = {
    "bigint": NOQUOTE,
    "bool": USEQUOTE,
    "boolean": USEQUOTE,
    "bytea": USEQUOTE,
    "date": USEQUOTE,
    "int2": NOQUOTE,
    "int4": NOQUOTE,
    "int8": NOQUOTE,
    "int": NOQUOTE,
    "integer": NOQUOTE,
    "float4": NOQUOTE,
    "float8": NOQUOTE,
    "numeric": NOQUOTE,
    "real": NOQUOTE,
    "smallint": NOQUOTE,
    "char": USEQUOTE,
    "text": USEQUOTE,
    "time": USEQUOTE,
    "timestamp": USEQUOTE,
    "varchar": USEQUOTE
    }


class DBError(Exception):
    pass

def _safe(text):
    """
    Something really stupid that replaces quotes with escaped quotes.
    """
    return text.replace("'", "''").replace("\\", "\\\\")

def quote(value, typeCode, string_escaper=_safe):
    """Add quotes for text types and no quotes for integer types.
    NOTE: uses Postgresql type codes.
    """
    q = dbTypeMap.get(typeCode, None)
    if q is None:
        raise DBError("Type %s not known" % typeCode)
    if value is None:
        return 'null'
    if q == NOQUOTE:
        return str(value)
    elif q == USEQUOTE:
        if typeCode.startswith('bool'):
            if value:
                value = '1'
            else:
                value = '0'
        if typeCode == "bytea":
            l = ["'"]
            for c in value:
                i = ord(c)
                if i == 0:
                    l.append("\\\\000")
                elif i == 92:
                    l.append(c * 4)
                elif 32 <= i <= 126:
                    l.append(c)
                else:
                    l.append("\\%03o" % i)
            l.append("'")
            return "".join(l)
        if not isinstance(value, bytes) and \
               not isinstance(value, str):
            value = str(value)
        return "'%s'" % string_escaper(value)
