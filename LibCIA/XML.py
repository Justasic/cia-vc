""" LibCIA.XML

Utilities for dealing with objects built on top of XML trees
"""
#
# CIA open source notification system
# Copyright (C) 2003 Micah Dowty <micahjd@users.sourceforge.net>
#
#  This library is free software; you can redistribute it and/or
#  modify it under the terms of the GNU Lesser General Public
#  License as published by the Free Software Foundation; either
#  version 2.1 of the License, or (at your option) any later version.
#
#  This library is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#  Lesser General Public License for more details.
#
#  You should have received a copy of the GNU Lesser General Public
#  License along with this library; if not, write to the Free Software
#  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#

from twisted.xish import domish
import types, os, shutil


class XMLObject(object):
    """An object based on an XML document tree. This class provides
       methods to load it from a string or a DOM element tree, and convert
       it back to an XML string.

       'xml' is either a twisted.xish.domish.Element, a string containing
       the message in XML, or any object with a read() method.
       """
    def __init__(self, xml=None):
        if isinstance(xml, domish.Element):
            self.loadFromElement(xml)
        elif hasattr(xml, 'read'):
            self.loadFromString(xml.read())
        elif xml is not None:
            self.loadFromString(xml)

    def __str__(self):
        return self.xml.toXml()

    def loadFromString(self, string):
        """Parse the given string as XML and set the contents of the message"""
        self.loadFromElement(parseString(string))

    def loadFromElement(self, root):
        """Set the contents of the Message from a parsed document tree given
           as a twisted.xish.domish.Element instance.
           """
        self.xml = root
        self.preprocess()

    def preprocess(self):
        """A hook where subclasses can add code to inspect a freshly
           loaded XML document and/or fill in any missing information.
           """
        pass


class XMLFunction(XMLObject):
    """An XMLObject that is parsed on creation into a function,
       making this class callable. The parser relies on member functions
       starting with 'element_' to recursively parse each element of the XML
       tree, returning a function implementing it.
       """
    requiredRootElement = None

    def preprocess(self):
        """Upon creating this object, parse the XML tree recursively
           into a function that will be invoked by __call__.
           """
        # Validate the root element type if the subclass wants us to.
        # This is hard to do elsewhere, since the element handlers don't
        # know where they are in the XML document.
        if self.requiredRootElement is not None and self.xml.name != self.requiredRootElement:
            raise XMLValidityError("Found a %r element where a root element of %r is required" %
                                   (self.xml.name, self.requiredRootElement))

        self.f = self.parse(self.xml)

    def __call__(self, *args, **kwargs):
        return self.f(*args, **kwargs)

    def parse(self, element):
        """Given an XML element, recursively builds a python function
           implementing the functionality it describes.
           """
        # Pass control on to the appropriate element_* function
        try:
            f = getattr(self, "element_" + element.name)
        except AttributeError:
            return self.unknownElement(element)
        return f(element)

    def unknownElement(self, element):
        """An unknown element was found, by default just generates an exception"""
        raise XMLValidityError("Unknown element name in %s: %r" % (self.__class__.__name__, element.name))


class XMLStorage(object):
    """A container for XMLObjects that supports loading and saving them to disk
       as child nodes of a given root element. This is an abstract implementation
       that doesn't specify how the items are stored in memory.
       """
    def __init__(self, fileName, rootName='storage', heading='', lazyLoad=False):
        self.fileName = fileName
        self.rootName = rootName
        self.heading = heading

        self.loaded = False
        self.emptyStorage()
        if not lazyLoad:
            self.loadIfNecessary()

    def loadIfNecessary(self):
        """If lazyLoad is enabled, the file won't be loaded on initialization, it will
           be loaded the first time this function is called.
           """
        if self.loaded == False and os.path.isfile(self.fileName):
            self.load()

    def load(self):
        """Read items from disk into memory. Called automatically on initialization
           if our file exists.
           """
        self.emptyStorage()
        f = open(self.fileName)
        xml = parseString(f.read())
        f.close()

        for tag in xml.children:
            if isinstance(tag, domish.Element):
                self.store(tag)
        self.loaded = True

    def save(self):
        """Save our in-memory representation back to disk"""
        # To keep the data safe in the event of an error or crash while writing
        # to disk, we write to a .new file, then only if that succeeded we copy
        # the existing file (if there is one) to a '~' backup and move the .new
        # file to its final location.
        f = open(self.fileName + '.new', "w")
        f.write(self.toXml())
        f.close()
        if os.path.isfile(self.fileName):
            shutil.copyfile(self.fileName, self.fileName + '~')
        os.rename(self.fileName + '.new', self.fileName)

    def toXml(self):
        """Return the contents of this XMLStorage as raw XML. If it hasn't
           been loaded yet, this returns the contents of the file on disk
           unmodified. If it has, this uses _serialize()
           """
        if os.path.isfile(self.fileName) and not self.loaded:
            # An optimization for the common case when the file exists
            # but hasn't been loaded yet- return the file verbatim.
            f = open(self.fileName)
            xml = f.read()
            f.close()
            return xml

        else:
            # Nope, we have to serialize our contents
            return self._serialize()

    def _serialize(self):
        """Flatten our storage to a list and output each node as raw XML
           inside our root node. Sorts each object by its attributes,
           so the output might be in a consistent order.
           """
        objects = self.flatten()
        objects.sort(lambda a, b: cmp(a.xml.attributes, b.xml.attributes))
        return '<?xml version="1.0"?>\n%s<%s>\n\n%s\n\n</%s>\n' % (
            self.heading, self.rootName,
            '\n\n'.join([obj.xml.toXml() for obj in objects]),
            self.rootName)

    def emptyStorage(self):
        """Subclasses must implement this to clear whatever storage is being
           used to keep XMLObjects in.
           """
        pass

    def store(self, xml):
        """Subclasses must implement this to add the given object to the storage.
           'xml' will be a domish.Element instance.
           """
        pass

    def flatten(self):
        """Subclasses must implement this to return a flat list of XMLObject
           instances in the storage.
           """
        pass


class XMLValidityError(Exception):
    """This error is raised by subclasses of XMLObject that encounter problems
       in the structure of XML documents presented to them. Normally this should
       correspond with the document not being valid according to its schema,
       but we don't actually use a validating parser.
       """
    pass


class DomishStringParser(domish.SuxElementStream):
    """Because domish doesn't include a parseString()..."""
    def __init__(self):
        domish.SuxElementStream.__init__(self)
        self.DocumentStartEvent = self.docStart
        self.ElementEvent = self.elem
        self.DocumentEndEvent = self.docEnd
        self.done = 0
        self.root = None

    def docStart(self, elem):
        self.root = elem

    def gotText(self, data):
        # This is (another) hack that seems to be necessary
        # to properly parse text included directly into the root node.
        if self.currElem:
            domish.SuxElementStream.gotText(self, data)
        elif self.root:
            self.root.addContent(data)

    def elem(self, elem):
        if self.root:
            self.root.addChild(elem)
        else:
            self.root = elem

    def docEnd(self):
        self.done = 1


def parseString(string):
    """Parse the given string as XML, return a domish.Element"""
    parser = DomishStringParser()
    parser.parse(string)
    return parser.root


def prettyPrint(xml):
    """Given a domish.XML object, return a nice-looking string
       representation.
       """
    # This is gross, but it works...
    from xml.dom import minidom
    s = minidom.parseString(xml.toXml()).toprettyxml()

    # Filter out blank lines
    return "\n".join([line for line in s.split("\n") if line.strip()])


def allText(xml):
    """Concatenate all text under the given domish.Element and return it.
       For a document like:

          <a>boing<b> </b>ha</a>

       The str() operator will return only 'boing' but this function
       will return 'boing ha'.
       """
    if type(xml) in types.StringTypes:
        return xml
    else:
        return "".join([allText(child) for child in xml.children])

### The End ###
