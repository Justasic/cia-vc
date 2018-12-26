""" LibCIA.ColorText

Parses the {color} tag format used by original CIA commit messages,
generating a <colorText> element tree.

The <colorText> format is much like HTML. Any node can contain text and/or any of the
nodes defined here. Tags and their effects can be nested like in HTML. The tags are:

  <b>               Bold, just like in HTML
  <u>               Underline, just like HTML
  <br/>             Line break, just like HTML
  <color fg="foo">  Set the foreground color
  <color bg="foo">  Set the background color

Color names are the same as those allowed in the original {color} format, a complete
list is in allowedColors.

This code gets awfully fun, since the original format allowed goop like
{blue}{reverse}{yellow}Hi{red}{bold}There{normal}
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

import copy

from cia.LibCIA import XML


allowedColors = (
    "black",
    "dark blue",
    "dark green",
    "green",
    "red",
    "brown",
    "purple",
    "orange",
    "yellow",
    "light green",
    "aqua",
    "light blue",
    "blue",
    "violet",
    "grey",
    "gray",
    "light grey",
    "light gray",
    "white",
    )


class ColorState:
    """Represents the current foreground color, background color, bold, and underline
       state. State changes are generated by tags in the original format, which are
       used to generate tags in the <colorText> format.
       """
    fgColor = None
    bgColor = None
    bold = False
    underline = False

    def reverseVideo(self):
        """Swap foreground and background colors"""
        self.fgColor, self.bgColor = self.bgColor, self.fgColor


class ColorTextParser:
    """Parses a commit in the old format (with {color} tags) and generates a <colorText>
       element containing the message's contents.

       Common usage:

         >>> p = ColorTextParser()

         >>> p.parseToString('hello')
         '<colorText>hello</colorText>'

         >>> p.parseToString('{bold}hello')
         '<colorText><b>hello</b></colorText>'

         >>> p.parseToString('{bold}hello{normal} world')
         '<colorText><b>hello</b> world</colorText>'

       Some pathological examples:

         >>> p.parseToString('{bold}{dark blue}{reverse}{yellow}hello{normal}{underline} world')
         '<colorText><color bg="dark blue"><color fg="yellow"><b>hello</b></color></color><u> world</u></colorText>'

         >>> p.parseToString('{wiggle}')
         '<colorText>{wiggle}</colorText>'

         >>> p.parseToString('{blue}')
         '<colorText/>'

         >>> p.parseToString('<b>')
         '<colorText>&lt;b&gt;</colorText>'

         >>> p.parseToString('{blue}{normal}hello')
         '<colorText>hello</colorText>'
       """
    def parse(self, message):
        """Given a string of text in the original CIA commit format, return a <colorText>
           element representing it as a DOM tree.
           """
        # Initialize our model of the current text format in the original message
        self.parsedState = ColorState()

        self.document = XML.createRootNode()

        # Initialize our stack of (element, ColorState) tuples representing
        # the state of the XML document being generated. This starts out with
        # our root element in it.
        self.elementStack = [
            (XML.addElement(self.document, "colorText"), ColorState())
            ]

        # Break up the message into lines, each with its whitespace stripped.
        # Run our lexical scanner on each line separately, turning it into
        # a stream of events. Insert <br/> tags between lines.
        lines = []
        for line in message.split("\n"):
            # Ignore extra whitespace
            line = line.strip()
            # Ignore blank lines
            if line:
                lines.append(line)
        for i in range(len(lines)):
            if i != 0:
                XML.addElement(self.elementStack[-1][0], 'br')
            self.lex(lines[i])
            self.closeTags()

        return self.document

    def parseToString(self, message):
        return XML.toString(self.parse(message).documentElement)

    def lex(self, message):
        """A simple lexical scanner to convert an incoming message into a stream of text and tag events"""
        while message:
            nextSquiggly = message.find("{")
            if nextSquiggly != 0:
                # Normal text

                if nextSquiggly > 0:
                    # Process normal text and take it out of the message buffer
                    self.textEvent(message[:nextSquiggly])
                    message = message[nextSquiggly:]
                else:
                    # This is the last event in the message
                    self.textEvent(message)
                    return
            else:
                # Possibly the beginning of a tag

                tagEnd = message.find("}")
                if tagEnd < 1:
                    # Unclosed tag, the rest of the message as normal text
                    self.textEvent(message)
                    return

                # Chomp up the tag
                self.tagEvent(message[1:tagEnd])
                message = message[tagEnd+1:]

    def tagEvent(self, tag):
        """We just received a color tag. If this is a recognized tag, use it
           to mutate the current parsedState. If not, put the squiggly brackets
           back on and treat it as plain text.
           """
        if tag == "bold":
            self.parsedState.bold = not self.parsedState.bold
        elif tag == "underline":
            self.parsedState.underline = not self.parsedState.underline
        elif tag == "reverse":
            self.parsedState.reverseVideo()
        elif tag == "normal":
            self.parsedState = ColorState()
        elif tag in allowedColors:
            self.parsedState.fgColor = tag
        else:
            # Unrecognized tag- output it unmodified
            self.textEvent("{" + tag + "}")

    def textEvent(self, text):
        """We just received some text. Make sure our parsedState matches the
           state of the current XML node and dump the new text in there.
           """
        self.updateState()
        node = self.elementStack[-1][0]
        node.appendChild(node.ownerDocument.createTextNode(text))

    def closeTags(self):
        """Close all currently opened tags"""
        self.elementStack = self.elementStack[:1]

    def pushTag(self, name, attributes={}, stateChanges={}):
        """Add a new element to the elementStack, placed at
           the end of the children list for the tag currently
           at the top of the stack.

           name:         The name of the new element
           attributes:   A dict of attributes to set on the new element
           stateChanges: A dict of attributes to change in the new tag's state
           """
        oldTag, oldState = self.elementStack[-1]

        newTag = XML.addElement(self.elementStack[-1][0], name)
        for key, value in attributes.items():
            newTag.setAttributeNS(None, key, value)

        newState = copy.deepcopy(oldState)
        newState.__dict__.update(stateChanges)

        self.elementStack.append((newTag, newState))

    def updateState(self):
        """Compare the current parsedState with the ColorState() associated
           with the XML element topmost on the stack. If they don't match,
           add and remove elements as necessary to fix this.
           """
        if self.elementStack[-1][1] == self.parsedState:
            # The states match, nothing to do
            return

        # For now this is pretty unintelligent- just close all tags
        # and reopen them as necessary to recreate the new state.
        self.closeTags()

        if self.parsedState.bgColor:
            self.pushTag('color',
                         {'bg': self.parsedState.bgColor},
                         {'bgColor': self.parsedState.bgColor}
                         )
        if self.parsedState.fgColor:
            self.pushTag('color',
                         {'fg': self.parsedState.fgColor},
                         {'fgColor': self.parsedState.fgColor}
                         )
        if self.parsedState.bold:
            self.pushTag('b',
                         {},
                         {'bold': True},
                         )
        if self.parsedState.underline:
            self.pushTag('u',
                         {},
                         {'underline': True},
                         )

### The End ###
