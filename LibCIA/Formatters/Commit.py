""" LibCIA.Formatters.Commit

Formatters used for converting commit messages to other formats.
Note that this only handles real XML commit messages. The legacy
'colorText' messages are handled by a separate module.
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

from LibCIA import Message, XML
from Nouvelle import tag
import re, posixpath
from twisted.python.util import OrderedDict
import Util

__all__ = ['CommitToIRC', 'CommitToPlaintext', 'CommitToXHTML',
           'CommitTitle', 'CommitToXHTMLLong']


class CommitFormatter(Message.ModularFormatter):
    """Base class for formatters that operate on commit messages.
       Includes a filter for commit messages, and utilities for
       extracting useful information from the commits.
       """
    filter = '<find path="/message/body/commit"/>'
    defaultComponentTree = """
    <format>
        <author/> <branch/> *
        <version/><autoHide>r<revision/></autoHide>
        <module/>/<files/>:
        <log/>
    </format>
    """

    # Subclasses can set this to limit the length of log messages, in lines
    lineLimit = None

    # Lines in the log longer than this are wrapped to wrapWidth
    widthLimit = None
    wrapWidth = None

    # If the list of files ends up longer than this many characters, summarize it
    filesWidthLimit = 60

    def param_lineLimit(self, tag):
        self.lineLimit = int(XML.shallowText(tag))

    def param_widthLimit(self, tag):
        self.widthLimit = int(XML.shallowText(tag))
        if self.wrapWidth > self.widthLimit:
            self.wrapWidth = self.widthLimit

    def param_wrapWidth(self, tag):
        self.wrapWidth = int(XML.shallowText(tag))

    def param_filesWidthLimit(self, tag):
        self.filesWidthLimit = int(XML.shallowText(tag))

    def component_author(self, element, args):
        return self.textComponent(element, args, "message", "body", "commit", "author")

    def component_version(self, element, args):
        return self.textComponent(element, args, "message", "body", "commit", "version")

    def component_revision(self, element, args):
        return self.textComponent(element, args, "message", "body", "commit", "revision")

    def component_branch(self, element, args):
        return self.textComponent(element, args, "message", "source", "branch")

    def component_module(self, element, args):
        return self.textComponent(element, args, "message", "source", "module")

    def component_project(self, element, args):
        return self.textComponent(element, args, "message", "source", "project")

    def component_files(self, element, args):
        """Break up our list of files into a common prefix and a sensibly-sized
           list of filenames after that prefix.
           """
        files = XML.dig(args.message.xml, "message", "body", "commit", "files")
        if not (files and XML.hasChildElements(files)):
            return []

        prefix, endings = self.consolidateFiles(files)
        endingStr = " ".join(endings)
        if len(endingStr) > self.filesWidthLimit:
            # If the full file list is too long, give a file summary instead
            endingStr = self.summarizeFiles(endings)
        if prefix.startswith('/'):
            prefix = prefix[1:]

        if endingStr:
            return ["%s (%s)" % (prefix, endingStr)]
        else:
            return [prefix]

    def component_log(self, element, args):
        log = XML.dig(args.message.xml, "message", "body", "commit", "log")
        if not log:
            return []

        # Break the log string into wrapped lines
        lines = []
        for line in Util.getNormalizedLog(log):
            # Ignore blank lines
            if not line:
                continue

            # Wrap long lines
            if self.widthLimit and len(line) > self.widthLimit:
                lines.extend(Util.wrapLine(line, self.wrapWidth))
            else:
                lines.append(line)

        # If our lineLimit is 1, don't bother starting long logs on the
        # next line since there will be no long logs. Instead of the
        # long (log message trimmed), just add an ellipsis.
        if self.lineLimit == 1:
            if len(lines) > 1:
                lines[0] += ' ...'
                del lines[1:]

        # Multiline logs shouldn't start on the same line as the metadata
        elif len(lines) > 1:
            lines.insert(0, '')

            # Truncate long log messages if we have a limit
            if self.lineLimit and len(lines) > self.lineLimit + 1:
                lines[0] = "(log message trimmed)"
                del lines[self.lineLimit + 1:]

        # Reassemble the log message and send it to the default formatter
        return ["\n".join(lines)]

    def summarizeFiles(self, files):
        """Given a list of strings representing file paths, return
           a summary of those files and/or directories. This is used
           in place of a full file list when that would be too long.
           """
        # Count the number of distinct directories we have
        dirs = {}
        for file in files:
            dirs[posixpath.split(file)[0]] = True

        if len(dirs) <= 1:
            return "%d files" % len(files)
        else:
            return "%d files in %d dirs" % (len(files), len(dirs))

    def consolidateFiles(self, xmlFiles):
        """Given a <files> element, find the directory common to all files
           and return a 2-tuple with that directory followed by
           a list of files within that directory.
           """
        files = []
        if xmlFiles:
            for fileTag in XML.getChildElements(xmlFiles):
                if fileTag.nodeName == 'file':
                    files.append(XML.shallowText(fileTag))

        # If we only have one file, return it as the prefix.
        # This prevents the below regex from deleting the filename
        # itself, assuming it was a partial filename.
        if len(files) == 1:
            return files[0], []

        # Start with the prefix found by commonprefix,
        # then actually make it end with a directory rather than
        # possibly ending with part of a filename.
        prefix = re.sub("[^/]*$", "", posixpath.commonprefix(files))

        endings = []
        for file in files:
            endings.append(file[len(prefix):])
        return prefix, endings


class CommitToIRC(CommitFormatter):
    """Converts commit messages to plain text with IRC color tags.
       This adds colorText elements to the component vocabulary defined by CommitFormatter.
       """
    medium = 'irc'
    lineLimit = 6
    widthLimit = 220
    wrapWidth = 80

    defaultComponentTree = """
    <format>
        <autoHide><color fg='green'><author/></color></autoHide>
        <autoHide><color fg='orange'><branch/></color></autoHide>
        *
        <autoHide><b><version/></b></autoHide>
        <autoHide>r<b><revision/></b></autoHide>
        <color fg='aqua'><module/></color>/<files/><b>:</b>
        <log/>
    </format>
    """

    def __init__(self):
        # Blah, we have to do this to avoid circular dependency
        from LibCIA.IRC import Formatting
        self.Formatting = Formatting

    def format(self, args):
        self.colorStack = self.Formatting.ColorStack()
        return CommitFormatter.format(self, args)

    def param_noColor(self, tag):
        """The <noColor> parameter disables colors.
           This is equivalent to a <format> parameter with CommitFormatter's
           default component tree.
           """
        self.componentTree = XML.parseString(CommitFormatter.defaultComponentTree
                                             ).documentElement

    def component_b(self, element, args):
        self.colorStack.push("bold")
        return self.colorStack.wrap(self.walkComponents(element.childNodes, args))

    def component_u(self, element, args):
        self.colorStack.push("underline")
        return self.colorStack.wrap(self.walkComponents(element.childNodes, args))

    def component_color(self, element, args):
        self.colorStack.push(*self.Formatting.parseColorElement(element))
        return self.colorStack.wrap(self.walkComponents(element.childNodes, args))


class CommitToPlaintext(CommitFormatter):
    """Converts commit messages to plain text. Currently this is the same as
       the default commit formatting.
       """
    medium = 'plaintext'


class CommitTitle(CommitFormatter):
    """Extracts a title from commit messages"""
    medium = 'title'

    def format(self, args):
        log = XML.dig(args.message.xml, "message", "body", "commit", "log")
        if log:
            return Util.extractSummary(log)


class CommitToXHTML(CommitFormatter):
    """Converts commit messages to XHTML, represented as a Nouvelle tag tree."""
    medium = 'xhtml'
    defaultComponentTree = """
    <format>
        <n:div style='border: 1px solid #888; background-color: #DDD; padding: 0.25em 0.5em; margin: 0em;'>
            <autoHide> Commit by <n:strong><author/></n:strong></autoHide>
            <autoHide> on <branch/></autoHide>
            <n:span style='color: #888;'> :: </n:span>
            <autoHide><n:b><version/></n:b></autoHide>
            <autoHide>r<n:b><revision/></n:b></autoHide>
            <n:b><module/></n:b>/<files/>:
        </n:div>
        <n:p style='padding: 0em; margin: 0.5em 0em;'>
            <log/>
        </n:p>
    </format>
    """

    def __init__(self):
        from LibCIA.Web import RegexTransform
        self.hyperlinker = RegexTransform.AutoHyperlink()

    def joinComponents(self, results):
        """Nouvelle is just fine dealing with lists, don't join anything"""
        return results

    def walkComponents(self, nodes, args):
        """Instead of concatenating lists, this implementation of walkComponents
           nests them. This is more efficient with nouvelle, and lets us detect
           empty results for <autoHide>.
           """
        results = []
        for node in nodes:
            results.append(self.evalComponent(node, args))
        return results

    def component_autoHide(self, element, args):
        """The standard autoHide component is rewritten to properly recurse
           into the contents of Nouvelle tags.
           """
        results = self.walkComponents(element.childNodes, args)
        if self._checkVisibility(results):
            return results
        else:
            return []

    def _checkVisibility(self, nodes):
        """Recursively check visibility for autoHide. Empty lists cause
           us to return 0, and Nouvelle tags are recursed into.
           """
        for node in nodes:
            if not node:
                return 0
            if isinstance(node[0], tag):
                if not self._checkVisibility(node[0].content):
                    return 0
        return 1

    def evalComponent(self, node, args):
        """Here we convert all components starting with 'n:' into Novuelle tags.
           FIXME: This should really be using proper DOM namespace manipulation and such
           """
        if node.nodeType == node.ELEMENT_NODE and node.nodeName.startswith("n:"):
            attrs = {}
            for attr in node.attributes.itervalues():
                attrs[str(attr.name)] = attr.value
            return [tag(node.nodeName[2:], **attrs)[ self.walkComponents(node.childNodes, args) ]]
        return CommitFormatter.evalComponent(self, node, args)

    def component_log(self, element, args):
        """Convert the log message to HTML. If the message seems to be preformatted
           (it has some lines with indentation) it is stuck into a <pre>. Otherwise
           it is converted to HTML by replacing newlines with <br> tags and converting
           bulletted lists.
           """
        log = XML.dig(args.message.xml, "message", "body", "commit", "log")
        if not log:
            return []
        content = []
        lines = Util.getNormalizedLog(log)
        nonListItemLines = []
        listItems = []

        if lines:
            # Scan the message, applying a few heuristics. If we see lines
            # that are still starting with a space after getNormalizedLog
            # has done its thing, assume the text is preformatted. Also
            # look for lines that appear to be list items.
            isPreFormatted = False
            for line in lines:
                if line and line[0] == ' ':
                    isPreFormatted = True

                if line.startswith("* ") or line.startswith("- "):
                    # Assume this is a list item, and convert the bullets to
                    # a proper XHTML list.
                    listItems.append(line[2:])
                else:
                    if listItems:
                        # It's a continuation of the last item
                        listItems[-1] = listItems[-1] + " " + line.strip()
                    else:
                        # If we haven't seen a list yet, stick this in nonListItemLines.
                        # If this log message isn't a list at all, everything will end
                        # up there but it will be safely ignored
                        nonListItemLines.append(line)

            if listItems:
                # It looks like a bulleted list. First output the nonListItemLines,
                # then stick the items inside a list.
                for line in nonListItemLines:
                    if content:
                        content.append(tag('br'))
                    content.append(line)
                content.append(tag('ul')[[ tag('li')[item] for item in listItems ]])

            elif isPreFormatted:
                # This is probably a preformatted message, stick it in a <pre>
                content.append(tag('pre')[ "\n".join(lines) ])

            else:
                # Plain old text, just stick <br>s between the lines
                for line in lines:
                    if content:
                        content.append(tag('br'))
                    content.append(line)
        else:
            content.append(tag('i')["No log message"])

        return self.hyperlinker.apply(content)


class CommitToXHTMLLong(CommitToXHTML):
    """Builds on the xhtml formatter to generate a longer representation of the commit,
       suitable for a full page rather than just an item in a listing.
       """
    medium = 'xhtml-long'
    defaultComponentTree = """
    <format>
        <n:h1>Commit Message</n:h1>
        <headers/>
        <n:p class='messageBody'><log/></n:p>
        <autoHide><n:h1>Modified Files</n:h1><files/></autoHide>
    </format>
    """

    def component_headers(self, element, args):
        """Format all relevant commit metadata in an email-style header box"""
        from LibCIA.Web import Template

        message   = args.message
        commit    = XML.dig(message.xml, "message", "body", "commit")
        source    = XML.dig(message.xml, "message", "source")
        author    = XML.dig(commit, "author")
        version   = XML.dig(commit, "version")
        revision  = XML.dig(commit, "revision")
        diffLines = XML.dig(commit, "diffLines")
        url       = XML.dig(commit, "url")
        log       = XML.dig(commit, "log")
        project   = XML.dig(source, "project")
        module    = XML.dig(source, "module")
        branch    = XML.dig(source, "branch")
        headers   = OrderedDict()

        if author:
            headers['Author'] = XML.shallowText(author)
        if project:
            headers['Project'] = XML.shallowText(project)
        if module:
            headers['Module'] = XML.shallowText(module)
        if branch:
            headers['Branch'] = XML.shallowText(branch)
        if version:
            headers['Version'] = XML.shallowText(version)
        if revision:
            headers['Revision'] = XML.shallowText(revision)
        if diffLines:
            headers['Changed Lines'] = XML.shallowText(diffLines)
        if url:
            headers['URL'] = tag('a', href=XML.shallowText(url))[ Util.extractSummary(url) ]

        return [Template.MessageHeaders(headers)]

    def component_files(self, element, args):
        """Format the contents of our <files> tag as a tree with nested lists"""
        from LibCIA.Web import Template

        files = XML.dig(args.message.xml, "message", "body", "commit", "files")
        if not (files and XML.hasChildElements(files)):
            return []

        # First we organize the files into a tree of nested dictionaries.
        # The dictionary we ultimately have FileTree render maps each node
        # (file or directory) to a dictionary of its contents. The keys
        # in these dictionaries can be any Nouvelle-renderable object
        # produced by format_file.
        #
        # As a first step, we build a dictionary mapping path segment to
        # [fileTag, children] lists. We then create a visual representation
        # of each fileTag and generate the final dictionary.
        fileTree = {}
        if xmlFiles:
            for fileTag in XML.getChildElements(xmlFiles):
                if fileTag.nodeName == 'file':
                    # Separate the file into path segments and walk into our tree
                    node = [None, fileTree]
                    for segment in XML.shallowText(fileTag).split('/'):
                        node = node[1].setdefault(segment, [None, {}])
                    # The leaf node owns this fileTag
                    node[0] = fileTag

        return [Template.FileTree(self.format_file_tree(fileTree))]

    def format_file_tree(self, fileTree):
        """This is the second half of format_files- it recursively
           converts a tree of [fileTag,children] style dictionaries
           into a tree of Template.FileTree() compatible dicts,
           using format_file() to render each fileTag.
           """
        result = {}
        for name, t in fileTree.iteritems():
            fileTag, children = t
            result[self.format_file(name, fileTag)] = self.format_file_tree(children)
        return result

    def format_file(self, name, fileTag=None):
        """Given the short name of a file, and optionally its XML tag,
           return a Nouvelle-serializable representation.
           """
        if fileTag:

            # If we have a 'uri' attribute, make this file a hyperlink
            uri = fileTag.getAttributeNS(None, 'uri')
            if uri:
                return tag('a', href=uri)[ name ]

        return name

### The End ###
