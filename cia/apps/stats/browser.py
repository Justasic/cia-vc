from django.shortcuts import render
from cia.LibCIA import XML
from cia.apps.stats.models import StatsTarget
import datetime

# Legacy imports
from cia.LibCIA.Stats.Target import StatsTarget as OldStatsTarget
from cia.LibCIA.Message import FormatterArgs
from cia.LibCIA import Formatters
from cia.LibCIA.Message import Message as OldMessage
from cia.LibCIA.Formatters.Commit import CommitFormatter
from Nouvelle.Serial import Serializer

class Message:
    serializer = Serializer()
    formatter_factory = Formatters.getFactory()

    def __init__(self, xxx_todo_changeme):
        (id, xml) = xxx_todo_changeme
        self.id = id
        self.hex_id = "%x" % id
        self.oldmsg = OldMessage(xml)

        self.timestamp = datetime.datetime.fromtimestamp(
            XML.digValue(self.oldmsg.xml, float, "message", "timestamp"))

        self.formatter = self.formatter_factory.findMedium('xhtml', self.oldmsg)
        self.is_commit = isinstance(self.formatter, CommitFormatter)
        if self.is_commit:
            for shortcut, path in list(XML.pathShortcuts.items()):
                doc = XML.XPath(path).queryObject(self.oldmsg)
                if doc:
                    setattr(self, shortcut, XML.shallowText(doc[0]))

    def get_year_and_month(self):
        """The template needs this for use with {% ifchanged %}"""
        return (self.timestamp.year, self.timestamp.month)

    def to_html(self):
        """Format any message as XHTML, using the LibCIA formatters and
           Nouvelle serializer. This is used as a fallback for non-commit
           messages.
           """
        return self.serializer.render(self.formatter.formatMessage(self.oldmsg))

    def format_log(self):
        return self.serializer.render(self.formatter.component_log(None, FormatterArgs(self.oldmsg)))

    def format_files(self):
        return self.serializer.render(self.formatter.component_files(None, FormatterArgs(self.oldmsg)))


def stats_page(request, path):
    oldtarget = OldStatsTarget(path)
    try:
        target = StatsTarget.objects.get(path=path)
    except StatsTarget.DoesNotExist:
        # If we don't have a stats target in the db, create a temp
        target = StatsTarget(path=path)
    target.enforce_defaults()

    messages = list(map(Message, oldtarget.messages.getLatest(20)))
    messages.reverse()

    return render(request, 'stats/stats_page.html', {
        'path': path,
        'target': target,
        'recent_messages': messages,
        })
