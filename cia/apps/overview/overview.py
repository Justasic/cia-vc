from django.shortcuts import render
from cia.apps.overview.models import GetStats
from datetime import datetime
import os


def _loadSidebar(path):
    """Load sidebar links from a simple text file format.
       Lines beginning with a dash specify a new section heading,
       links are made by lines of the form 'title :: URL'.
       Other lines are ignored.
       """
    tabs = []
    section = {"links": []}
    for line in open(path):
        line = line.strip()
        if not line:
            continue

        if line[0] == '-':
            if section["links"]:
                tabs.append(section)
            section = {"links": []}
            section["title"] = line[1:].strip()
            # sections.append(line[1:].strip())
            continue

        pieces = line.split("::", 1)
        if len(pieces) > 1:
            title, url = pieces
            section["links"].append(
                {"title": title.strip(), "link": url.strip()})

    tabs.append(section)
    return tabs


def rel_path(p):
    return os.path.join(os.path.abspath(os.path.split(__file__)[0]), p)


def GetNewestStat(count, stat_type):
    stats = GetStats("first_time", "forever", stat_type, "DESC", count)

    for stat in stats:
        stat['first_time'] = datetime.fromtimestamp(int(stat['first_time']))
        if stat['title'] is None:
            stat['title'] = stat['target_path'].split('/')[1]

    return stats


def GetActiveStat(count, stat_type):
    stats = GetStats("event_count", "today", stat_type, "DESC", count)

    for stat in stats:
        stat['bargraph'] = "0em %fem" % math.log(stat['event_count'])

        if stat['title'] is None:
            stat['title'] = stat['target_path'].split('/')[1]

    return stats


def main(request):
    return render(request, 'overview.html', {
        'announcement_available': False,
        'announcement_text': "Hello!",
        'tabs': _loadSidebar(rel_path("../../doc/.default.sidebar")),
        'active_authors': GetActiveStat(15, "author"),
        'active_projects': GetActiveStat(15, "project"),
        'new_authors': GetNewestStat(15, "author"),
        'new_projects': GetNewestStat(15, "project"),
    })
