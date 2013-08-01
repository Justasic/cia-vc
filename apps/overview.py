from django.shortcuts import render_to_response
from django.template.context import RequestContext
import os

def _loadSidebar(path):
        """Load sidebar links from a simple text file format.
           Lines beginning with a dash specify a new section heading,
           links are made by lines of the form 'title :: URL'.
           Other lines are ignored.
           """
    tabs = []
	section = {"links": []}
       	for line in open(path).xreadlines():
		line = line.strip()
		if not line:
			continue

		if line[0] == '-':
			tabs.append(section)
			section = {"links": []}
			section["title"] = line[1:].strip()
	                #sections.append(line[1:].strip())
               		continue

        	pieces = line.split("::", 1)
       		if len(pieces) > 1:
               		title, url = pieces
			section["links"].append({"title": title.strip(), "link": url.strip()})
               		#sections[-1].rows.append( tag('a', href=url.strip())[title.strip()] )

	return tabs

def rel_path(p):
    return os.path.join(os.path.abspath(os.path.split(__file__)[0]), p)



def main(request):

	tabs = [
		{
			'title': 'Stats',
			'links': [
				{
					'title': 'Overview',
					'link': '/',
				},
				{
					'title': 'Projects',
					'link': '/stats/project',
				},
				{
					'title': 'Authors',
					'link': '/stats/author',
				},
				{
					'title': 'System',
					'link': '/stats/project',
				},
				{
					'title': 'All Commits',
					'link': '/stats/total/commits',
				},
			]
		},
	]

	tabs2 = [
		{
			'title': 'Stats'
		}

	]

	ctx = RequestContext(request, {
		'announcement_available': False,
		'announcement_text': "Hello!",
		'tabs': _loadSidebar(rel_path("../doc/.default.sidebar")),
		})
	return render_to_response('overview.html', ctx)
