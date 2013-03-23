from django.shortcuts import render_to_response
from django.template.context import RequestContext

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
		'tabs': tabs2,
		})
	return render_to_response('overview.html', ctx)