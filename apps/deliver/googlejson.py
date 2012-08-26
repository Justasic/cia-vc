from django import http
from django.utils import simplejson, html

import xmlrpclib

SERVER_URL = "http://cia.vc"

PROJECT_TRANS = {
    'cia-vc': 'CIA.vc'
}

GET_RESPONSE = """
    <html><body>
    <p>
    This is the URL where you can send post-commit JSON notifications to,
    in the format
    <a href="http://code.google.com/p/support/wiki/PostCommitWebHooks">
    Google came up with</a>
    </p>

    <p>
    Set this URL in the Administer tab, in Source,
    Post-Commit Web Hooks, Post-Commit URL,
    and CIA will get a notification whenever a commit happens.
    Don't forget to turn off repository polling in CIA's project settings
    after you did that.
    </p>
    </body></html>
    """

XML_TEMPLATE = """<?xml version="1.0" encoding="UTF-8"?>
    <message>
      <generator>
        <name>Simple JSON POST parser for google code</name>
        <version>0.5</version>
      </generator>
      <source>
        <project>%(project)s</project>
      </source>
      <timestamp>%(timestamp)s</timestamp>
      <body>
        <commit>
          <revision>%(revision)s</revision>
          <author>%(author)s</author>
          <url>%(rev_url)s</url>
          <log>%(log)s</log>
          <files>
            %(files)s
          </files>
        </commit>
      </body>
    </message>
    """

def accept(request):
    if request.method == 'GET':
        return http.HttpResponse(GET_RESPONSE)
    elif request.method != 'POST':
        return http.HttpResponseNotAllowed(["GET", "POST"])

    # Okay, we have a POST.
    server = xmlrpclib.ServerProxy(SERVER_URL)
    body = request.raw_post_data
    data = simplejson.loads(body)
    info = {}

    project = data['project_name']
    if project in PROJECT_TRANS:
        project = PROJECT_TRANS[project]
    info['project'] = html.escape(project)
    info['repo_url'] = html.escape(data['repository_path'])
    for revision in data['revisions']:
        info['revision'] = html.escape(revision['revision'])
        info['rev_url'] = html.escape(revision['url'])
        info['author'] = html.escape(revision['author'])
        info['timestamp'] = html.escape(revision['timestamp'])
        info['log'] = html.escape(revision['message'])
        files = []
        if 'added' in revision:
            for entry in revision['added']:
                files.append('<file action="add">' +
                             html.escape(entry) +
                             '</file>')
        if 'modified' in revision:
            for entry in revision['modified']:
                files.append('<file action="modify">' +
                             html.escape(entry) +
                             '</file>')
        if 'removed' in revision:
            for entry in revision['removed']:
                files.append('<file action="remove">' +
                             html.escape(entry) +
                             '</file>')
        info['files'] = '\n            '.join(files)
        xml = XML_TEMPLATE % info
        server.hub.deliver(xml)

    return http.HttpResponse('Message accepted in queue.')
