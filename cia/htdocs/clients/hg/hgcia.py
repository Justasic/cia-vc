# Copyright (C) 2007 Brendan Cully <brendan@kublai.com>
# Published under the GNU GPL

'''CIA notification

This is mean to be run as a changegroup or incoming hook.
To configure it, set the following options in your hgrc:

[cia]
# your registered CIA user name
user = foo
# the name of the project in CIA
project = foo
# the module (subproject) (optional)
#module = foo
# Append a diffstat to the log message (optional)
#diffstat = False
# The URL of the CIA notification service (optional)
#url = http://cia.vc/
# print message instead of sending it (optional)
#test = False

[hooks]
# one of these:
changegroup.cia = python:hgcia.hook
#incoming.cia = python:hgcia.hook

[web]
# If you want hyperlinks (optional)
baseurl = http://server/path/to/repo
'''

from mercurial.i18n import *
from mercurial.node import *
from mercurial import version, patch

import xmlrpclib
from xml.sax import saxutils

HGCIA_VERSION = '0.1'
HGCIA_URL = 'http://hg.kublai.com/mercurial/hgcia'

class ciamsg(object):
    def __init__(self, cia, ctx):
        self.cia = cia
        self.ctx = ctx
        self.url = self.cia.url

    def fileelem(self, path):
        if self.url:
            n = short(self.ctx.node())
            uri = '%s/file/%s/%s' % (self.url, n, path)
            uri = 'uri=%s' % saxutils.quoteattr(uri)
        else:
            uri = ''

        return '<file %s>%s</file>' % (uri, saxutils.escape(path))

    def sourceelem(self, project, module=None, branch=None):
        msg = ['<source>', '<project>%s</project>' % saxutils.escape(project)]
        if module:
            msg.append('<module>%s</module>' % saxutils.escape(module))
        if branch:
            msg.append('<branch>%s</branch>' % saxutils.escape(branch))
        msg.append('</source>')

        return '\n'.join(msg)

    def diffstat(self):
        class patchbuf:
            def __init__(self):
                self.lines = []
                # diffstat is stupid
                self.name = 'cia'
            def write(self, data):
                self.lines.append(data)
            def close(self):
                pass

        n = self.ctx.node()
        pbuf = patchbuf()
        patch.export(self.cia.repo, [n], fp=pbuf)
        return patch.diffstat(pbuf.lines) or ''

    def xml(self):
        n = short(self.ctx.node())
        src = self.sourceelem(self.cia.project, module=self.cia.module,
                              branch=self.ctx.branch())
        # unix timestamp
        dt = self.ctx.date()
        timestamp = dt[0]

        author = saxutils.escape(self.ctx.user())
        rev = '%d:%s' % (self.ctx.rev(), n)

        log = self.ctx.description()
        if self.cia.diffstat:
            log = '%s\n-- \n%s' % (log, self.diffstat())
        log = saxutils.escape(log)

        url = self.url and '<url>%s/rev/%s</url>' % (saxutils.escape(self.url),
                                                     n) or ''
        files = '\n'.join([self.fileelem(f) for f in self.ctx.files()])

        msg = '''
<message>
  <generator>
    <name>Mercurial</name>
    <version>%s</version>
    <url>%s</url>
  </generator>
  %s
  <body>
    <commit>
      <author>%s</author>
      <version>%s</version>
      <log>%s</log>
      %s
      <files>%s</files>
    </commit>
  </body>
  <timestamp>%d</timestamp>
</message>
        ''' % (version.get_version(), saxutils.escape(HGCIA_URL), src,
               author, rev, log, url, files, timestamp)

        return msg

class hgcia(object):
    '''CIA notification class'''
    def __init__(self, ui, repo):
        self.ui = ui
        self.repo = repo

        self.ciaurl = self.ui.config('cia', 'url', 'http://cia.vc')
        self.user = self.ui.config('cia', 'user')
        self.project = self.ui.config('cia', 'project')
        self.module = self.ui.config('cia', 'module')
        self.diffstat = self.ui.configbool('cia', 'diffstat')
        self.dryrun = self.ui.configbool('cia', 'test')
        self.url = self.ui.config('web', 'baseurl')

    def sendrpc(self, msg):
        srv = xmlrpclib.Server(self.ciaurl)
        srv.hub.deliver(msg)

def hook(ui, repo, hooktype, node=None, url=None, **kwargs):
    '''send CIA notification'''
    def sendmsg(cia, ctx):
        msg = ciamsg(cia, ctx).xml()
        if cia.dryrun:
            ui.write(msg)
        else:
            cia.sendrpc(msg)

    n = bin(node)
    cia = hgcia(ui, repo)
    if not cia.user:
        ui.debug(_('cia: no user specified'))
        return
    if not cia.project:
        ui.debug(_('cia: no project specified'))
        return
    if hooktype == 'changegroup':
        start = repo.changelog.rev(n)
        end = repo.changelog.count()
        for rev in xrange(start, end):
            n = repo.changelog.node(rev)
            ctx = repo.changectx(n)
            sendmsg(cia, ctx)
    else:
        ctx = repo.changectx(n)
        sendmsg(cia, ctx)
