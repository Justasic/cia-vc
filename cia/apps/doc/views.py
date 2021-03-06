from django.shortcuts import render
from django.http import Http404, HttpResponseRedirect
from django.conf import settings
from docutils.core import publish_parts
from django.core.cache import cache
from django.template import loader
import os

def get_sidebar_templates(path):
    """Return a list of sidebar template names to try for the
       given documentation path. A path of 'foo/bar' could
       have any of the following templates:

           doc/foo_bar_sidebar.html
           doc/foo_sidebar.html
           doc/default_sidebar.html

       """
    pattern = "doc/%s_sidebar.html"
    paths = []
    p = path.split('/')
    for i in range(len(p), 0, -1):
        paths.append(pattern % '_'.join(p[:i]))
    paths.append(pattern % "default")
    return paths
    
    direct, basename = os.path.split(path)
    specific = os.path.join(direct, format % basename)
    if os.path.isfile(specific):
        return specific
    else:
        return os.path.join(direct, format % 'default')

def page(request, path):
    key = 'cia.apps.doc.page.%s' % (path.replace('/', '.'))
    ctx = cache.get(key)
    if not ctx:

        filePath = os.path.join(settings.CIA_DOC_PATH, path)

        # "index" is the document representing its parent
        # directory. We canonicalize URLs here such that they
        # never include "index".
        #
        if os.path.basename(filePath) == 'index':
            return HttpResponseRedirect("..")
        if os.path.isdir(filePath):
            filePath = os.path.join(filePath, 'index')
        if not os.path.isfile(filePath):
            raise Http404

        ctx = render(request, 'layout_doc.html', {
            'parts': publish_parts(
                source = open(filePath).read(),
                writer_name = "html4css1",
                settings_overrides = {
                    'cloak_email_addresses': True,
                    'initial_header_level': 2,
                },
            ),
            'sidebar': loader.render_to_string(get_sidebar_templates(path), request=request),
        })
        cache.set(key, ctx)

    return ctx
