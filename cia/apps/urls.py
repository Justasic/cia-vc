
from django.conf.urls import url, include
from django.urls import path, re_path
from django.conf import settings
import os

import cia.apps.feedback
import cia.apps.overview.overview

urlpatterns = [
    re_path(r'^account/', include('cia.apps.accounts.urls')),
    re_path(r'^api/', include('cia.apps.api.urls')),
#    path(r'^admin/', include('django.contrib.admin.urls')),
    re_path(r'^blog/', include('cia.apps.blog.urls')),
    re_path(r'^deliver/', include('cia.apps.deliver.urls')),
    re_path(r'^doc/', include('cia.apps.doc.urls')),
    re_path(r'^images/', include('cia.apps.images.urls')),
    re_path(r'^stats-experimental/', include('cia.apps.stats.urls')),
    re_path(r'^feedback/', cia.apps.feedback.feedback),
    re_path(r'^$', cia.apps.overview.overview.main),
]

# XXX: Experimental
if settings.DEBUG:
    import cia.apps.stats.views as fidviews
    urlpatterns += [
        re_path(r'^testing/fidtool/graph/', fidviews.fidtool_graph),
        re_path(r'^testing/fidtool/counts/', fidviews.fidtool_counts),
        re_path(r'^testing/fidtool/page/', fidviews.fidtool_page),
    ]

if settings.DEBUG:
    from django.views.static import serve
    urlpatterns += [
        re_path(r'^media/(?P<path>.*)$', serve, {
            'document_root': settings.MEDIA_ROOT,
            'show_indexes': True,
        })
    ]

