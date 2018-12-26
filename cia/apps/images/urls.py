from django.conf.urls import url, include
from django.urls import path, re_path
from cia.apps.images import views
from django.conf import settings
from django.views.static import serve
import os

urlpatterns = [
    re_path(r'^upload/$', views.upload),
]

if settings.DEBUG:
    urlpatterns += [
        re_path(r'^db/(?P<path>.*)$', serve, {
            'document_root': os.path.join(settings.CIA_DATA_PATH, "db/images"),
            'show_indexes': True,
        })
    ]
