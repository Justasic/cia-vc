from django.conf.urls import url, include
from django.urls import path, re_path
from cia.apps.doc import views

urlpatterns = [
    url(r'^$', views.page, dict(path='')),
    re_path(r'^(?P<path>[a-zA-Z0-9_\-]+(/[a-zA-Z0-9_\-]+)*)/$', views.page),
]
