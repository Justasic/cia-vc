from django.conf.urls import url, include
from django.urls import path, re_path
from django.conf import settings
import django.contrib.auth.views
from cia.apps.deliver import googlejson, githubjson

urlpatterns = [
    re_path(r'^simplejson/$', googlejson.accept),
    re_path(r'^simplejson$', googlejson.accept),
    re_path(r'^github/$', githubjson.accept),
]
