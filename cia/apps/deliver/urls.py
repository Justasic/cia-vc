from django.conf.urls import *
from django.conf import settings
import django.contrib.auth.views
from cia.apps.deliver import googlejson, githubjson

urlpatterns = patterns('',
    url(r'^simplejson/$', googlejson.accept),
    url(r'^simplejson$', googlejson.accept),
    url(r'^github/$', githubjson.accept),
)
