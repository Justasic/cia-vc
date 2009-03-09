from django.conf.urls.defaults import *
from django.conf import settings
import django.contrib.auth.views
from cia.apps.deliver import googlejson

urlpatterns = patterns('',
    (r'^simplejson/$', googlejson.accept),
    (r'^simplejson$', googlejson.accept),
)
