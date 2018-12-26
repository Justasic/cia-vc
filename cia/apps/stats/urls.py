from django.conf.urls import url, include
from django.urls import path, re_path
from cia.apps.stats import browser

urlpatterns = [
    # Valid stats paths consist of at least one path segment, each of
    # which is not allowed to begin with a dot.
    re_path(r'^(?P<path>[^./][^/]*(/[^./][^/]*)*)/$', browser.stats_page),

    # Special-case for the root stats page
    re_path(r'^$', browser.stats_page, {'path': ''}),
]
