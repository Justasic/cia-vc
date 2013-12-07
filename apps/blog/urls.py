from django.conf.urls import *
from cia.apps.blog import models
from cia.apps.blog.views import BlogFeed, CommentFeed

urlpatterns = patterns('cia.apps.blog.views',
   (r'^(?P<year>\d{4})/(?P<month>\d{2})/(?P<slug>[-\w]+)/$', 'detail'),
   (r'^(?P<year>\d{4})/(?P<month>\d{2})/$',                  'archive'),
   (r'^$',                                                   'archive', {'num_latest': 15}),
   (r'^feed/$',                                              BlogFeed()),
   (r'^comment-feed/$',                                      CommentFeed()),
   (r'^post-comment/$',                                      'post_comment'),
   (r'^delete-comment/(\d+)/(\d+)/$',                        'delete_comment'),
   (r'^delete-comment/(\d+)/$',                              'delete_comment'),
   (r'^new-post/$',                                          'detail'),
)
