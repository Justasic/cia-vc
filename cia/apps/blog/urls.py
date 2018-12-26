from django.conf.urls import url, include
from django.urls import path, re_path
from cia.apps.blog import models
from cia.apps.blog.views import BlogFeed, CommentFeed, detail, archive, post_comment, delete_comment

urlpatterns = [
   re_path(r'^(?P<year>\d{4})/(?P<month>\d{2})/(?P<slug>[-\w]+)/$', detail),
   re_path(r'^(?P<year>\d{4})/(?P<month>\d{2})/$',                  archive),
   re_path(r'^$',                                                   archive, {'num_latest': 15}),
   re_path(r'^feed/$',                                              BlogFeed()),
   re_path(r'^comment-feed/$',                                      CommentFeed()),
   re_path(r'^post-comment/$',                                      post_comment),
   re_path(r'^delete-comment/(\d+)/(\d+)/$',                        delete_comment),
   re_path(r'^delete-comment/(\d+)/$',                              delete_comment),
   re_path(r'^new-post/$',                                          detail),
]
