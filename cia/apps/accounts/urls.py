from django.conf.urls import url, include
from django.urls import path, re_path
from django.conf import settings
from django.contrib.auth import views as auth_views
from django.contrib import auth
from cia.apps.accounts import authplus, bots, assets

account_page = {
    'next': '/account/',
}

login_page = {
    'next': settings.LOGIN_URL,
    'template_name': 'accounts/login.html'
}

lost_pages = {
    'next': settings.LOGIN_URL,
    'recovery_page': '/account/reset/%s/',
}

urlpatterns = [
    re_path(r'^login/$', auth_views.LoginView.as_view(template_name='accounts/login.html'), name='login'),
    re_path(r'^logout/$', auth_views.LogoutView.as_view(), name='logout'),
    re_path(r'^register/$', authplus.register, account_page, name='register'),
    re_path(r'^lost/$', authplus.lost, lost_pages, name='lost'),
    re_path(r'^reset/(?P<key>\w+)/$', authplus.reset, account_page, name='reset'),

    re_path(r'^$', assets.profile),

    re_path(r'^(?P<asset_type>bots)/add/$', bots.add_bot),
    re_path(r'^(?P<asset_type>bots)/(?P<asset_id>\d+)/$', bots.bot),
    re_path(r'^bot-cloud/', bots.bot_cloud),

    re_path(r'^(?P<asset_type>projects)/add(/(?P<name>.*))?/$', assets.add_stats_asset, {
        'prefix': 'project/',
        'template': 'accounts/project_add.html',
    }),
    re_path(r'^(?P<asset_type>authors)/add(/(?P<name>.*))?/$', assets.add_stats_asset, {
        'prefix': 'author/',
        'template': 'accounts/author_add.html',
    }),

    re_path(r'^(?P<asset_type>projects)/(?P<asset_id>\d+)/$', assets.project),
    re_path(r'^(?P<asset_type>authors)/(?P<asset_id>\d+)/$', assets.stats_asset),

    re_path(r'^changes/$', assets.generic_page, {'template': 'accounts/change_history.html'}),
    re_path(r'^changes/(?P<asset_type>[a-z]+)/(?P<asset_id>\d+)/page(?P<page_number>\d+)/$', assets.changes),
    re_path(r'^changes/my/page(?P<page_number>\d+)/$', assets.changes),
    re_path(r'^changes/all/page(?P<page_number>\d+)/$', assets.changes, {'current_user': False}),

    re_path(r'^conflict/(?P<asset_type>[a-z]+)/(?P<asset_id>\d+)/$', assets.conflict),
]
