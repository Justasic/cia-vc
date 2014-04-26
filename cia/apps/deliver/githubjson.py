__author__ = 'justasic'

from django import http
from django.utils import simplejson, html
from django.conf import settings

import xmlrpclib

SERVER_URL = settings.CIA_RPC_URL

PROJECT_TRANS = {
    'cia-vc': 'CIA.vc'
}


def accept(request):
    return http.HttpResponse('Message accepted in queue.')