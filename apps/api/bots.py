from cia.apps.api.util import json_result
from cia.apps.legacy import bots


###########################
#    Bots and Requests    #
###########################

@json_result
def request_status(request, host, channel, port=None):
    if port:
        target = "%s:%s/%s" % (host, port, channel)
    else:
        target = "%s/%s" % (host, channel)
    return {
        'request': bots.report(target)
        }


###########################
#   Server-wide Totals    #
###########################

@json_result
def totals(request):
    return {
        'totals': bots.totals()
        }   


###########################
#      Message Log        # 
###########################

@json_result
def message_log(request):
    return {
        'log': bots.msglog()
        }
