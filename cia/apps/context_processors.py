from django.conf import settings
from django.contrib.sites.models import Site


def site(request):
    return {
        'site': Site.objects.get_current(),
        }

