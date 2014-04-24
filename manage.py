#!/usr/bin/env python
import os
import sys

if __name__ == "__main__":
    # because we're not as updated as django 1.5 on how our project should be structured
    # We must add whatever directory we're in + up a dir to make manage.py be recognized
    # inside the project directory
    sys.path.append(os.path.expanduser(".."))
    os.environ.setdefault("DJANGO_SETTINGS_MODULE", "cia.settings")

    from django.core.management import execute_from_command_line

    execute_from_command_line(sys.argv)
