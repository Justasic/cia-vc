#!/usr/bin/env python
""" A very simple backup script for CIA """

import datetime, os, sys

#
# A little code to read CIA's database settings file.
# It's a list of key-value pairs, one per line, with
# key and value separated by '='.
#
CIA_DB_SETTINGS = {}
for line in open(os.path.expanduser('~/.cia_db')):
    line = line.strip()
    try:
        key, value = line.split('=', 1)
        CIA_DB_SETTINGS[key.strip()] = value.strip()
    except ValueError:
        pass

# XXX: This may be a hacky script but it works.
# Dump the database.
db = CIA_DB_SETTINGS.get('db', 'cia')
user = CIA_DB_SETTINGS.get('user', 'root')
passwd = CIA_DB_SETTINGS.get('passwd', '')
timestamp = datetime.datetime.now().strftime("%Y.%m.%d")
os.system("mysqldump -u %s -p%s %s > cia.backup.%s.sql" % (user, passwd, db, timestamp))

# make the directory if it doesn't exist.
os.system("mkdir -p ~/cia/backups/")

# make a tar of the data/ directory and the sql backup
os.system("tar -cJf cia.backup.%s.tar.xz ~/cia/data/ cia.backup.%s.sql" % (timestamp, timestamp))

# remove the SQL file and move the tarball.
os.unlink("cia.backup.%s.sql" % timestamp)
os.system("mv cia.backup.%s.tar.xz ~/cia/backups/" % timestamp)
