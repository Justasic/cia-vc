#!/bin/sh

cd /home/cia/cia/data/queue/$1

# Write mail to temp file
cat >tmpmail.$$


# Lock is a simple file, protected by O_EXCL aka sh -C
#set -C
#while ! echo $$ >dump.lock ;do
#  sleep 1
#done 2>/dev/null
#set +C

lockfile -l 60 dump.lock

filenum=`cat dump.next`
expr "$filenum" + 1 >dump.next

# Release lock
rm -f dump.lock

# "atomically" create file, so queue master doesn't pick it up half-written.
mv tmpmail.$$ "mail.$filenum"

# And notify queue master
kill -USR1 `cat queue.pid`

exit 0
