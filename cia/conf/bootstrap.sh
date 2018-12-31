#
# Bot daemon
#
# Delete files and clean up before we try and start again
cd cia/

export LOGDIR=/var/log/cia
export REQUEST_HOST=cia.stacksmash.net
export REQUEST_PORT=80

#rm $LOGDIR/*.log
rm -f /var/run/cia/bots.socket

for port in 3920 3930 3931 3932 3933; do
    kill `cat server-$port.pid`
done
kill `cat bot_server.pid`

# Start the bot daemon
/usr/bin/python -OO /usr/bin/twistd -oy conf/bot_server.tac -l /var/log/cia/bot_server.log --pidfile=bot_server.pid

#
# RPC daemon
#
export PORT=3920
pidfile=server-$PORT.pid
/usr/bin/python -OO /usr/bin/twistd -oy conf/official.rpc.tac \
    -l $LOGDIR/server-$PORT.log --pidfile=$pidfile

#
# Web daemons
#
for port in 3930 3931 3932 3933; do
    export PORT=$port
    pidfile=server-$PORT.pid
    /usr/bin/python -OO /usr/bin/twistd -oy conf/official.web.tac \
        -l $LOGDIR/server-$PORT.log --pidfile=$pidfile
done

#cd ..
# Start django
#/usr/bin/python manage.py runfcgi socket=/var/run/cia/django.sock pidfile=/var/run/cia/django.pid
uwsgi --ini conf/uwsgi.ini

