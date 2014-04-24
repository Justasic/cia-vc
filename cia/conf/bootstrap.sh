#
# Bot daemon
#
cd ~/cia
rm *.log
rm bots.socket
/usr/bin/python -OO /usr/bin/twistd -oy conf/bot_server.tac -l /var/log/cia/bot_server.log --pidfile=bot_server.pid

#
# RPC daemon
#
export LOGDIR=/var/log/cia
export REQUEST_HOST=cia.stacksmash.net
export REQUEST_PORT=80

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

# Start django
/usr/bin/python ~/cia/manage.py runfcgi socket=/var/run/cia/django.sock pidfile=/var/run/cia/django.pid

# The first web server is primarily for spiders: make it lower-priority
snice +10 `cat ~/cia/server-3930.pid`
