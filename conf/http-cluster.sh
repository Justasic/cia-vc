export LOGDIR=/var/log/cia

#pound -f conf/official.pound.conf

export REQUEST_HOST=cia.vc
export REQUEST_PORT=80

export PORT=3920
/usr/bin/python -OO /usr/bin/twistd -oy conf/official.rpc.tac \
    -l $LOGDIR/server-$PORT.log --pidfile=server-$PORT.pid

for port in 3930 3931 3932 3933 3934; do
    export PORT=$port
    /usr/bin/python -OO /usr/bin/twistd -oy conf/official.web.tac \
        -l $LOGDIR/server-$PORT.log --pidfile=server-$PORT.pid
done
