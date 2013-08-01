#!/bin/sh

export CIADIR=$HOME/cia
export PYTHONPATH=$HOME
export DJANGO_SETTINGS_MODULE=cia.settings

pid=`cat $CIADIR/data/queue/commit/queue.pid` 2>/dev/null

ps -p $pid >/dev/null 2>/dev/null ||
  nohup $CIADIR/mail/incoming-q.py >/dev/null 2>&1 </dev/null &

pid=`cat $CIADIR/data/queue/ping/queue.pid` 2>/dev/null

ps -p $pid >/dev/null 2>/dev/null ||
  nice nohup $CIADIR/mail/polld.py >/dev/null 2>&1 </dev/null &
