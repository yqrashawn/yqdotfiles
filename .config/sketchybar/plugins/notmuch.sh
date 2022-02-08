#!/usr/bin/env bash

cd ~/.mail/account.gmail && gmi sync && cd ~/.mail/account.yqrashawn && gmi sync && notmuch new
COUNT=$(notmuch count tag:inbox)
sketchybar -m --set $NAME label="$COUNT"
