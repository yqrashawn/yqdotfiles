#!/usr/bin/env bash

cd $HOME/.mail/account.gmail && $HOME/.pyenv/shims/gmi sync && cd $HOME/.mail/account.yqrashawn && $HOME/.pyenv/shims/gmi sync
notmuch new
COUNT=$(notmuch count tag:inbox)
sketchybar -m --set $NAME label="$COUNT"
