#!/usr/bin/env zsh

cd $HOME/.mail/account.gmail &&
  gmi sync &&
  cd $HOME/.mail/account.yqrashawn &&
  gmi sync &&
  notmuch new &&
  sketchybar -m --set $NAME label=$(notmuch count tag:inbox and tag:unread)
