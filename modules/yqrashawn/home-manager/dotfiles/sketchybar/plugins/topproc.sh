#!/usr/bin/env bash

TOPPROC=$(ps axo "%cpu,ucomm" | sort -nr | head -n1 | awk '{printf "%.0f%% %s\n", $1, $2}' | sed -e 's/com.apple.//g')
CPUP=$(echo $TOPPROC | sed -nr 's/([^\%]+).*/\1/p')

if [ $CPUP -gt 70 ]; then
  sketchybar -m --set $NAME label=" $TOPPROC"
else
  sketchybar -m --set $NAME label=""
fi
