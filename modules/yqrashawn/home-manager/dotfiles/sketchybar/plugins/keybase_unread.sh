#!/usr/bin/env bash

keybase chat list-unread | grep '* yholybasil'

if [[ $? -ne 0 ]]; then
    sketchybar -m --set $NAME label=""
else
    sketchybar -m --set $NAME label="KB"
fi