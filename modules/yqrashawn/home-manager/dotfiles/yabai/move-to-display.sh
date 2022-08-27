#!/usr/bin/env sh

CUR_WINID=$(yabai -m query --windows --window | jq '.id')

yabai -m window --display "${@}"
yabai -m window --focus "${CUR_WINID}"
