#!/usr/bin/env sh

CUR_WINID=$(yabai -m query --windows --window | jq '.id')

yabai -m window --space $(yabai -m query --spaces | jq "map(select(.display == ${1})) | .[${2} - 1] | .id")
yabai -m window --focus "${CUR_WINID}"
