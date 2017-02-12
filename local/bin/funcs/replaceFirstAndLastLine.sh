#!/bin/zsh -f
for i in *
do
  if test -f "$i"
then
    echo "$i" && sed -i '' -e '1i\
/* eslint-disable */'  "$i"

    # sed -i '' -e '$ a\
    # })()' $i
  fi
done
