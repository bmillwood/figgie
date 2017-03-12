#!/bin/bash -eux
jbuilder build {bot,server}/main.exe web/main.js
for thing in bot server
do
  [ -e "$thing.exe" ] \
    || ln -sf "_build/system/$thing/main.exe" "$thing.exe"
done
[ -e web.js ] || ln -s _build/system/web/main.js web.js
