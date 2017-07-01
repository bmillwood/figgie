#!/bin/bash
set -eux

BUILD=_build/system

SERVER=$BUILD/server/main.exe
BOT=$BUILD/bot/main.exe

"$SERVER" -length-of-round 1m -enable-chat true -log-level Debug \
  -room "Room 101" &
victims=$!
trap 'kill $victims' EXIT
sleep 1

function bot() {
  ty=$1
  shift
  "$BOT" "$ty" -server localhost:58828 -log-level Debug "$@" \
    2>&1 | sed -re "s/^/$ty /" &
  victims="$victims $!"
  sleep 3
}

bot count
bot chaos
bot sell
wait
