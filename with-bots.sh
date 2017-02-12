#!/bin/bash
./server.byte -length-of-round 1m &
victims=$!
sleep 1
for which in 1 2 3 4 5 6
do
  ./bot.byte sell -server localhost:58828 \
    -log-level Debug \
    -size 1 -fade 1 -at 6 \
    -which $which 2>&1 | sed -re "s/^/sell$which /" &
  victims="$victims $!"
  sleep 1
done
./bot.byte count -server localhost:58828 \
  -log-level Debug \
  2>&1 | sed -re "s/^/count /" &
victims="$victims $!"
trap "kill $victims" EXIT
wait
