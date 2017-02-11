#!/bin/bash
./server.byte &
victims=$!
sleep 0.5
for which in 1 2
do
  ./bot.byte sell -server localhost:58828 \
    -size 1 -fade 1 -at 6 \
    -which $which &
  victims="$victims $!"
done
./bot.byte count -server localhost:58828 &
victims="$victims $!"
trap "kill $victims" EXIT
wait
