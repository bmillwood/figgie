#!/bin/bash
./figgie.byte server &
victims=$!
sleep 0.1
for which in 1 2
do
  ./figgie.byte bot sell -server localhost:58828 \
    -size 1 -fade 1 -at 6 \
    -which $which &
  victims="$victims $!"
done
./figgie.byte bot count -server localhost:58828 &
victims="$victims $!"
trap "kill $victims" EXIT
wait
