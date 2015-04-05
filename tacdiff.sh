#!/bin/bash

CURRENT_OUTPUT="/tmp/eecs483.out"

make

echo "Solution | Ours"

./dcc -d tac < samples/${1}.decaf > ${CURRENT_OUTPUT}
./solution/dcc -d tac < samples/${1}.decaf | \
        diff -aw - ${CURRENT_OUTPUT}
