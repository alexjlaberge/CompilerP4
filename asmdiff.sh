#!/bin/bash

CURRENT_OUTPUT="/tmp/eecs483.out"

make

if [ ! -z ${1} ]
then
	echo "Solution | Ours"

	./dcc < samples/${1}.decaf > ${CURRENT_OUTPUT}
	./solution/dcc < samples/${1}.decaf | diff -aw - ${CURRENT_OUTPUT}
	exit $?
fi

for f in `ls samples/*.decaf`
do
	echo -n "Testing ${f}..."
	./dcc < ${f} 2> /dev/null > ${CURRENT_OUTPUT}
	./solution/dcc < ${f} 2> /dev/null | diff -awq - ${CURRENT_OUTPUT} && echo "PASS"
done
