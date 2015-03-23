#!/bin/bash

INFILES=`ls samples/*.decaf | sed -e 's/\.decaf//'`
CURRENT_OUTPUT="/tmp/eecs483p4.out"

function spim_run() {
        if [ -f ${1}.in ]
        then
                solution/spim -file ${1}.s < ${1}.in > ${1}.out 2> /dev/null
        else
                solution/spim -file ${1}.s > ${1}.out 2> /dev/null
        fi
}

make

for input in ${INFILES}
do
        echo -ne "Testing ${input}..."

        # First, test ours and get the output in memory
        ./dcc < ${input}.decaf > ${input}.s 2> /dev/null
        spim_run ${input}
        cp ${input}.out ${CURRENT_OUTPUT}

        ./solution/dcc < ${input}.decaf > ${input}.s 2> /dev/null
        spim_run ${input}

        diff -aq ${input}.out ${CURRENT_OUTPUT} && echo "PASS"
        if [ ! "$?" -eq 0 ]
        then
                FAILED="${FAILED} ${input}"
        fi

        if [ "samples/${1}" == ${input} ]
        then
                diff -a ${input}.out ${CURRENT_OUTPUT}
                exit 1
        fi
done

echo
echo "Failures:"

for failure in ${FAILED}
do
        echo "    ${failure}"
done
