#!/bin/bash

INFILES=`ls samples/*.decaf | sed -e 's/\.decaf//'`

function spim_run() {
        if [ -f ${1}.in ]
        then
                solution/spim -file ${1}.s < ${1}.in > ${1}.out
        else
                solution/spim -file ${1}.s > ${1}.out
        fi
}

make

for input in ${INFILES}
do
        echo -ne "Testing ${input}..."

        # First, test ours and get the output in memory
        #./dcc < ${input}.decaf > ${input}.s
        #spim_run ${input}
        #CURRENT_OUTPUT=`cat ${input}.out`
        CURRENT_OUTPUT=""

        ./solution/dcc < ${input}.decaf > ${input}.s
        spim_run ${input}

        echo ${CURRENT_OUTPUT} | diff -aq ${input}.out - && echo "PASS"
        if [ ! "$?" -eq 0 ]
        then
                FAILED="${FAILED} ${input}"
        fi

        if [ "samples/${1}" == ${input} ]
        then
                echo ${CURRENT_OUTPUT} | diff -a ${input}.out -
                exit 1
        fi
done

echo
echo "Failures:"

for failure in ${FAILED}
do
        echo "    ${failure}"
done
