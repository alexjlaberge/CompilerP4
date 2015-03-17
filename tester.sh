#!/bin/bash
# Proper header for a Bash script.

make

if [ ! -z ${1} ]
then
        input="samples/${1}.decaf"
        ./dcc < ${input} 2>&1 | diff -a \
                `echo ${input} | sed -e "s/\..*/\.out/"` -
        exit 0
fi

FAILED=""

for input in `ls samples/*.decaf`
do
        echo -ne "Testing ${input}..."
        ./solution/dcc < ${input} 2>&1 &> `echo ${input} | sed -e "s/\..*/\.out/"`
        ./dcc < ${input} 2>&1 | diff -aq \
                `echo ${input} | sed -e "s/\..*/\.out/"` - && echo "PASS"
        if [ ! "$?" -eq 0 ]
        then
                FAILED="${FAILED} ${input}"
        fi
done

echo
echo "Failures:"

for failure in ${FAILED}
do
        echo "    ${failure}"
done
