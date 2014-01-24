#!/bin/bash

if test $# -ne 7; then
    echo "./test.sh distance manspeed raptorspeed injuredspeed angle distmin timeincrement"
    exit
fi

./raptors $1 $2 $3 $4 $5 $6 $7
echo 'set term png; plot "trace" using 1:2 title "Man" w lines, "trace" using 3:4 title "Injured" w lines, "trace" using 5:6 title "Raptor1" w lines, "trace" using 7:8 title "Raptor2" w lines' | gnuplot > trace.png

wc -l trace | cut -d' ' -f1

