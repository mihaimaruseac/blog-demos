#!/bin/bash

#> angle
#
#for i in {0..360}; do
#    echo -n .
#    if test $i -eq 60; then
#        echo
#    elif test $i -eq 120; then
#        echo
#    elif test $i -eq 180; then
#        echo
#    elif test $i -eq 240; then
#        echo
#    elif test $i -eq 300; then
#        echo
#    fi
#    echo "$i `./test.sh 200 8 15 1 $i 0.1 0.001`" >> angle
#done

echo 'set term png; set grid polar; set polar; set xrange[-14000:14000]; set yrange [-14000:14000]; set xtics 1000; set ytics 1000; set angles degrees; plot "angle" w lines' | gnuplot > angle.png


