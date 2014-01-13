#!/bin/bash

./ships $1 $2 $3 $4
echo 'set term png; plot "trace" using 1:2 title "First" with lines, "trace" using 3:4 title "Second" with lines' | gnuplot > trace.png
echo 'set term png; plot "dist" title "Distance" with lines' | gnuplot > dist.png

