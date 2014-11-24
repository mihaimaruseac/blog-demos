#!/bin/bash

cat << END | gnuplot
set term png
set output 'time.png'
f(x) = a*x + b
g(x) = x / 55 * 10
fit f(x) "output" u (g(\$1)):2 via a,b
title_f(a,b) = sprintf('%.2g * size + %.2g', a, b)
plot "output" u (g(\$1)):2 w p ps 0.1 pt 4 title "pts",\
     "output" u (g(\$1)):(f(g(\$1))) w l ls 2 title title_f(a,b)
END
