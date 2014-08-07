#!/bin/bash

f=`mktemp`
python prob.py > $f

cat << END | gnuplot > $1
set term png;
plot "$f" w l
END

rm $f
