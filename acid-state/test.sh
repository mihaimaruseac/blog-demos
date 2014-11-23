#!/bin/bash

./dist/build/test/test c

for i in `seq 100000`; do
    for j in `seq 1 10`; do
        ./dist/build/test/test i $j
    done
    time ./dist/build/test/test s
done
