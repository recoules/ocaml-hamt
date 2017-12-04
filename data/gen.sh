#!/bin/bash

p=$(pwd)/seq/
for i in {1..1000}
do
    for j in 2 3 4 5 6
    do
        ../benchmark/gen $j $p $i
    done
done
