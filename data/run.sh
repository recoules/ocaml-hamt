#!/bin/bash

p=$(pwd)/seq/
rep=(0 0 1000 1000 100 10 2)
echo "seed;operation;size;struct;n;time;minor words; major words; promoted words;compaction;major gc;minor gc"
for i in {1..1000}
do
    for j in 2 3 4 5 6
    do
       ../benchmark/run $j $p $i ${rep[j]}
    done
done
